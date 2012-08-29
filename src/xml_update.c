/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_update.h"
#include "xpath.h"

static void adjustIgnoreList(XMLScan scan, XMLNodeOffset minimum, int shift);
static void propagateChange(XMLScanOneLevel levelScan, int *shift, int *hdrSizeIncr, char *tree, char *resData,
			  char **srcCursor, char **resCursor, XMLNodeOffset *newRootOff);
static void copyXMLDecl(XMLCompNodeHdr doc, char **resCursor);
static void copySiblings(XMLCompNodeHdr parent, char **srcCursor, char **resCursor);

PG_FUNCTION_INFO_V1(xmlnode_add);

Datum
xmlnode_add(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);

	xpath		xpathPtr = (xpath) PG_GETARG_POINTER(1);
	XPathStorageHeader *storageHdr = (XPathStorageHeader *) VARDATA(xpathPtr);
	XPathExpression exprBase;
	XPathHeader xpHdr;
	XPath xpath;

	xmlnode		newNdVar = (xmlnode) PG_GETARG_VARLENA_P(2);
	BpChar	   *modeVar = PG_GETARG_BPCHAR_PP(3);
	char		mode = *(VARDATA_ANY(modeVar));
	XMLNodeHdr	newNode;
	xmldoc		result;
	XMLScanData xscan;
	XMLCompNodeHdr docRoot;

	if (storageHdr->paramCount > 0)
	{
		elog(ERROR, "this function does not accept parameterized xpath expression");
	}

	exprBase = (XPathExpression) ((char *) storageHdr + sizeof(XPathStorageHeader));
	xpHdr = (XPathHeader) ((char *) exprBase + exprBase->common.size);
	xpath = getSingleXPath(exprBase, xpHdr);

	if (xpath->relative)
	{
		elog(ERROR, "absolute XPath expected");
	}
	if (xpath->depth == 0)
	{
		elog(ERROR, "invalid target path");
	}
	if (xpath->targNdKind == XMLNODE_ATTRIBUTE)
	{
		elog(ERROR, "target path for node addition must not point to attribute");
	}

	newNode = XNODE_ROOT(newNdVar);
	if (newNode->kind == XMLNODE_ATTRIBUTE)
	{
		elog(ERROR, "invalid node type to add: %s", getXMLNodeKindStr(newNode->kind));
	}
	docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);
	initXMLScan(&xscan, NULL, xpath, xpHdr, docRoot, doc, xpath->descendants > 0);
	result = updateXMLDocument(&xscan, doc, XMLNODE_ACTION_ADD, newNode, XMLADD_MODE(mode));
	finalizeXMLScan(&xscan);
	PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(xmlnode_remove);

extern Datum
xmlnode_remove(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);

	xpath		xpathPtr = (xpath) PG_GETARG_POINTER(1);
	XPathStorageHeader *storageHdr = (XPathStorageHeader *) VARDATA(xpathPtr);
	XPathExpression exprBase;
	XPathHeader xpHdr;
	XPath xpath;

	xmldoc		result;
	XMLScanData xscan;
	XMLCompNodeHdr docRoot;

	if (storageHdr->paramCount > 0)
	{
		elog(ERROR, "this function does not accept parameterized xpath expression");
	}

	exprBase = (XPathExpression) ((char *) storageHdr + sizeof(XPathStorageHeader));
	xpHdr = (XPathHeader) ((char *) exprBase + exprBase->common.size);
	xpath = getSingleXPath(exprBase, xpHdr);

	if (xpath->relative)
	{
		elog(ERROR, "absolute XPath expected");
	}
	if (xpath->depth == 0)
	{
		elog(ERROR, "invalid target path");
	}
	docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);
	initXMLScan(&xscan, NULL, xpath, xpHdr, docRoot, doc, false);
	result = updateXMLDocument(&xscan, doc, XMLNODE_ACTION_REMOVE, NULL, XMLADD_INVALID);

	finalizeXMLScan(&xscan);
	PG_RETURN_POINTER(result);
}

xmldoc
updateXMLDocument(XMLScan xscan, xmldoc doc, XMLNodeAction action, XMLNodeHdr newNode,
				  XMLAddMode addMode)
{

	XMLNodeHdr	targNode = getNextXMLNode(xscan, false);
	xmldoc		result;

	if (targNode != NULL)
	{
		bool		freeSrc = false;

		result = doc;

		while (targNode != NULL)
		{
			XMLScan		scTmp = xscan;
			XNodeListItem ignore;

			if (action == XMLNODE_ACTION_ADD)
			{
				unsigned int unresolvedNmspCount;
				char	  **unresolvedNamespaces = getUnresolvedXMLNamespaces(newNode, &unresolvedNmspCount);

				if (unresolvedNmspCount > 0)
				{
					XMLScanOneLevel scanLevel;
					XMLNodeContainerData declarations;
					unsigned int i,
								j;

					/*
					 * Zero-depth target path is not allowed, so the state
					 * should always exist.
					 */
					Assert(xscan->state != NULL);

					xmlnodeContainerInit(&declarations);

					/*
					 * Collect namespace declarations throughout the stack up
					 * to the document root.
					 */
					scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);
					do
					{
						XMLCompNodeHdr parentNode = scanLevel->parent;

						if (parentNode->common.kind == XMLNODE_ELEMENT)
						{		/* Otherwise it must be XMLNODE_DOC. */
							collectXMLNamespaceDeclarations(parentNode, NULL, NULL, &declarations, true,
															NULL, NULL);
						}
						scanLevel = scanLevel->up;
					} while (scanLevel != NULL);

					/*
					 * If we're adding node *into* the target element, then
					 * it's namespace declarations are important too. (If
					 * trying to insert into non-element, no need to check:
					 * error will be raised later.)
					 */
					if (addMode == XMLADD_INTO && targNode->kind == XMLNODE_ELEMENT)
					{
						collectXMLNamespaceDeclarations((XMLCompNodeHdr) targNode, NULL, NULL, &declarations, true,
														NULL, NULL);
					}

					/* Examine each unresolved namespace. */
					for (i = 0; i < unresolvedNmspCount; i++)
					{
						char	   *namespace = unresolvedNamespaces[i];
						XNodeListItem *declItem = declarations.content;
						bool		found = false;

						for (j = 0; j < declarations.position; j++)
						{
							XMLNodeHdr	declNode;
							char	   *decl;

							Assert(declItem->kind == XNODE_LIST_ITEM_SINGLE_PTR);

							declNode = (XMLNodeHdr) declItem->value.singlePtr;
							decl = (char *) XNODE_CONTENT(declNode) + strlen(XNODE_NAMESPACE_DEF_PREFIX) + 1;
							if (strcmp(namespace, decl) == 0)
							{
								found = true;
								break;
							}
							declItem++;
						}

						if (!found)
						{
							elog(ERROR, "can't add node to a document, the resulting document would use unbound namespace '%s'",
								 namespace);
						}
					}
					xmlnodeContainerFree(&declarations);
				}

				result = xmlnodeAdd(result, xscan, targNode, newNode, addMode, freeSrc, &ignore);

				if (xscan->ignoreList)
				{
					xmlnodeAddListItem(xscan->ignoreList, &ignore);
				}

			}
			else if (action == XMLNODE_ACTION_REMOVE)
			{
				result = xmlnodeRemove(result, xscan, targNode, freeSrc);
			}
			else
			{
				elog(ERROR, "unknown action");
			}

			/*
			 * If anything has been added at level > 0, then it had to be
			 * inside root element and thus the well-formedness can't be
			 * compromised.
			 */
			if (xscan->depth == 0)
			{
				XMLNodeOffset *rootOff = (XMLNodeOffset *) ((char *) result + VARSIZE(result) -
													  sizeof(XMLNodeOffset));
				XMLCompNodeHdr root = (XMLCompNodeHdr) ((char *) VARDATA(result) + *rootOff);

				checkXMLWellFormedness(root);
			}

			/*
			 * Make sure the scan and its subscans use the modified document.
			 */
			while (scTmp != NULL)
			{
				scTmp->document = result;
				scTmp = scTmp->subScan;
			}

			targNode = getNextXMLNode(xscan, action == XMLNODE_ACTION_REMOVE);

			if (!freeSrc)
			{
				freeSrc = true;
			}
		}
	}
	else
	{
		XMLNodeOffset *docRootOff = (XMLNodeOffset *) ((char *) doc + VARSIZE(doc) -
													   sizeof(XMLNodeOffset));
		char	   *inputTree = (char *) VARDATA(doc);
		XMLNodeHdr	srcNode = (XMLNodeHdr) (inputTree + *docRootOff);

		result = (xmldoc) copyXMLNode(srcNode, NULL, true, NULL);
	}
	return result;
}

/*
 * Adds 'newNode' before/after/into 'targNode', which is contained in 'doc'.
 * 'xscan' is modified so that it points to 'targetNode' in the new
 * (returned) document. 'targNode' must not be NULL
 *
 * 'ignore' is an output parameter. It represents a new node or node range to be added to ignore list,
 * in order to prevent infinite recursions.
 *
 * Well-formedness of the resulting documents needs to be checked.
 */
xmldoc
xmlnodeAdd(xmldoc doc, XMLScan xscan, XMLNodeHdr targNode, XMLNodeHdr newNode,
		   XMLAddMode mode, bool freeSrc, XNodeListItem *ignore)
{

	char	   *inputTree = (char *) VARDATA(doc);
	XMLNodeOffset *docRootOff = NULL;
	XMLCompNodeHdr docNodeSrc = (XMLCompNodeHdr) XNODE_ROOT(doc);
	XMLScanOneLevel levelScan;
	unsigned short int i;
	unsigned int extraSpace = 0;
	unsigned int newNdSize,
				targNdSize,
				resultSizeMax,
				resultSize;
	char	   *result = NULL;
	char	   *resData = NULL;
	XMLNodeOffset targNdOff;
	XMLNodeOffset srcIncr;
	char	   *srcCursor = inputTree;
	char	   *resCursor = NULL;
	XMLCompNodeHdr levelNode,
				parentSrc,
				parentTarg;
	unsigned char bwidthSrc,
				bwidthTarg;
	XMLScan		xscTmp;

	/*
	 * Where root of the new subtree is located in the 'resultData'.
	 */
	char	   *newNdRoot = NULL;
	char	  **newNdRoots = NULL;
	int			hdrSizeIncr = 0;
	int			shift;
	XMLNodeOffset refSrc,
				refTarg,
				newRootOff;
	unsigned short lastInd = 0;

	/*
	 * Index of the new index, 0-based. If XMLADD_INTO is the node, it's index
	 * of the existing node a new node has been inserted into.
	 */
	unsigned short newNdIndex = 0;
	unsigned int intoHdrSzIncr = 0;

	/*
	 * Check the parameters
	 */
	Assert(targNode != NULL);

	/*
	 * w/o the following restriction, it'd be tricky to ensure that attributes
	 * are not mixed with non-attribute nodes..
	 */
	if (mode == XMLADD_REPLACE)
	{
		if ((newNode->kind == XMLNODE_ATTRIBUTE && targNode->kind != XMLNODE_ATTRIBUTE) ||
			(newNode->kind != XMLNODE_ATTRIBUTE && targNode->kind == XMLNODE_ATTRIBUTE))
		{
			elog(ERROR, "'replace' mode can't be used to replace attribute with non-attribute node and vice versa.");
		}
	}

	/*
	 * Strictly, checkXMLWellFormedness() should recognize invalid location of
	 * the DTD node in the resulting document. However it doesn't seem to be
	 * worth letting the function search through the whole tree recursively
	 * just for the sake of this (very) special case. (The current version of
	 * checkXMLWellFormedness only checks the first level of the document
	 * tree.)
	 */
	if (newNode->kind == XMLNODE_DTD)
	{
		elog(ERROR, "DTD node can't be added to an existing document.");
	}

	/*
	 * If document fragment is passed as the new node, it must not contain any
	 * attribute. Specific requirements have to be maintained for attributes
	 * (e.g. uniqueness or position in the tree). That's a reason to use a
	 * separate function to set attributes.
	 */
	if (newNode->kind == XMLNODE_DOC_FRAGMENT)
	{
		Assert(((XMLCompNodeHdr) newNode)->children > 0);
		if (checkFragmentForAttributes((XMLCompNodeHdr) newNode))
		{
			elog(ERROR, "document fragment can't be added as long as it contains attribute(s)");
		}
	}

	/*
	 * Estimate how much the storage will grow. First, find out if some
	 * references will grow in 'byte width'
	 */
	targNdOff = (char *) targNode - inputTree;

	xscTmp = xscan;
	do
	{
		levelScan = xscTmp->state;

		for (i = 0; i <= xscTmp->depth; i++)
		{
			levelNode = levelScan->parent;

			/*
			 * Let's expect the worst - each reference size to grow to the
			 * maximum: sizeof(XMLNodeOffset)
			 */
			extraSpace += levelNode->children * (sizeof(XMLNodeOffset) - XNODE_GET_REF_BWIDTH(levelNode));
			levelScan++;
		}
		xscTmp = xscTmp->subScan;
	} while (xscTmp != NULL);

	levelScan--;

	/*
	 * .. plus the new node(s) and the corresponding reference(s)
	 */
	newNdSize = getXMLNodeSize(newNode, true);
	if (newNode->kind == XMLNODE_DOC_FRAGMENT)
	{
		/*
		 * Ignore the fragment node itself - only its children will be added.
		 */
		newNdSize -= getXMLNodeSize(newNode, false);
		extraSpace += newNdSize + ((XMLCompNodeHdr) newNode)->children * sizeof(XMLNodeOffset);
	}
	else
	{
		extraSpace += newNdSize + sizeof(XMLNodeOffset);
	}

	resultSizeMax = VARSIZE(doc) + extraSpace;
	targNdSize = getXMLNodeSize(targNode, true);
	if (mode == XMLADD_REPLACE)
	{
		resultSizeMax -= targNdSize;
	}

	/*
	 * Make sure that the new node / subtree is ignored by further scan. For
	 * simplicity we treat the new node as a range, even if it might only be a
	 * single node. If a single node is added, we just set the upper limit to
	 * the last byte that still belongs to that node.
	 */
	ignore->kind = XNODE_LIST_ITEM_RANGE;
	ignore->valid = true;

	if (mode == XMLADD_BEFORE || mode == XMLADD_REPLACE)
	{
		result = (char *) palloc(resultSizeMax);
		resCursor = resData = VARDATA(result);

		/*
		 * Copy input tree up to the target node's first descendant (first in
		 * terms of absolute offset in the array)
		 */
		if (targNode->kind == XMLNODE_ELEMENT)
		{
			srcIncr = (char *) getFirstXMLNodeLeaf((XMLCompNodeHdr) targNode) - inputTree;
		}
		else
		{
			srcIncr = targNdOff;
		}
		memcpy(resCursor, inputTree, srcIncr);
		srcCursor += srcIncr;
		resCursor += srcIncr;

		ignore->value.range.lower = resCursor - resData;

		/*
		 * Copy the new node(s). Once it's done, 'resCursor' points to a
		 * position right after the new node.
		 */
		copyXMLNodeOrDocFragment(newNode, newNdSize, &resCursor, &newNdRoot, &newNdRoots);

		/*
		 * It's not necessary to compute exact offset of the highest node of a
		 * subtree. All we need is to ensure that the node following the new
		 * node/subtree is not included in the 'range to be ignored'.
		 */
		ignore->value.range.upper = resCursor - 1 - resData;

		if (mode == XMLADD_REPLACE && xscan->ignoreList)
		{
			/*
			 * Remove the original target node (added during a scan) from the
			 * ignore list.
			 *
			 * It could cause problems when a subtree has been replaced by any
			 * kind of smaller node or subtree. In such a case the
			 * corresponding ignore list item could start pointing to a valid
			 * node behind the new node and continuing scan would skip it.
			 */
			XNodeListItem *item = xscan->ignoreList->content;
			unsigned int listSize = xscan->ignoreList->position;
			unsigned short i;

			for (i = 0; i < listSize; i++)
			{
				if (item->valid)
				{
					/*
					 * As mentioned above, only those 'ignore list items'
					 * should be removed now that scan (getNextXMLNode) has
					 * added. Therefore we don't care about ranges: those can
					 * only be added to the list by this function. If that
					 * happens, such a range remains valid (it represents tne
					 * new, possibly smaller subtree, as opposed to the
					 * original one).
					 */
					if (item->kind == XNODE_LIST_ITEM_SINGLE_OFF && item->value.singleOff == targNdOff)
					{
						item->valid = false;
					}
				}
				item++;
			}
		}

		srcIncr = targNdSize;

		if (mode == XMLADD_BEFORE)
		{
			/*
			 * Copy the whole target node (including subtree)
			 */
			copyXMLNode(targNode, resCursor, false, NULL);
			resCursor += targNdSize;
			shift = newNdSize;

			/*
			 * 'srcCursor' is now at the target node. As a new node is being
			 * added BEFORE, all references to the target node must be
			 * included in the adjustment (shift).
			 *
			 * 'srcCursor' may be pointing to 'last leaf' as opposed to header
			 * of the target node. That makes no difference, as the references
			 * to target node never point before 'srcCursor'.
			 */
			adjustIgnoreList(xscan, (XMLNodeOffset) (srcCursor - inputTree), shift);
		}
		else
		{
			shift = newNdSize - targNdSize;

			/*
			 * 'srcIncr' is now at the beginning of the new node. Only
			 * references pointing behind 'srcIncr + targNdSize' need to be
			 * adjusted.
			 *
			 * Those pointing to the removed node can be ignored because no
			 * scan will find them later anyway.
			 */
			adjustIgnoreList(xscan, (XMLNodeOffset) (srcCursor - inputTree + targNdSize), shift);
		}

		/* Move behind the target node */
		srcCursor += srcIncr;
	}
	else if (mode == XMLADD_AFTER)
	{
		result = (char *) palloc(resultSizeMax);
		resCursor = resData = VARDATA(result);

		/*
		 * Copy input tree up to the target node header
		 */
		srcIncr = targNdOff;
		memcpy(resCursor, inputTree, srcIncr);
		srcCursor += srcIncr;
		resCursor += srcIncr;

		/*
		 * Copy the target node header. If the node has descendants, these are
		 * at lower offsets and were copied at the initial stage.
		 */
		memcpy(resCursor, srcCursor, srcIncr = getXMLNodeSize(targNode, false));

		/*
		 * 'srcCursor' is still at the target node (header). As the new node
		 * is added AFTER, the target node's position doesn't change.
		 * Therefore '+ 1' to the minimum increased position.
		 */
		adjustIgnoreList(xscan, (XMLNodeOffset) (srcCursor - inputTree + 1), newNdSize);
		srcCursor += srcIncr;
		resCursor += srcIncr;

		shift = newNdSize;

		/* ... and copy the new node */
		ignore->value.range.lower = resCursor - resData;
		copyXMLNodeOrDocFragment(newNode, newNdSize, &resCursor, &newNdRoot, &newNdRoots);
		ignore->value.range.upper = resCursor - 1 - resData;
	}
	else if (mode == XMLADD_INTO)
	{
		XMLCompNodeHdr targElement = (XMLCompNodeHdr) targNode;
		char	   *refSrcPtr;
		char	   *refDstPtr;
		XMLCompNodeHdr targUpdated;
		unsigned int cntLen;
		char		bws,
					bwt;

		if (targNode->kind != XMLNODE_ELEMENT)
		{
			elog(ERROR, "'into' addition mode can only be used for element");
		}
		result = (char *) palloc(resultSizeMax);
		resCursor = resData = VARDATA(result);

		/*
		 * The following is similar to 'XMLADD_BEFORE', however the target
		 * node is not copied.
		 */
		srcIncr = (char *) targElement - inputTree;
		memcpy(resCursor, inputTree, srcIncr);
		srcCursor += srcIncr;
		resCursor += srcIncr;

		/* Copy the new node. */
		ignore->value.range.lower = resCursor - resData;
		copyXMLNodeOrDocFragment(newNode, newNdSize, &resCursor, &newNdRoot, &newNdRoots);
		ignore->value.range.upper = resCursor - 1 - resData;

		/*
		 * 'srcCursor' now points at the (still empty) target element, which
		 * will be moved by new node insertion. Therefore, references to the
		 * target node and all the following must be adjusted.
		 */
		shift = newNdSize;
		adjustIgnoreList(xscan, (XMLNodeOffset) (srcCursor - inputTree), shift);

		/*
		 * Copy the target node header now
		 */
		memcpy(resCursor, srcCursor, srcIncr = sizeof(XMLCompNodeHdrData));
		targUpdated = (XMLCompNodeHdr) resCursor;
		refDstPtr = XNODE_FIRST_REF(targUpdated);

		/* Copy references for the existing nested nodes. */
		if (targElement->children > 0)
		{
			unsigned short i;
			XMLNodeHdr	last;

			refSrcPtr = XNODE_FIRST_REF(targElement);
			bws = XNODE_GET_REF_BWIDTH(targElement);
			refSrc = readXMLNodeOffset(&refSrcPtr, bws, false);
			bwt = getXMLNodeOffsetByteWidth(refSrc + newNdSize);

			for (i = 0; i < targElement->children; i++)
			{
				refSrc = readXMLNodeOffset(&refSrcPtr, bws, true);
				writeXMLNodeOffset(refSrc + newNdSize, &refDstPtr, bwt, true);
			}
			last = (XMLNodeHdr) ((char *) targElement - refSrc);
			if (last->kind == XMLNODE_ATTRIBUTE)
			{
				targUpdated->common.flags &= ~XNODE_EMPTY;
			}
		}
		else
		{
			bws = 0;
			targUpdated->common.flags &= ~XNODE_EMPTY;
			bwt = getXMLNodeOffsetByteWidth(newNdSize);
		}

		/* Add reference(s) for the new node */
		if (newNode->kind == XMLNODE_DOC_FRAGMENT)
		{
			XMLCompNodeHdr frag = (XMLCompNodeHdr) newNode;
			unsigned short i;

			refSrcPtr = XNODE_FIRST_REF(frag);
			for (i = 0; i < frag->children; i++)
			{
				refTarg = (XMLNodeOffset) ((char *) targUpdated - newNdRoots[i]);
				writeXMLNodeOffset(refTarg, &refDstPtr, bwt, true);
			}
			targUpdated->children += frag->children;
		}
		else
		{
			refTarg = (XMLNodeOffset) ((char *) targUpdated - newNdRoot);
			if (targElement->children == 0)
			{
				bwt = getXMLNodeOffsetByteWidth(refTarg);
			}
			writeXMLNodeOffset(refTarg, &refDstPtr, bwt, true);
			targUpdated->children++;
		}

		intoHdrSzIncr = targUpdated->children * bwt - targElement->children * bws;

		/* copy target node name */
		srcCursor = XNODE_ELEMENT_NAME(targElement);
		resCursor = refDstPtr;
		cntLen = strlen(srcCursor);
		strcpy(resCursor, srcCursor);
		srcCursor += cntLen + 1;
		resCursor += cntLen + 1;

		if (bws != bwt)
		{
			XNODE_RESET_REF_BWIDTH(targUpdated);
			XNODE_SET_REF_BWIDTH(targUpdated, bwt);
		}

		intoHdrSzIncr = getXMLNodeSize((XMLNodeHdr) targUpdated, false) - getXMLNodeSize(targNode, false);
		if (intoHdrSzIncr > 0)
		{
			/*
			 * srcCursor is now right after the target node. References
			 * pointing originally to nodes after the target node (shifted
			 * already by 'shift') need to be additionally shifted, because
			 * addition of a new node increases the target node header.
			 */
			adjustIgnoreList(xscan, (XMLNodeOffset) (srcCursor - inputTree + shift), intoHdrSzIncr);
			shift += intoHdrSzIncr;
		}
	}
	else
	{
		elog(ERROR, "Unknown addition mode.");
	}

	parentSrc = levelScan->parent;

	/*
	 * Note that 'siblingsLeft' attribute of the scan does include the child
	 * element we're at.
	 */
	if (levelScan->siblingsLeft > 1)
	{
		/*
		 * Copy the remaining siblings of the new node if there are some.
		 */
		copySiblings(parentSrc, &srcCursor, &resCursor);
	}

	/*
	 * Copy header of the new node's parent and (adjusted) references
	 */
	newRootOff = resCursor - resData;
	memcpy(resCursor, srcCursor, srcIncr = sizeof(XMLCompNodeHdrData));
	parentTarg = (XMLCompNodeHdr) resCursor;
	srcCursor += srcIncr;
	resCursor += srcIncr;

	if (parentSrc->children > 0)
	{
		/*
		 * Copy the existing references and add the new one. 'srcCursor' is
		 * now at the first reference
		 */
		unsigned int i = 0;
		unsigned short int refSrcCount = 1;
		XMLNodeOffset refTargMax;

		bwidthSrc = XNODE_GET_REF_BWIDTH(parentSrc);
		refSrc = readXMLNodeOffset(&srcCursor, bwidthSrc, false);

		/*
		 * Determine index (order) of the new node. If document fragment is
		 * being added, this is the index of the first child of the fragment.
		 */
		switch (mode)
		{
			case XMLADD_AFTER:
				newNdIndex = parentSrc->children - levelScan->siblingsLeft + 1;
				break;

			case XMLADD_BEFORE:
			case XMLADD_REPLACE:
			case XMLADD_INTO:
				newNdIndex = parentSrc->children - levelScan->siblingsLeft;
				break;

			default:
				elog(ERROR, "unknown addition mode: %u", mode);
				break;
		}

		/*
		 * When new child is added, the maximum child offset of the new parent
		 * will always be greater than that of the 'source parent'.
		 */
		refTargMax = refSrc;

		/*
		 * 'newNdSize' may be too high value to add in case the new node is
		 * going to be the first child and at the same time it has a subtree.
		 * In this case the offset does not span the subtree, so we could
		 * compute the subtree's size and subtract it. However the risk of
		 * using too many bytes due to this corner case is not critical.
		 *
		 * Other than that it wouldn't not be clear what to subtract in case
		 * the new node is document fragment.
		 */
		refTargMax += newNdSize;

		if (mode == XMLADD_BEFORE)
		{
			/*
			 * Only size of the target node's subtree is the actual required
			 * addition to the distance between the parent node and its (new)
			 * first child.
			 *
			 * However 'targNdSize' also contains size of the target node
			 * itself, which might make estimate of the maximum offset
			 * exaggerated. To avoid this we could compute size of the target
			 * node itself and subtract it, but the extra size computation
			 * does not seem to be worthwhile (the node itself is typically
			 * small: just header and element name).
			 */
			refTargMax += targNdSize;
		}
		else
		{
			if (mode == XMLADD_REPLACE)
			{
				/*
				 * Unlike the simplified additions above, we must be tentative
				 * when subtracting. If the target node is the first child of
				 * its parent, we must not subtract size of the subtree
				 * because its not included in 'refSrc'.
				 */
				if (newNdIndex == 0)
				{
					refTargMax -= getXMLNodeSize(targNode, false);
				}
				else
				{
					refTargMax -= targNdSize;
				}
			}
		}

		bwidthTarg = getXMLNodeOffsetByteWidth(refTargMax);

		parentTarg->children = parentSrc->children;

		if (mode != XMLADD_INTO)
		{
			if (mode != XMLADD_REPLACE)
			{
				parentTarg->children++;
			}
			if (newNode->kind == XMLNODE_DOC_FRAGMENT)
			{
				parentTarg->children += ((XMLCompNodeHdr) newNode)->children - 1;
			}
		}

		if (mode != XMLADD_INTO)
		{
			for (i = 0; i < parentTarg->children; i++)
			{
				bool		nextSrcRef = true;

				if (i < newNdIndex)
				{
					refTarg = refSrc + newNdSize;
					if (mode == XMLADD_REPLACE)
					{
						refTarg -= targNdSize;
					}
				}
				else
				{
					if (newNode->kind == XMLNODE_DOC_FRAGMENT)
					{
						XMLCompNodeHdr fragNode = (XMLCompNodeHdr) newNode;
						unsigned short int newNdIndLast = newNdIndex + fragNode->children - 1;

						if (i >= newNdIndex && i <= newNdIndLast)
						{
							refTarg = (XMLNodeOffset) ((char *) parentTarg - newNdRoots[i - newNdIndex]);
							nextSrcRef = false;
						}
						else
						{
							refTarg = refSrc;
						}
					}
					else
					{
						if (i == newNdIndex)
						{
							refTarg = (XMLNodeOffset) ((char *) parentTarg - newNdRoot);

							/*
							 * Adding a new reference now, no need to read one
							 * from the source node.
							 */
							nextSrcRef = false;
						}
						else
						{
							refTarg = refSrc;
						}
					}
				}

				writeXMLNodeOffset(refTarg, &resCursor, bwidthTarg, true);

				if (mode == XMLADD_REPLACE && i == newNdIndex)
				{
					nextSrcRef = true;
				}
				if (nextSrcRef)
				{
					srcCursor += bwidthSrc;

					/*
					 * If the following condition is not met, all references
					 * have been read and it makes no sense trying to
					 * 'decipher' next value from the stream.
					 */
					if (refSrcCount < parentSrc->children)
					{
						refSrc = readXMLNodeOffset(&srcCursor, bwidthSrc, false);
						refSrcCount++;
					}
				}
			}
		}
		else
		{
			/*
			 * parentSrc->children > 0 && mode == XMLADD_INTO
			 *
			 * The XMLADD_INTO mode is specific: only size increment of the
			 * target node affects the references. The new node itself has no
			 * impact here, as it was added one level lower.
			 */
			for (i = 0; i < parentTarg->children; i++)
			{
				refSrc = readXMLNodeOffset(&srcCursor, bwidthSrc, true);
				refTarg = refSrc;
				if (i == newNdIndex)
				{
					refTarg += intoHdrSzIncr;
				}
				else if (i < newNdIndex)
				{
					refTarg += newNdSize + intoHdrSzIncr;
				}
				writeXMLNodeOffset(refTarg, &resCursor, bwidthTarg, true);
			}
		}
		hdrSizeIncr = parentTarg->children * bwidthTarg - parentSrc->children * bwidthSrc;
	}
	else
	{
		/* parentSrc->children == 0 */
		bwidthSrc = 0;

		if (mode != XMLADD_INTO)
		{
			if (newNode->kind == XMLNODE_DOC_FRAGMENT)
			{
				XMLCompNodeHdr fragNode = (XMLCompNodeHdr) newNode;
				unsigned short i;

				refTarg = (XMLNodeOffset) ((char *) parentTarg - newNdRoots[0]);
				bwidthTarg = getXMLNodeOffsetByteWidth(refTarg);
				for (i = 0; i < fragNode->children; i++)
				{
					refTarg = (XMLNodeOffset) ((char *) parentTarg - newNdRoots[i]);
					writeXMLNodeOffset(refTarg, &resCursor, bwidthTarg, true);
				}
				parentTarg->children = fragNode->children;
				hdrSizeIncr = parentTarg->children * bwidthTarg;
			}
			else
			{
				refTarg = (XMLNodeOffset) ((char *) parentTarg - newNdRoot);
				bwidthTarg = getXMLNodeOffsetByteWidth(refTarg);
				writeXMLNodeOffset(refTarg, &resCursor, bwidthTarg, true);
				parentTarg->children = 1;
				hdrSizeIncr = bwidthTarg;
			}
		}
		else
		{
			refTarg = intoHdrSzIncr;
			bwidthTarg = getXMLNodeOffsetByteWidth(refTarg);
			writeXMLNodeOffset(refTarg, &resCursor, bwidthTarg, true);
			parentTarg->children = 1;
			hdrSizeIncr = bwidthTarg;
		}
	}

	if (newNdRoots != NULL)
	{
		pfree(newNdRoots);
	}
	if (bwidthSrc != bwidthTarg)
	{
		XNODE_RESET_REF_BWIDTH(parentTarg);
		XNODE_SET_REF_BWIDTH(parentTarg, bwidthTarg);
	}

	/*
	 * Adjust the scan state so that the scan can continue on the document
	 * returned
	 */
	levelScan->parent = parentTarg;

	/*
	 * 'lastInd' is the index that should be considered the last processed
	 * when scan is going to continue in the result that this function
	 * returns.
	 */

	if (mode == XMLADD_AFTER || mode == XMLADD_REPLACE)
	{
		lastInd = (newNode->kind != XMLNODE_DOC_FRAGMENT) ? newNdIndex :
			newNdIndex + ((XMLCompNodeHdr) newNode)->children - 1;
	}
	else if (mode == XMLADD_BEFORE)
	{
		lastInd = newNdIndex + ((newNode->kind != XMLNODE_DOC_FRAGMENT) ? 1 :
								((XMLCompNodeHdr) newNode)->children);
	}


	if (mode != XMLADD_INTO)
	{
		if (parentSrc->children > 0)
		{
			levelScan->nodeRefPtr = XNODE_FIRST_REF(parentTarg) + lastInd * bwidthTarg;
		}
		else
		{
			levelScan->nodeRefPtr = NULL;
		}
	}
	else
	{
		/*
		 * Reference order does not change, but the width might have done.
		 */
		levelScan->nodeRefPtr = XNODE_FIRST_REF(parentTarg) + newNdIndex * bwidthTarg;
	}

	/* Copy tag name */
	if (parentSrc->common.kind == XMLNODE_ELEMENT)
	{
		unsigned int cntLen = strlen(srcCursor);

		strcpy(resCursor, srcCursor);
		srcCursor += cntLen + 1;
		resCursor += cntLen + 1;
	}

	/*
	 * References to 'srcCursor' or higher (increased earlier by 'shift') need
	 * to be adjusted to additional shift 'hdrSizeIncr'
	 */
	adjustIgnoreList(xscan, (XMLNodeOffset) (srcCursor - inputTree + shift), hdrSizeIncr);
	shift += hdrSizeIncr;

	if (newNode->kind != XMLNODE_ATTRIBUTE && (parentTarg->common.flags & XNODE_EMPTY) &&
		mode != XMLADD_REPLACE)
	{
		/* The element is no longer empty */
		unsigned char resetMask = ~XNODE_EMPTY;

		parentTarg->common.flags &= resetMask;
	}

	/*
	 * Propagate the reference changes to higher levels of the tree if such
	 * exist.
	 */
	propagateChange(levelScan, &shift, &hdrSizeIncr, inputTree, resData, &srcCursor, &resCursor, &newRootOff);

	copyXMLDecl(docNodeSrc, &resCursor);
	docRootOff = (XMLNodeOffset *) resCursor;
	*docRootOff = newRootOff;
	resultSize = resCursor - result + sizeof(XMLNodeOffset);

	if (resultSize > resultSizeMax)
	{
		elog(ERROR, "add node: wrong estimate of result size");
	}
	SET_VARSIZE(result, resultSize);
	if (freeSrc)
	{
		pfree(doc);
	}
	return (xmldoc) result;
}

/*
 * Removes 'targNode' from 'doc'.
 *
 * 'xscan' is updated so that search for the next matching nodes can continue in the new (modified)
 * document.
 *
 * 'freeSrc' says whether the source document should be pfreed. 'true' is only passed when the function
 * returns modified document and then receives it again to remove another node. On the other hand, the
 * source document ('doc') must not be pfreed when the first node is being removed. In such a case, 'doc'
 * is what the xpath() function recieved.
 *
 * Returns modified document.
 */
xmldoc
xmlnodeRemove(xmldoc doc, XMLScan xscan, XMLNodeHdr targNode, bool freeSrc)
{
	char	   *inputTree = (char *) VARDATA(doc);
	XMLNodeOffset *docRootOff = NULL;
	XMLCompNodeHdr docNodeSrc = (XMLCompNodeHdr) XNODE_ROOT(doc);
	XMLScanOneLevel levelScan;
	unsigned int targNdSize;
	unsigned int resultSizeMax;
	char	   *result = NULL;
	char	   *resData = NULL;
	XMLNodeOffset targNdOff;
	XMLNodeOffset srcIncr;
	char	   *srcCursor = inputTree;
	char	   *resCursor = NULL;
	XMLCompNodeHdr parentSrc,
				parentTarg;
	unsigned char bwidthSrc,
				bwidthTarg;
	int			hdrSizeIncr;
	int			shift;
	unsigned short int i;
	XMLNodeOffset refSrc,
				newRootOff,
				refTarg = 0;
	unsigned short int targNdIndex = 0;

	Assert(targNode != NULL);

	/* The target node might have been found by a sub-scan. */
	while (xscan->subScan != NULL)
	{
		xscan = xscan->subScan;
	}
	levelScan = XMLSCAN_CURRENT_LEVEL(xscan);

	targNdOff = (char *) targNode - inputTree;
	targNdSize = getXMLNodeSize(targNode, true);
	resultSizeMax = VARSIZE(doc) - targNdSize;
	result = (char *) palloc(resultSizeMax);
	resCursor = resData = VARDATA(result);

	/*
	 * The principle is quite similar to that of 'xmlnodeAss()', except for
	 * (not) adding any new node.
	 */
	if (targNode->kind == XMLNODE_ELEMENT)
	{
		srcIncr = (char *) getFirstXMLNodeLeaf((XMLCompNodeHdr) targNode) - inputTree;
	}
	else
	{
		srcIncr = targNdOff;
	}
	memcpy(resCursor, inputTree, srcIncr);

	/*
	 * 'targNdSize' is added in order to skip the target node
	 */
	srcCursor += srcIncr + targNdSize;
	resCursor += srcIncr;

	shift = -targNdSize;

	parentSrc = levelScan->parent;
	if (levelScan->siblingsLeft > 1)
	{
		copySiblings(parentSrc, &srcCursor, &resCursor);
	}
	newRootOff = resCursor - resData;
	memcpy(resCursor, srcCursor, srcIncr = sizeof(XMLCompNodeHdrData));
	parentTarg = (XMLCompNodeHdr) resCursor;
	srcCursor += srcIncr;
	resCursor += srcIncr;
	parentTarg->children = parentSrc->children - 1;
	bwidthSrc = XNODE_GET_REF_BWIDTH(parentSrc);

	if (parentSrc->children > 1)
	{
		unsigned int refRangeDecr;
		unsigned short int refSrcCount;

		targNdIndex = parentSrc->children - levelScan->siblingsLeft;
		if (targNdIndex == 0)
		{
			refRangeDecr = getXMLNodeSize(targNode, false);
		}
		else
		{
			refRangeDecr = targNdSize;
		}
		refSrc = readXMLNodeOffset(&srcCursor, bwidthSrc, false);
		refSrcCount = 1;
		bwidthTarg = getXMLNodeOffsetByteWidth(refSrc - refRangeDecr);
		for (i = 0; i < parentSrc->children; i++)
		{
			if (i < targNdIndex)
			{
				refTarg = refSrc - targNdSize;
			}
			else if (i > targNdIndex)
			{
				refTarg = refSrc;
			}
			if (i != targNdIndex)
			{
				writeXMLNodeOffset(refTarg, &resCursor, bwidthTarg, true);
			}
			srcCursor += bwidthSrc;
			if (refSrcCount < parentSrc->children)
			{
				refSrc = readXMLNodeOffset(&srcCursor, bwidthSrc, false);
				refSrcCount++;
			}
		}
		hdrSizeIncr = parentTarg->children * bwidthTarg - parentSrc->children * bwidthSrc;
	}
	else
	{
		/*
		 * No children left
		 */
		hdrSizeIncr = -bwidthSrc;
		bwidthTarg = 1;
		srcCursor += bwidthSrc;
	}

	if (bwidthSrc != bwidthTarg)
	{
		XNODE_RESET_REF_BWIDTH(parentTarg);
		XNODE_SET_REF_BWIDTH(parentTarg, bwidthTarg);
	}

	/*
	 * Adjust the scan state so that the scan can continue on the document
	 * returned
	 */
	levelScan->parent = parentTarg;
	if (parentSrc->children > 1)
	{
		levelScan->nodeRefPtr = XNODE_FIRST_REF(parentTarg) + targNdIndex * bwidthTarg;
	}
	else
	{
		levelScan->nodeRefPtr = NULL;
	}

	shift += hdrSizeIncr;

	if (parentSrc->common.kind == XMLNODE_ELEMENT)
	{
		/*
		 * Copy tag name
		 */
		unsigned int cntLen = strlen(srcCursor);

		strcpy(resCursor, srcCursor);
		srcCursor += cntLen + 1;
		resCursor += cntLen + 1;

		/*
		 * ... does it become empty element?
		 */
		if (!(parentSrc->common.flags & XNODE_EMPTY))
		{
			bool		empty = false;

			if (parentTarg->children > 0)
			{
				char	   *refPtr = XNODE_LAST_REF(parentTarg);
				XMLNodeOffset lastRefOff = readXMLNodeOffset(&refPtr, XNODE_GET_REF_BWIDTH(parentTarg), false);
				XMLNodeHdr	lastChild = (XMLNodeHdr) ((char *) parentTarg - lastRefOff);

				if (lastChild->kind == XMLNODE_ATTRIBUTE)
				{
					empty = true;
				}
			}
			else
			{
				empty = true;
			}
			if (empty)
			{
				parentTarg->common.flags |= XNODE_EMPTY;
			}
		}
	}

	propagateChange(levelScan, &shift, &hdrSizeIncr, inputTree, resData, &srcCursor,
					&resCursor, &newRootOff);

	copyXMLDecl(docNodeSrc, &resCursor);
	docRootOff = (XMLNodeOffset *) resCursor;
	*docRootOff = newRootOff;
	SET_VARSIZE(result, resCursor - result + sizeof(XMLNodeOffset));
	if (freeSrc)
	{
		pfree(doc);
	}
	return (xmldoc) result;
}

/*
 * If the intermediate result set is remembered (in order to keep uniqueness
 * under special conditions), it has to be ensured that the references are
 * valid when node is added/removed/replaced.
 *
 * 'minimum' - the minimum offset value affected
 */
static void
adjustIgnoreList(XMLScan scan, XMLNodeOffset minimum, int shift)
{
	unsigned short i;
	XMLNodeContainer cont = scan->ignoreList;

	if (cont == NULL)
	{
		return;
	}
	for (i = 0; i < cont->position; i++)
	{
		XNodeListItem *item = cont->content + i;

		if (item->kind == XNODE_LIST_ITEM_SINGLE_OFF)
		{
			if (item->value.singleOff >= minimum)
			{
				item->value.singleOff += shift;
			}
		}
		else
		{
			/* This is a node range */
			if (item->value.range.lower >= minimum)
			{
				item->value.range.lower += shift;
				item->value.range.upper += shift;
			}
		}

	}
}

/*
 * Adjust upper level(s) of XML document tree/subtree if node is added to /
 * removed from node pointed to by 'levelScan'. 'levelScan' is adjusted so
 * that scan can continue as if nothing was added / removed.
 */
static void
propagateChange(XMLScanOneLevel levelScan, int *shift, int *hdrSizeIncr,
				char *tree, char *resData, char **srcCursor, char **resCursor, XMLNodeOffset *newRootOff)
{

	while (levelScan->up)
	{
		unsigned short int currChild,
					j,
					gap,
					bwidthSrc,
					bwidthTarg;
		XMLCompNodeHdr parentSrc,
					parentTarg;
		XMLNodeOffset refSrc;
		unsigned int srcIncr;
		XMLNodeOffset parentSrcOff;
		int			refMax;

		/* Move one level higher */
		levelScan = levelScan->up;

		parentSrc = levelScan->parent;
		gap = (char *) parentSrc - *srcCursor;
		if (gap > 0)
		{
			/*
			 * Copy nodes not affected by this change.
			 */
			memcpy(*resCursor, *srcCursor, gap);
		}
		*srcCursor = (char *) parentSrc;
		parentSrcOff = (char *) parentSrc - tree;
		parentTarg = (XMLCompNodeHdr) (resData + parentSrcOff + *shift);
		*resCursor = (char *) parentTarg;
		*newRootOff = (char *) parentTarg - resData;
		memcpy(*resCursor, *srcCursor, srcIncr = sizeof(XMLCompNodeHdrData));
		*srcCursor += srcIncr;
		*resCursor += srcIncr;

		/*
		 * Again, copy (and adjust) the references
		 */
		bwidthSrc = XNODE_GET_REF_BWIDTH(parentSrc);
		refSrc = readXMLNodeOffset(srcCursor, bwidthSrc, false);

		currChild = parentSrc->children - levelScan->siblingsLeft;
		refMax = currChild > 0 ? refSrc + *shift : refSrc + *hdrSizeIncr;

		bwidthTarg = getXMLNodeOffsetByteWidth(refMax);


		for (j = 0; j < parentSrc->children; j++)
		{
			XMLNodeOffset refTarg;;

			if (j < currChild)
			{
				refTarg = refSrc + *shift;
			}
			else if (j == currChild)
			{
				refTarg = refSrc + *hdrSizeIncr;
				levelScan->nodeRefPtr = *resCursor;
			}
			else
			{
				refTarg = refSrc;
			}
			writeXMLNodeOffset(refTarg, resCursor, bwidthTarg, true);

			*srcCursor += bwidthSrc;
			if ((j + 1) < parentSrc->children)
			{
				refSrc = readXMLNodeOffset(srcCursor, bwidthSrc, false);
			}
		}

		if (bwidthSrc != bwidthTarg)
		{
			XNODE_RESET_REF_BWIDTH(parentTarg);
			XNODE_SET_REF_BWIDTH(parentTarg, bwidthTarg);
		}
		if (parentSrc->common.kind == XMLNODE_ELEMENT)
		{
			/*
			 * Copy tag name
			 */
			unsigned int cntLen = strlen(*srcCursor);

			strcpy(*resCursor, *srcCursor);
			*srcCursor += cntLen + 1;
			*resCursor += cntLen + 1;
		}

		/*
		 * Starting the 2nd level above node addition, only the references can
		 * be responsible for increased node size. That's why only header size
		 * is added to the shift.
		 */
		*hdrSizeIncr = parentTarg->children * bwidthTarg - parentSrc->children * bwidthSrc;
		*shift += *hdrSizeIncr;

		/*
		 * Adjust the scan state so that it's usable for the document
		 * returned.
		 */
		levelScan->parent = parentTarg;
	}
}

static void
copyXMLDecl(XMLCompNodeHdr doc, char **resCursor)
{
	if (doc->common.flags & XNODE_DOC_XMLDECL)
	{
		XMLDecl		decl = (XMLDecl) XNODE_ELEMENT_NAME(doc);
		unsigned short declSize = sizeof(XMLDeclData);

		memcpy(*resCursor, decl, declSize);
		*resCursor += declSize;
	}
}


/*
 * Continue copying nodes from '*srcCursor' to '*resCursor' and stop right before their parent.
 */
static void
copySiblings(XMLCompNodeHdr parent, char **srcCursor, char **resCursor)
{
	char	   *refPtr = XNODE_LAST_REF(parent);
	XMLNodeOffset lastSblOffRel = readXMLNodeOffset(&refPtr, XNODE_GET_REF_BWIDTH(parent), false);
	XMLCompNodeHdr lastSbl = (XMLCompNodeHdr) ((char *) parent - lastSblOffRel);
	unsigned int incr = (char *) lastSbl + getXMLNodeSize((XMLNodeHdr) lastSbl, false) - *srcCursor;

	memcpy(*resCursor, *srcCursor, incr);
	*srcCursor += incr;
	*resCursor += incr;
}
