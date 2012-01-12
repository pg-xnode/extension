#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_update.h"
#include "xpath.h"

static void adjustTempResult(XMLScan scan, XMLNodeOffset minimum, int shift);
static void propagateChange(XMLScanOneLevel levelScan, int *shift, int *hdrSizeIncr, char *tree, char *resData,
			 char **srcCursor, char **resCursor, XMLNodeOffset * newRootOff);
static void copyXMLNodeOrDocFragment(XMLNodeHdr newNode, unsigned int newNdSize, char **resCursor,
						 char **newNdRoot, char ***newNdRoots);
static char **copyXMLDocFragment(XMLCompNodeHdr fragNode, char **resCursorPtr);
static void copyXMLDecl(XMLCompNodeHdr doc, char **resCursor);
static void copySiblings(XMLCompNodeHdr parent, char **srcCursor, char **resCursor);

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

			if (action == XMLNODE_ACTION_ADD)
			{
				result = xmlnodeAdd(result, xscan, targNode, newNode, addMode, freeSrc);
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
 * Well-formedness of the resulting documents needs to be checked.
 */
xmldoc
xmlnodeAdd(xmldoc doc, XMLScan xscan, XMLNodeHdr targNode, XMLNodeHdr newNode,
		   XMLAddMode mode, bool freeSrc)
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
	if (newNode->kind == XMLNODE_DOC_FRAGMENT)
	{
		Assert(((XMLCompNodeHdr) newNode)->children > 0);
	}

	/*
	 * Estimate how much the storage will grow First, find out if some
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
			Assert((i == 0 && levelNode->common.kind == XMLNODE_DOC) ||
				   (i > 0 && levelNode->common.kind == XMLNODE_ELEMENT));

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

		/*
		 * Copy the new node(s)
		 */
		copyXMLNodeOrDocFragment(newNode, newNdSize, &resCursor, &newNdRoot, &newNdRoots);

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
			adjustTempResult(xscan, (XMLNodeOffset) (srcCursor - inputTree), shift);
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
			adjustTempResult(xscan, (XMLNodeOffset) (srcCursor - inputTree + targNdSize), shift);
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
		adjustTempResult(xscan, (XMLNodeOffset) (srcCursor - inputTree + 1), newNdSize);
		srcCursor += srcIncr;
		resCursor += srcIncr;

		shift = newNdSize;

		/*
		 * ... and copy the new node
		 */
		copyXMLNodeOrDocFragment(newNode, newNdSize, &resCursor, &newNdRoot, &newNdRoots);
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

		/*
		 * Copy the new node
		 */
		copyXMLNodeOrDocFragment(newNode, newNdSize, &resCursor, &newNdRoot, &newNdRoots);

		/*
		 * 'srcCursor' now points at the (still empty) target element, which
		 * will be moved by new node insertion. Therefore, references to the
		 * target node and all the following must be adjusted.
		 */
		shift = newNdSize;
		adjustTempResult(xscan, (XMLNodeOffset) (srcCursor - inputTree), shift);

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
		memcpy(resCursor, srcCursor, cntLen);
		srcCursor += cntLen + 1;
		resCursor += cntLen;
		*resCursor = '\0';
		resCursor++;

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
			adjustTempResult(xscan, (XMLNodeOffset) (srcCursor - inputTree + shift), intoHdrSzIncr);
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

		bwidthSrc = XNODE_GET_REF_BWIDTH(parentSrc);
		refSrc = readXMLNodeOffset(&srcCursor, bwidthSrc, false);

		/*
		 * TODO if the new node will have index 0, only its header size should
		 * be added to the maximum reference range and not the whole
		 * 'newNdSize'
		 */
		if (mode != XMLADD_REPLACE)
		{
			bwidthTarg = getXMLNodeOffsetByteWidth(refSrc + newNdSize);
		}
		else
		{
			bwidthTarg = getXMLNodeOffsetByteWidth(refSrc + newNdSize - targNdSize);
		}


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

		memcpy(resCursor, srcCursor, cntLen);
		srcCursor += cntLen + 1;
		resCursor += cntLen;
		*resCursor = '\0';
		resCursor++;
	}

	/*
	 * References to 'srcCursor' or higher (increased earlier by 'shift') need
	 * to be adjusted to additional shift 'hdrSizeIncr'
	 */
	adjustTempResult(xscan, (XMLNodeOffset) (srcCursor - inputTree + shift), hdrSizeIncr);
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
		bwidthTarg = 0;
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

		memcpy(resCursor, srcCursor, cntLen);
		srcCursor += cntLen + 1;
		resCursor += cntLen;
		*resCursor = '\0';
		resCursor++;

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
	i = xscan->depth;
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
adjustTempResult(XMLScan scan, XMLNodeOffset minimum, int shift)
{
	unsigned short i;
	XMLNodeContainer cont = scan->resTmp;

	if (cont == NULL)
	{
		return;
	}
	for (i = 0; i < cont->position; i++)
	{
		XMLNodeOffset value = cont->items[i];

		if (value >= minimum)
		{
			cont->items[i] = value + shift;
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
				char *tree, char *resData, char **srcCursor, char **resCursor, XMLNodeOffset * newRootOff)
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

			memcpy(*resCursor, *srcCursor, cntLen);
			*srcCursor += cntLen + 1;
			*resCursor += cntLen;
			**resCursor = '\0';
			(*resCursor)++;
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
copyXMLNodeOrDocFragment(XMLNodeHdr newNode, unsigned int newNdSize, char **resCursor,
						 char **newNdRoot, char ***newNdRoots)
{

	XMLNodeOffset newNdOff;

	if (newNode->kind == XMLNODE_DOC_FRAGMENT)
	{
		*newNdRoots = copyXMLDocFragment((XMLCompNodeHdr) newNode, resCursor);
	}
	else
	{
		copyXMLNode(newNode, *resCursor, false, &newNdOff);
		*newNdRoot = *resCursor + newNdOff;
		*resCursor += newNdSize;
	}
}

/*
 * Copy document fragment (i.e. children of 'fragNode', but not 'fragNode' itself)
 * to a memory starting at '*resCursorPtr'.
 * When done, '*resCursorPtr' points right after the copied fragment.
 *
 * Returns array where each element represents offset of particular new (just inserted) node
 * from the beginning of the output memory chunk.
 */
static char **
copyXMLDocFragment(XMLCompNodeHdr fragNode, char **resCursorPtr)
{
	char	   *refs = XNODE_FIRST_REF(fragNode);
	char		bwidth = XNODE_GET_REF_BWIDTH(fragNode);
	unsigned short int i;
	char	  **newNdRoots;
	char	   *resCursor = *resCursorPtr;

	if (fragNode->common.kind != XMLNODE_DOC_FRAGMENT)
	{
		elog(ERROR, "incorrect node kind %s where document fragment expected",
			 getXMLNodeKindStr(fragNode->common.kind));
	}
	newNdRoots = (char **) palloc(fragNode->children * sizeof(char *));
	for (i = 0; i < fragNode->children; i++)
	{
		XMLNodeHdr	newNdPart = (XMLNodeHdr) ((char *) fragNode - readXMLNodeOffset(&refs, bwidth, true));
		XMLNodeOffset newNdPartOff;

		copyXMLNode(newNdPart, resCursor, false, &newNdPartOff);
		newNdRoots[i] = resCursor + newNdPartOff;
		resCursor += getXMLNodeSize(newNdPart, true);
	}
	*resCursorPtr = resCursor;
	return newNdRoots;
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
