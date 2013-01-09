/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_update.h"
#include "xpath.h"

static void additionChecks(XMLNodeHdr targNode, XMLNodeHdr newNode, XMLAddMode mode);
static XNodeInternal buildTree(XMLNodeHdr srcNode, XNodeInternal parent,
 XMLNodeHdr *targetNodes, unsigned int targNdCount, XNodeInternal *treeNodes,
		  bool recordParents);
static void freeTree(XNodeInternal root);
static unsigned int getTreeStorageSize(XNodeInternal root);
static unsigned int getTreeStorageSizeInternal(XNodeInternal root, int depth);
static void addNodeOrFragment(XMLNodeHdr newNode, XMLNodeContainer container);
static unsigned int getTargetNodePosition(XNodeInternal parent, XMLNodeHdr targNode);
static void checkUnresolvedNamespaces(char *tree, XMLScan xscan, XMLNodeHdr targNode, XMLAddMode addMode,
						  char **unresolved, unsigned int unresolvedCount);

PG_FUNCTION_INFO_V1(xmlnode_add);

Datum
xmlnode_add(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);

	xpath		xpathPtr = (xpath) PG_GETARG_POINTER(1);
	XPathHeader xpHdr = getXPathHeader(xpathPtr);
	XPathExpression expr;
	XPath xpath;

	xmlnode		newNdVar = (xmlnode) PG_GETARG_VARLENA_P(2);
	BpChar	   *modeVar = PG_GETARG_BPCHAR_PP(3);
	char		mode = *(VARDATA_ANY(modeVar));
	XMLNodeHdr	newNode;
	xmldoc		result;
	XMLScanData xscan;
	XMLCompNodeHdr docRoot;

	if (xpHdr->paramCount > 0)
	{
		elog(ERROR, "this function does not accept parameterized xpath expression");
	}

	expr = getXPathExpressionFromStorage(xpHdr);
	xpath = getSingleXPath(expr, xpHdr);

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
	XPathHeader xpHdr = getXPathHeader(xpathPtr);
	XPath xpath;
	XPathExpression exprBase;

	xmldoc		result;
	XMLScanData xscan;
	XMLCompNodeHdr docRoot;

	if (xpHdr->paramCount > 0)
	{
		elog(ERROR, "this function does not accept parameterized xpath expression");
	}

	exprBase = getXPathExpressionFromStorage(xpHdr);
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

	/*
	 * XMLADD_REPLACE is there just to make it compilable. The function
	 * ignores that parameter as long as the action is 'remove'.
	 */
	result = updateXMLDocument(&xscan, doc, XMLNODE_ACTION_REMOVE, NULL, XMLADD_REPLACE);

	finalizeXMLScan(&xscan);
	PG_RETURN_POINTER(result);
}

xmldoc
updateXMLDocument(XMLScan xscan, xmldoc doc, XMLNodeAction action, XMLNodeHdr newNode,
				  XMLAddMode addMode)
{
	XMLNodeHdr	targNode;
	unsigned int i,
				unresolvedNmspCount = 0;
	char	  **unresolvedNamespaces = NULL;
	XMLNodeContainerData targNodes;
	XMLNodeHdr *targNdArray;
	XNodeListItem *listItem;
	XNodeInternal *treeNodes;
	unsigned int targNodesIntSize;
	bool		recordParents;
	XMLNodeOffset docRootOff;
	char	   *docData;
	XMLCompNodeHdr docRoot,
				docRootNew;
	XNodeInternal docModifiable;
	unsigned int matches = 0;
	char	   *result,
			   *resData,
			   *resTmp;
	XMLNodeOffset resRootOff,
			   *resRootOffPtr;
	unsigned int resSize;

	if (action != XMLNODE_ACTION_ADD && action != XMLNODE_ACTION_REMOVE)
	{
		elog(ERROR, "unrecognized update action %u", action);
	}

	docRootOff = XNODE_ROOT_OFFSET(doc);
	docData = (char *) VARDATA(doc);
	docRoot = (XMLCompNodeHdr) (docData + docRootOff);
	targNode = getNextXMLNode(xscan);

	if (targNode == NULL)
	{
		return (xmldoc) copyXMLNode((XMLNodeHdr) docRoot, NULL, true, NULL);
	}

	xmlnodeContainerInit(&targNodes);

	if (action == XMLNODE_ACTION_ADD)
	{
		unresolvedNamespaces = getUnresolvedXMLNamespaces(docData, newNode, &unresolvedNmspCount);
	}

	do
	{
		bool		ignoreTargNode = false;

		/*
		 * If the next node is in a subtree that we're going to replace/remove
		 * anyway, then it makes no sense to pay attention to it. Such a case
		 * can ony happen if the location path uses descendant axe.
		 */
		if (xscan->xpath->descendants > 0 &&
			((action == XMLNODE_ACTION_ADD && addMode == XMLADD_REPLACE) ||
			 action == XMLNODE_ACTION_REMOVE))
		{
			unsigned int j;
			XNodeListItem *item = targNodes.content;

			for (j = 0; j < targNodes.position; j++)
			{
				if (isXMLNodeDescendant(targNode, (XMLCompNodeHdr) item->value.singlePtr))
				{
					ignoreTargNode = true;
					break;
				}
				item++;
			}
		}

		if (ignoreTargNode)
			continue;

		if (action == XMLNODE_ACTION_ADD)
		{
			additionChecks(targNode, newNode, addMode);
		}

		matches++;

		if (action == XMLNODE_ACTION_ADD && unresolvedNmspCount > 0)
		{
			char	   *tree = (char *) VARDATA(xscan->document);

			checkUnresolvedNamespaces(tree, xscan, targNode, addMode, unresolvedNamespaces, unresolvedNmspCount);
		}

		/*
		 * Either the namespace validation passed or the new node has all
		 * namespaces bound.
		 */

		xmlnodePushSinglePtr(&targNodes, targNode);
	} while ((targNode = getNextXMLNode(xscan)) != NULL);

	if (unresolvedNmspCount > 0)
	{
		unsigned int j;

		/* Cleanup. */
		for (j = 0; j < unresolvedNmspCount; j++)
		{
			pfree(unresolvedNamespaces[j]);
		}
		pfree(unresolvedNamespaces);
	}

	targNdArray = (XMLNodeHdr *) palloc(targNodes.position * sizeof(XMLNodeHdr));
	listItem = targNodes.content;

	for (i = 0; i < targNodes.position; i++)
	{
		targNdArray[i] = (XMLNodeHdr) listItem->value.singlePtr;
		listItem++;
	}

	targNodesIntSize = targNodes.position * sizeof(XNodeInternal);
	treeNodes = (XNodeInternal *) palloc(targNodesIntSize);
	MemSet(treeNodes, 0, targNodesIntSize);

	/*
	 * When adding a node *into*, the target node is affected. Otherwise we
	 * have to modify its parent.
	 */
	recordParents = !(action == XMLNODE_ACTION_ADD && addMode == XMLADD_INTO);

	docModifiable = buildTree((XMLNodeHdr) docRoot, NULL, targNdArray, targNodes.position, treeNodes, recordParents);

	for (i = 0; i < targNodes.position; i++)
	{
		unsigned int ctxPos,
					j;
		XMLNodeContainerData childrenNew;
		XNodeInternal treeNode = treeNodes[i];

		Assert(treeNode != NULL);
		Assert(treeNode->children.content != NULL);

		if (action == XMLNODE_ACTION_ADD)
		{
			if (addMode == XMLADD_INTO)
			{
				addNodeOrFragment(newNode, &treeNode->children);
			}
			else if (addMode == XMLADD_BEFORE || addMode == XMLADD_AFTER ||
					 addMode == XMLADD_REPLACE)
			{
				XNodeListItem *listItem;
				unsigned int max;

				/*
				 * 'treeNode' is now parent of the target node. We have to
				 * find at which (context) position the target node is
				 * located.
				 */
				ctxPos = getTargetNodePosition(treeNode, targNdArray[i]);

				/* Now create new list of references. */
				xmlnodeContainerInit(&childrenNew);

				/* Copy references up to the target node. */

				switch (addMode)
				{
					case XMLADD_BEFORE:
					case XMLADD_REPLACE:
						max = ctxPos;
						break;

					case XMLADD_AFTER:
						max = ctxPos + 1;
						break;

					default:
						max = 0;	/* Just keep the compiler silent. */
						elog(ERROR, "unrecognized addition mode %u", addMode);
						break;
				}

				listItem = treeNode->children.content;
				for (j = 0; j < max; j++)
				{
					xmlnodePushSinglePtr(&childrenNew, listItem->value.singlePtr);
					listItem++;
				}

				/* Add the new node(s). */
				addNodeOrFragment(newNode, &childrenNew);

				/*
				 * And the original ones. Skip the target node if replacing
				 * it.
				 */
				if (addMode == XMLADD_REPLACE)
				{
					freeTree(listItem->value.singlePtr);
					listItem++;
					j++;
				}
				for (; j < treeNode->children.position; j++)
				{
					xmlnodePushSinglePtr(&childrenNew, listItem->value.singlePtr);
					listItem++;
				}

				/* Add the new list to the tree. */
				xmlnodeContainerFree(&treeNode->children);
				memcpy(&treeNode->children, &childrenNew, sizeof(XMLNodeContainerData));
			}
		}
		else
		{						/* XMLNODE_ACTION_REMOVE */
			/* Find context position of the node to be removed. */
			ctxPos = getTargetNodePosition(treeNode, targNdArray[i]);

			xmlnodeContainerInit(&childrenNew);

			/* Copy preceding nodes. */
			listItem = treeNode->children.content;
			for (j = 0; j < ctxPos; j++)
			{
				xmlnodePushSinglePtr(&childrenNew, listItem->value.singlePtr);
				listItem++;
			}

			/* Eliminate the target node. */
			freeTree(listItem->value.singlePtr);
			listItem++;
			j++;

			/* And copy the rest. */
			for (; j < treeNode->children.position; j++)
			{
				xmlnodePushSinglePtr(&childrenNew, listItem->value.singlePtr);
				listItem++;
			}

			xmlnodeContainerFree(&treeNode->children);
			memcpy(&treeNode->children, &childrenNew, sizeof(XMLNodeContainerData));
		}
	}

	result = (char *) palloc(getTreeStorageSize(docModifiable));
	resTmp = resData = VARDATA(result);

	writeXMLNodeInternal(docModifiable, false, &resTmp, &resRootOff);
	docRootNew = (XMLCompNodeHdr) (resData + resRootOff);
	checkXMLWellFormedness(docRootNew);

	if (docRoot->common.flags & XNODE_DOC_XMLDECL)
	{
		XMLDecl		decl = (XMLDecl) XNODE_ELEMENT_NAME(docRoot);
		unsigned short declSize = sizeof(XMLDeclData);

		memcpy(resTmp, decl, declSize);
		resTmp += declSize;
		docRootNew->common.flags |= XNODE_DOC_XMLDECL;
	}

	resTmp = (char *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, resTmp);
	resRootOffPtr = (XMLNodeOffset *) resTmp;
	*resRootOffPtr = resRootOff;
	resTmp += sizeof(XMLNodeOffset);
	resSize = (char *) resTmp - result;
	SET_VARSIZE(result, resSize);

	freeTree(docModifiable);
	pfree(targNdArray);
	pfree(treeNodes);
	xmlnodeContainerFree(&targNodes);
	return (xmldoc) result;
}


static void
additionChecks(XMLNodeHdr targNode, XMLNodeHdr newNode, XMLAddMode mode)
{
	Assert(targNode != NULL);

	if (mode != XMLADD_BEFORE && mode != XMLADD_AFTER && mode != XMLADD_INTO &&
		mode != XMLADD_REPLACE)
	{
		elog(ERROR, "unrecognized addition mode: %u", mode);
	}

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

	if (mode == XMLADD_INTO && targNode->kind != XMLNODE_ELEMENT)
	{
		elog(ERROR, "'into' addition mode can only be used for element");
	}
}

/*
 * Creates modifiable (in-memory) structure representing 'srcNode' and all its descendants.
 *
 * 'specialNodes' are those where 'XNodeInternal' must exist either for its *direct* parent
 * (this if for 'before', 'after' or 'replace' mode) or for the node itself ('into' mode).
 * To keep things simple, we always create 'XNodeInternal' for both the node and its parent.
 *
 * If given node has no 'special descendant', then we create 'XNodeInternal' for the whole
 * subtree the node is root of.
 *
 * 'treeNodes' gets filled with pointers to nodes of the output tree that we need
 * to modify. 'recordParents' controls whether these will be the new nodes
 * themselves or their parents.
 */
static XNodeInternal
buildTree(XMLNodeHdr srcNode, XNodeInternal parent,
 XMLNodeHdr *targetNodes, unsigned int targNdCount, XNodeInternal *treeNodes,
		  bool recordParents)
{
	XMLNodeIteratorData iterator;
	XMLNodeHdr	childSrc;
	XNodeInternal result;
	unsigned int i;

	if (srcNode->kind == XMLNODE_DOC_FRAGMENT)
	{
		elog(ERROR, "document fragment is not accepted source node for document tree");
	}

	result = (XNodeInternal) palloc(sizeof(XNodeInternalData));
	result->node = srcNode;
	result->copy = false;
	result->children.content = NULL;

	/* Make the relevant nodes accessible from outside. */
	for (i = 0; i < targNdCount; i++)
	{
		if (srcNode == targetNodes[i])
		{
			if (recordParents)
			{
				treeNodes[i] = parent;
			}
			else
			{
				treeNodes[i] = result;
			}
			break;
		}
	}

	if (srcNode->kind == XMLNODE_DOC || srcNode->kind == XMLNODE_ELEMENT)
	{
		unsigned int i;
		bool		deconstruct = false;
		XMLCompNodeHdr srcComp = (XMLCompNodeHdr) srcNode;

		for (i = 0; i < targNdCount; i++)
		{
			XMLNodeHdr	targetNode = targetNodes[i];

			if (isXMLNodeDescendant(targetNode, srcComp))
			{
				deconstruct = true;
				break;
			}
		}

		if (deconstruct)
		{
			xmlnodeContainerInit(&result->children);
			initXMLNodeIterator(&iterator, srcComp, true);

			while ((childSrc = getNextXMLNodeChild(&iterator)) != NULL)
			{
				XNodeInternal child;

				/*
				 * Target node is located under this node, so it has to be
				 * deconstructed.
				 */
				child = buildTree(childSrc, result, targetNodes, targNdCount, treeNodes, recordParents);
				xmlnodePushSinglePtr(&result->children, child);
			}
		}
	}

	return result;
}

static void
freeTree(XNodeInternal root)
{
	if (root->children.content != NULL)
	{
		unsigned int i;
		XNodeListItem *childItem = root->children.content;

		for (i = 0; i < root->children.position; i++)
		{
			freeTree(childItem->value.singlePtr);
			childItem++;
		}
		xmlnodeContainerFree(&root->children);
	}

	pfree(root);
}

/*
 * Compute storage size for a tree represented by 'root'.
 */
static unsigned int
getTreeStorageSize(XNodeInternal root)
{
	return getTreeStorageSizeInternal(root, 0);
}

static unsigned int
getTreeStorageSizeInternal(XNodeInternal root, int depth)
{
	XNodeListItem *childItem = root->children.content;
	unsigned int result;

	result = getXMLNodeSize(root->node, childItem == NULL);

	if (XNODE_IS_COMPOUND(root->node))
		result += MAX_PADDING(XNODE_ALIGNOF_COMPNODE);

	/*
	 * If we have pointers to the children, their sizes will be added in
	 * recursive calls of this function. Otherwise the node (subtree) size
	 * retrieved above already contains sizes of the children.
	 */
	if (childItem != NULL)
	{
		unsigned int i,
					childCount = root->children.position;

		/* Account for the children... */
		for (i = 0; i < childCount; i++)
		{
			result += getTreeStorageSizeInternal(childItem->value.singlePtr, depth + 1);
			childItem++;
		}

		/*
		 * ... and their references. For simplicity, let's consider the
		 * maximum possible storage per reference.
		 */
		result += childCount * sizeof(XMLNodeOffset);
	}

	if (depth == 0)
	{
		/* Varlena header + root node offset + possible declaration. */
		result += VARHDRSZ + MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET) + sizeof(XMLNodeOffset)
			+ sizeof(XMLDeclData);
	}
	return result;
}

static void
addNodeOrFragment(XMLNodeHdr newNode, XMLNodeContainer container)
{
	XNodeInternal newNodeInt;

	if (newNode->kind == XMLNODE_DOC_FRAGMENT)
	{
		XMLNodeIteratorData iterator;
		XMLNodeHdr	child;

		initXMLNodeIterator(&iterator, (XMLCompNodeHdr) newNode, false);

		while ((child = getNextXMLNodeChild(&iterator)) != NULL)
		{
			newNodeInt = buildTree(child, NULL, NULL, 0, NULL, false);
			xmlnodePushSinglePtr(container, newNodeInt);
		}
	}
	else
	{
		newNodeInt = buildTree(newNode, NULL, NULL, 0, NULL, false);
		xmlnodePushSinglePtr(container, newNodeInt);
	}
}

static unsigned int
getTargetNodePosition(XNodeInternal parent, XMLNodeHdr targNode)
{
	XNodeListItem *item;
	bool		targFound = false;
	unsigned int j;

	item = parent->children.content;
	for (j = 0; j < parent->children.position; j++)
	{
		XNodeInternal nodeInt = (XNodeInternal) item->value.singlePtr;

		if (nodeInt->node == targNode)
		{
			targFound = true;
			break;
		}
		item++;
	}

	if (!targFound)
	{
		elog(ERROR, "target node not found");
	}
	return j;
}

static void
checkUnresolvedNamespaces(char *tree, XMLScan xscan, XMLNodeHdr targNode, XMLAddMode addMode,
						  char **unresolved, unsigned int unresolvedCount)
{
	XMLScanOneLevel scanLevel;
	XMLNodeContainerData declarations;
	unsigned int i,
				j;

	/*
	 * Zero-depth target path is not allowed, so the state should always
	 * exist.
	 */
	Assert(xscan->state != NULL);

	xmlnodeContainerInit(&declarations);

	/*
	 * Collect namespace declarations throughout the stack up to the document
	 * root.
	 */
	scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);
	do
	{
		XMLCompNodeHdr parentNode = scanLevel->parent;

		if (parentNode->common.kind == XMLNODE_ELEMENT)
		{						/* Otherwise it must be XMLNODE_DOC. */
			collectXMLNamespaceDeclarations(tree, parentNode, NULL, NULL, &declarations, true,
											NULL, NULL);
		}
		scanLevel = scanLevel->up;
	} while (scanLevel != NULL);

	/*
	 * If we're adding node *into* the target element, then it's namespace
	 * declarations are important too. (If trying to insert into non-element,
	 * no need to check: error will be raised later.)
	 */
	if (addMode == XMLADD_INTO && targNode->kind == XMLNODE_ELEMENT)
	{
		collectXMLNamespaceDeclarations(tree, (XMLCompNodeHdr) targNode, NULL, NULL, &declarations, true,
										NULL, NULL);
	}

	/* Examine each unresolved namespace. */
	for (i = 0; i < unresolvedCount; i++)
	{
		char	   *namespace = unresolved[i];
		XNodeListItem *declItem = declarations.content;
		bool		found = false;

		for (j = 0; j < declarations.position; j++)
		{
			XMLNodeHdr	declNode;
			char	   *decl;

			Assert(declItem->kind == XNODE_LIST_ITEM_SINGLE_OFF);
			declNode = (XMLNodeHdr) (tree + declItem->value.singleOff);
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
}
