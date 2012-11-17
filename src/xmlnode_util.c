/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_update.h"

static void xmlTreeWalker(XMLTreeWalkerContext *context);
static unsigned int getNodePadding(char *start, XMLNodeOffset offAbs, XMLNodeHdr node);

#ifdef XNODE_DEBUG
static void dumpXScanDebug(StringInfo output, XMLScan scan, char *docData, XMLNodeOffset docRootOff);
#endif

void
xmlnodeContainerInit(XMLNodeContainer cont)
{
	cont->size = XNODE_CONTAINER_CHUNK;
	cont->content = (XNodeListItem *) palloc(cont->size * sizeof(XNodeListItem));
	cont->position = 0;
}

void
xmlnodeContainerFree(XMLNodeContainer cont)
{
	if (cont->content != NULL)
	{
		pfree(cont->content);
	}
}

void
xmlnodePushBoolean(XMLNodeContainer cont, bool boolean)
{
	XNodeListItem itemNew;

	itemNew.kind = XNODE_LIST_ITEM_BOOLEAN;
	itemNew.value.boolean = boolean;
	xmlnodeAddListItem(cont, &itemNew);
}

void
xmlnodePushSingleNode(XMLNodeContainer cont, XMLNodeOffset singleNode)
{
	XNodeListItem itemNew;

	itemNew.kind = XNODE_LIST_ITEM_SINGLE_OFF;
	itemNew.value.singleOff = singleNode;
	xmlnodeAddListItem(cont, &itemNew);
}

void
xmlnodePushSinglePtr(XMLNodeContainer cont, void *item)
{
	XNodeListItem itemNew;

	itemNew.kind = XNODE_LIST_ITEM_SINGLE_PTR;
	itemNew.value.singlePtr = item;
	xmlnodeAddListItem(cont, &itemNew);
}

void
xmlnodeAddListItem(XMLNodeContainer cont, XNodeListItem *itemNew)
{
	unsigned int pos = cont->position;
	XNodeListItem *item = cont->content + pos;

	itemNew->valid = true;
	memcpy(item, itemNew, sizeof(XNodeListItem));

	cont->position++;
	if (cont->position == cont->size)
	{
		cont->size += XNODE_CONTAINER_CHUNK;
		cont->content = (XNodeListItem *) repalloc(cont->content, cont->size
												   * sizeof(XNodeListItem));
		elog(DEBUG1, "node container reallocated. New size: %u.", cont->size);
	}
}

/*
 * Sometimes it's relied on that this function is non-destructive, i.e. the position
 * can be reset in order to retrieve the values multiple times.
 */
XMLNodeOffset
xmlnodePopOffset(XMLNodeContainer cont)
{
	if (cont->position == 0)
	{
		elog(ERROR, "Stack is empty.");
	}
	cont->position--;
	return cont->content[cont->position].value.singleOff;
}

/*
 * Return binary size of a node.
 *
 * 'node' the node to be examined 'subtree' consider subtree (if exists)?
 */
unsigned int
getXMLNodeSize(XMLNodeHdr node, bool subtree)
{
	unsigned int result = 0;
	char	   *content;
	unsigned int attNameLen;

	switch (node->kind)
	{
		case XMLNODE_DOC:
		case XMLNODE_ELEMENT:
		case XMLNODE_DOC_FRAGMENT:
			{
				unsigned int children;
				char		bwidth;

				children = ((XMLCompNodeHdr) node)->children;
				bwidth = XNODE_GET_REF_BWIDTH((XMLCompNodeHdr) node);
				result = sizeof(XMLCompNodeHdrData);

				if (subtree)
				{
					char	   *subtreeStart;

					subtreeStart = (char *) getFirstXMLNodeLeaf((XMLCompNodeHdr) node);
					result += (char *) node - subtreeStart;
				}
				result += children * bwidth;

				if (node->kind == XMLNODE_ELEMENT)
				{
					content = XNODE_ELEMENT_NAME((XMLCompNodeHdr) node);
					result += strlen(content) + 1;
				}
				else if (node->kind == XMLNODE_DOC)
				{
					if (node->flags & XNODE_DOC_XMLDECL)
					{
						result += sizeof(XMLDeclData);
					}
				}
				return result;
			}
		case XMLNODE_DTD:
		case XMLNODE_COMMENT:
		case XMLNODE_CDATA:
		case XMLNODE_PI:
		case XMLNODE_TEXT:
			result = sizeof(XMLNodeHdrData);
			content = (char *) node + result;
			result += strlen(content) + 1;

			if (node->kind == XMLNODE_PI && (node->flags & XNODE_PI_HAS_VALUE))
			{
				content += strlen(content) + 1;
				result += strlen(content) + 1;
			}
			return result;

		case XMLNODE_ATTRIBUTE:
			result = sizeof(XMLNodeHdrData);
			content = (char *) node + result;
			attNameLen = strlen(content) + 1;
			result += attNameLen;
			content += attNameLen;
			result += strlen(content) + 1;
			return result;

		default:
			elog(ERROR, "unrecognized node kind to determine size of: %u", node->kind);
			break;
	}
	return 0;
}

char *
getXMLNodeKindStr(XMLNodeKind k)
{
	StringInfoData result;

	xnodeInitStringInfo(&result, 32);

	switch (k)
	{
		case XMLNODE_DOC:
			appendStringInfoString(&result, "XML document");
			break;
		case XMLNODE_DTD:
			appendStringInfoString(&result, "DTD");
			break;
		case XMLNODE_ELEMENT:
			appendStringInfoString(&result, "XML element");
			break;
		case XMLNODE_ATTRIBUTE:
			appendStringInfoString(&result, "XML element attribute");
			break;
		case XMLNODE_COMMENT:
			appendStringInfoString(&result, "XML comment");
			break;
		case XMLNODE_CDATA:
			appendStringInfoString(&result, "CDATA section");
			break;
		case XMLNODE_PI:
			appendStringInfoString(&result, "processing instruction");
			break;
		case XMLNODE_TEXT:
			appendStringInfoString(&result, "text node");
			break;
		case XMLNODE_DOC_FRAGMENT:
			appendStringInfoString(&result, "document fragment");
			break;
		default:
			elog(ERROR, "unknown node kind: %u", k);
			return NULL;
	}
	return result.data;
}

/*
 * Returns a copy of a node and it's children if there are any.
 *
 * 'node' - the node to copy
 *
 * 'target' - if not NULL, it's assumed that sufficient space is available and the copy is written there,
 * *including possible padding*.
 *
 * If NULL, the appropriate chunk of memory is palloc'd by the function.
 *
 * 'xmlnode' - if 'true', valid value of 'xmlnode' (varlena) type is returned. Otherwise we return
 * block of memory containing XMLNodeHdrData structure as well as all (at lower positions located) descendants.
 *
 * 'root' - at which position of the returned subtree its root element
 * is located. If 'xmlnode' is true, VARHDRSZ is *not* included in this offset.
 */
char *
copyXMLNode(XMLNodeHdr node, char *target, bool xmlnode, XMLNodeOffset *root)
{
	char	   *content = NULL;
	unsigned int cntLen = 0;
	char	   *end,
			   *start,
			   *result;
	unsigned int dataLength,
				resultSizeSafe;
	XMLNodeOffset rootOffAbs;
	unsigned int paddMax,
				padding = 0;

	start = NULL;
	if (XNODE_IS_COMPOUND(node))
	{
		XMLCompNodeHdr compNode = (XMLCompNodeHdr) node;

		content = XNODE_ELEMENT_NAME(compNode);
		switch (node->kind)
		{
			case XMLNODE_ELEMENT:
				cntLen = strlen(content) + 1;
				break;

			case XMLNODE_DOC:
				if (node->flags & XNODE_DOC_XMLDECL)
				{
					cntLen = sizeof(XMLDeclData);
				}
				else
				{
					cntLen = 0;
				}
				break;

			default:
				cntLen = 0;
				break;
		}
		start = (char *) getFirstXMLNodeLeaf(compNode);
	}
	else if (node->kind == XMLNODE_ATTRIBUTE || node->kind == XMLNODE_PI)
	{
		content = (char *) node + sizeof(XMLNodeHdrData);

		/*
		 * Both name and value need to be taken into account
		 */
		cntLen = strlen(content) + 1;
		if (!(node->kind == XMLNODE_PI && ((node->flags & XNODE_PI_HAS_VALUE) == 0)))
		{
			char	   *cntTmp = content + cntLen;

			cntLen += strlen(cntTmp) + 1;
		}
		start = (char *) node;
	}
	else
	{
		switch (node->kind)
		{
			case XMLNODE_DTD:
			case XMLNODE_COMMENT:
			case XMLNODE_CDATA:
			case XMLNODE_PI:
			case XMLNODE_TEXT:
				content = (char *) node + sizeof(XMLNodeHdrData);
				cntLen = strlen(content) + 1;
				break;

			default:
				elog(ERROR, "unable to copy node of type %u", node->kind);
				break;
		}

		if (node->kind == XMLNODE_PI && (node->flags & XNODE_PI_HAS_VALUE))
		{
			content += cntLen;
			cntLen += strlen(content) + 1;
		}
		start = (char *) node;
	}

	rootOffAbs = (char *) node - start;
	end = content + cntLen;

	padding = 0;
	dataLength = end - start;

	if (xmlnode)
	{
		paddMax = MAX_PADDING(XNODE_ALIGNOF_COMPNODE) + MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET);
		resultSizeSafe = VARHDRSZ + dataLength + sizeof(XMLNodeOffset) + paddMax;
	}
	else
	{
		paddMax = MAX_PADDING(XNODE_ALIGNOF_COMPNODE);
		resultSizeSafe = dataLength + paddMax;
	}

	result = (target != NULL) ? target : (char *) palloc(resultSizeSafe);

	if (xmlnode)
	{
		char	   *data,
				   *ptrUnaligned;
		XMLNodeOffset *rootOffRel;

		data = VARDATA(result);
		padding = getNodePadding(data, rootOffAbs, node);
		data += padding;
		memcpy(data, start, dataLength);

		ptrUnaligned = data + dataLength;
		rootOffRel = (XMLNodeOffset *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, ptrUnaligned);
		*rootOffRel = (char *) node - start + padding;

		SET_VARSIZE(result, (char *) rootOffRel - result + sizeof(XMLNodeOffset));
	}
	else
	{
		padding = getNodePadding(result, rootOffAbs, node);
		memcpy(result + padding, start, dataLength);
	}

	if (root != NULL)
	{
		*root = rootOffAbs + padding;
	}

	return result;
}

/*
 * Copy document fragment (i.e. children of 'fragNode', but not 'fragNode' itself)
 * to a memory starting at '*resCursorPtr'.
 *
 * When done, '*resCursorPtr' points right after the copied fragment.
 *
 * Note that '*resCursorPtr' must contain enough space for possible padding.
 *
 * Returns array where each element represents offset of particular new (just inserted) node
 * from the beginning of the output memory chunk.
 */
char	  **
copyXMLDocFragment(XMLCompNodeHdr fragNode, char **resCursorPtr)
{
	unsigned short int i = 0;
	char	  **newNdRoots;
	char	   *resCursor = *resCursorPtr;
	XMLNodeIteratorData iterator;
	XMLNodeHdr	newNdPart;

	if (fragNode->common.kind != XMLNODE_DOC_FRAGMENT)
	{
		elog(ERROR, "incorrect node kind %s where document fragment expected",
			 getXMLNodeKindStr(fragNode->common.kind));
	}
	newNdRoots = (char **) palloc(fragNode->children * sizeof(char *));

	initXMLNodeIterator(&iterator, fragNode, true);

	while ((newNdPart = getNextXMLNodeChild(&iterator)) != NULL)
	{
		XMLNodeOffset newNdPartCopyOff;
		XMLNodeHdr	newNdPartCopy;

		copyXMLNode(newNdPart, resCursor, false, &newNdPartCopyOff);
		newNdPartCopy = (XMLNodeHdr) (resCursor + newNdPartCopyOff);
		newNdRoots[i++] = (char *) newNdPartCopy;

		/* Move right after the last copy. */
		resCursor = (char *) newNdPartCopy + getXMLNodeSize(newNdPartCopy, false);

	}
	*resCursorPtr = resCursor;
	return newNdRoots;
}

/*
 * Note that '*resCursor' must contain enough space for possible padding.
 */
void
copyXMLNodeOrDocFragment(XMLNodeHdr newNode, char **resCursor, char **newNdRoot, char ***newNdRoots)
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

		/*
		 * Move the cursor to the first address following the new node just
		 * copied.
		 */
		*resCursor = *newNdRoot + getXMLNodeSize(newNode, false);
	}
}

/*
 * Returns first leaf node of a subtree that starts with 'elh'. This is tight
 * to the parser behaviour: children are stored at lower addresses than
 * parents. First, the element attributes are stored, then other child nodes
 * (the same logic is applied on them recursively) and finally the element
 * itself.
 */

XMLNodeHdr
getFirstXMLNodeLeaf(XMLCompNodeHdr compNode)
{
	if (!XNODE_HAS_CHILDREN(compNode))
	{
		return (XMLNodeHdr) compNode;
	}
	else
	{
		char	   *firstRef = XNODE_FIRST_REF(compNode);
		XMLNodeHdr	childNode = (XMLNodeHdr) ((char *) compNode - readXMLNodeOffset(&firstRef,
									 XNODE_GET_REF_BWIDTH(compNode), false));

		if (XNODE_IS_COMPOUND(childNode))
			return getFirstXMLNodeLeaf((XMLCompNodeHdr) childNode);
		else
			return childNode;
	}

	/*
	 * Use return just to suppress compiler warnings. Control never gets
	 * there.
	 */
	return NULL;
}

void
checkXMLWellFormedness(XMLCompNodeHdr root)
{
	int			i,
				elIndex,
				dtdIndex;
	unsigned short int elements = 0;
	unsigned short int dtds = 0;
	XMLNodeIteratorData iterator;
	XMLNodeHdr	currNode;

	if (root->common.kind != XMLNODE_DOC && root->common.kind != XMLNODE_DOC_FRAGMENT)
	{
		elog(ERROR, "well-formedness  can't be checked for node type %u", root->common.kind);
	}
	elIndex = dtdIndex = -1;
	i = 0;

	initXMLNodeIterator(&iterator, root, true);

	while ((currNode = getNextXMLNodeChild(&iterator)) != NULL)
	{
		if (currNode->kind == XMLNODE_ELEMENT)
		{
			elements++;
			elIndex = i;
		}
		else if (currNode->kind == XMLNODE_DTD)
		{
			dtds++;
			dtdIndex = i;
		}
		else if (currNode->kind == XMLNODE_DOC || currNode->kind == XMLNODE_ATTRIBUTE ||
				 currNode->kind == XMLNODE_CDATA || currNode->kind == XMLNODE_TEXT ||
				 currNode->kind == XMLNODE_DOC_FRAGMENT)
		{
			elog(ERROR, "%s must not be a direct child of the root",
				 getXMLNodeKindStr(currNode->kind));
		}

		i++;
	}

	if (elements != 1)
	{
		elog(ERROR, "well-formed document must contain exactly one root element");
	}

	if (dtds > 1)
	{
		elog(ERROR, "well-formed document must not contain more than one DTD node");
	}
	else if (dtds > 0 && dtdIndex > elIndex)
	{
		elog(ERROR, "element must not be located before DTD");
	}
}


/*
 * Check if UTF-8 character 'c' fits one item of the 'intervals'.array.
 */
bool
isXMLCharInInterval(char *c, UTF8Interval *intervals, unsigned short int intCount)
{
	unsigned short int i;
	UTF8Interval *interval = intervals;

	for (i = 0; i < intCount; i++)
	{
		if (utf8cmp(c, interval->first) >= 0 && utf8cmp(c, interval->last) <= 0)
		{
			return true;
		}
		interval++;
	}
	return false;
}


int
utf8cmp(char *c1, char *c2)
{
	unsigned char len1 = pg_utf_mblen((unsigned char *) c1);
	unsigned char len2 = pg_utf_mblen((unsigned char *) c2);

	Assert(len1 <= UTF_MAX_WIDTH && len2 <= UTF_MAX_WIDTH);
	if (len1 != len2)
	{
		return len1 > len2 ? 1 : -1;
	}
	else
	{
		unsigned char j;

		for (j = 0; j < len1; j++)
		{
			if (*c1 != *c2)
			{
				return *c1 > *c2 ? 1 : -1;
			}
			c1++;
			c2++;
		}
		return 0;
	}
}

double
xnodeGetNumValue(char *str, bool raiseError, bool *isNumber)
{
	double		result;
	char	   *c;

	result = strtod(str, &c);
	if (str == c)
	{
		*isNumber = false;
		if (raiseError)
		{
			elog(ERROR, "'%s' can't be cast to number", str);
		}
		else
		{
			return 0.0;
		}
	}
	while (*c != '\0')
	{
		if (!XNODE_WHITESPACE(c))
		{
			*isNumber = false;
			if (raiseError)
			{
				elog(ERROR, "'%s' can't be cast to number", str);
			}
			else
			{
				return 0.0;
			}
		}
		c++;
	}
	*isNumber = true;
	return result;
}

char *
getElementNodeStr(XMLCompNodeHdr element)
{
	XMLScanData textScan;
	XMLNodeHdr	textNode;
	StringInfoData si;

	initScanForTextNodes(&textScan, element);
	xnodeInitStringInfo(&si, 32);

	while ((textNode = getNextXMLNode(&textScan)) != NULL)
	{
		char	   *cntPart = XNODE_CONTENT(textNode);

		appendStringInfoString(&si, cntPart);
	}
	finalizeScanForTextNodes(&textScan);
	return si.data;
}

/*
 * Textual form of a node is returned. Elements can be compared without
 * having to construct a single string out of the text nodes.
 */
char *
getNonElementNodeStr(XMLNodeHdr node)
{
	if (node->kind == XMLNODE_ELEMENT)
	{
		elog(ERROR, "unexpected node kind %u", node->kind);
	}
	switch (node->kind)
	{
		case XMLNODE_ATTRIBUTE:
			{
				char	   *attName = XNODE_CONTENT(node);
				char	   *attValue = attName + strlen(attName) + 1;

				return attValue;
			}

		case XMLNODE_COMMENT:
		case XMLNODE_CDATA:
		case XMLNODE_TEXT:
			return XNODE_CONTENT(node);

		case XMLNODE_PI:
			if (node->flags & XNODE_PI_HAS_VALUE)
			{
				char	   *content = XNODE_CONTENT(node);

				content += strlen(content) + 1;
				return content;
			}
			else
			{
				return NULL;
			}

		default:
			elog(ERROR, "unable to convert node of kind %u to string", node->kind);
			break;
	}
	return NULL;
}

void
walkThroughXMLTree(XMLNodeHdr rootNode, VisitXMLNode visitor, bool attributes, void *userData)
{
	XMLTreeWalkerContext context;

	context.stack = (XMLNodeHdr *) palloc(XMLTREE_STACK_CHUNK * sizeof(XMLNodeHdr));
	context.stackSize = XMLTREE_STACK_CHUNK;
	context.depth = 0;
	context.attributes = attributes;
	context.stack[context.depth] = rootNode;
	context.userData = userData;
	context.visitor = visitor;
	xmlTreeWalker(&context);
	pfree(context.stack);
}

/*
 * Test if a valid number starts at 'str'.
 * If it does, then '*end' is set to the first character after the number.
 *
 * If 'skipWhitespace' is true, then also skip all the following whitespace
 * should there be any.
 */
bool
xmlStringIsNumber(char *str, double *numValue, char **end, bool skipWhitespace)
{
	*numValue = strtod(str, end);

	if (*end == str)
	{
		return false;
	}

	if (skipWhitespace)
	{
		while (**end != '\0')
		{
			if (!XNODE_WHITESPACE(*end))
			{
				return false;
			}
			*end += pg_utf_mblen((unsigned char *) *end);
		}
		return true;
	}
	else
	{
		return true;
	}
}

bool
xmlStringWhitespaceOnly(char *str)
{
	while (*str != '\0')
	{
		unsigned int cWidth = pg_utf_mblen((unsigned char *) str);

		if (!XNODE_WHITESPACE(str))
		{
			break;
		}
		str += cWidth;
	}

	/* The string contains merely white spaces. */
	return (*str == '\0');
}

/*
 * It shouldn't be possible to create a document fragment containing mixture
 * of attribute and non-attribute nodes. Nevertheless, it's better to check all
 * direct children.
 */
bool
checkFragmentForAttributes(XMLCompNodeHdr fragment)
{
	XMLNodeHdr	child;
	XMLNodeIteratorData iterator;

	Assert(fragment->common.kind == XMLNODE_DOC_FRAGMENT);

	initXMLNodeIterator(&iterator, fragment, true);

	while ((child = getNextXMLNodeChild(&iterator)) != NULL)
	{
		if (child->kind == XMLNODE_ATTRIBUTE)
		{
			return true;
		}
	}
	return false;
}

/*
 * Returns true if 'node' is either descendant of 'treeRoot' or the root
 * itself.
 */
bool
isXMLNodeDescendant(XMLNodeHdr node, XMLCompNodeHdr treeRoot)
{
	XMLNodeHdr	firstLeaf;

	if (node == (XMLNodeHdr) treeRoot)
		return true;

	if (!XNODE_IS_COMPOUND((XMLNodeHdr) treeRoot))
		return false;

	firstLeaf = getFirstXMLNodeLeaf(treeRoot);
	return (node >= firstLeaf && node < (XMLNodeHdr) treeRoot);
}

static void
xmlTreeWalker(XMLTreeWalkerContext *context)
{
	XMLNodeHdr	node = context->stack[context->depth];

	context->visitor(context->stack, context->depth, context->userData);

	if (XNODE_IS_COMPOUND(node) || node->kind >= XMLTEMPLATE_ROOT)
	{
		XMLCompNodeHdr compNode = (XMLCompNodeHdr) node;
		bool		isTemplate = compNode->common.kind >= XMLTEMPLATE_ROOT;
		XMLNodeIteratorData iterator;
		XMLNodeHdr	child;
		unsigned int childNr = 0;

		context->depth++;
		if (context->depth == XMLTREE_WALKER_MAX_DEPTH)
		{
			elog(ERROR, "maximum tree depth %u reached while walking through the document tree", XMLTREE_WALKER_MAX_DEPTH);
		}

		if (context->depth == context->stackSize)
		{
			context->stackSize += XMLTREE_STACK_CHUNK;
			context->stack = (XMLNodeHdr *) repalloc(context->stack, context->stackSize);
		}

		if (isTemplate)
			initXMLNodeIteratorSpecial(&iterator, compNode, true);
		else
			initXMLNodeIterator(&iterator, compNode, true);

		while ((child = getNextXMLNodeChild(&iterator)) != NULL)
		{
			if (isTemplate && (child == (XMLNodeHdr) compNode))

				/*
				 * 'child == compNode' indicates 'XMLNodeOffsetInvalid'. In
				 * other words: missing optional attribute.
				 */
				continue;

			if (child->kind == XMLNODE_ATTRIBUTE && !context->attributes)
				continue;

			context->stack[context->depth] = child;
			xmlTreeWalker(context);
			childNr++;
		}
		context->depth--;
	}
}

#ifdef XNODE_DEBUG

static void
dumpXScanDebug(StringInfo output, XMLScan scan, char *docData, XMLNodeOffset docRootOff)
{
	unsigned short i;
	XMLScanOneLevel level = scan->state;

	appendStringInfo(output, "xscan [dpth: %u, xpthroot: %u, xpthdpth: %u]\n", scan->depth,
					 scan->xpathRoot, scan->xpath->depth);
	for (i = 0; i <= scan->depth; i++)
	{
		XMLCompNodeHdr parent = level->parent;
		char	   *firstRefPtr = XNODE_FIRST_REF(parent);
		char		bwidth = XNODE_GET_REF_BWIDTH(parent);

		appendStringInfo(output, "level: %u, at: %u, sbl: %u, ", i,
						 (unsigned int) (((char *) level->nodeRefPtr - firstRefPtr) / bwidth), level->siblingsLeft);

		appendStringInfo(output, "parent[");
		if (i > 0)
		{
			appendStringInfo(output, "%u", (XMLNodeOffset) ((char *) parent - docData));
		}
		else
		{
			appendStringInfoString(output, scan->parent == NULL ? "document" : "upper scan target");
		}
		appendStringInfoString(output, "]\n");
		level++;
	}
}
#endif


/*
 * Convenience function to allow allocation of small chunk where the default
 * initial size seems to be unnecessary.
 */
void
xnodeInitStringInfo(StringInfo stringInfo, int len)
{
	stringInfo->maxlen = len;
	stringInfo->data = (char *) palloc(stringInfo->maxlen);
	resetStringInfo(stringInfo);
}

void
initXMLNodeIterator(XMLNodeIterator iterator, XMLCompNodeHdr node, bool attributes)
{
	if (!XNODE_IS_COMPOUND((XMLNodeHdr) node) && node->common.kind != XMLTEMPLATE_ROOT)
		elog(ERROR, "element, document, document fragment or template expected for iteration, %u received instead",
			 node->common.kind);

	iterator->node = node;
	iterator->bwidth = XNODE_GET_REF_BWIDTH(node);
	iterator->childrenLeft = node->children;
	iterator->childOffPtr = XNODE_FIRST_REF(node);
	iterator->attributes = attributes;
}

void
initXMLNodeIteratorSpecial(XMLNodeIterator iterator, XMLCompNodeHdr node,
						   bool attrsSpecial)
{
	XMLNodeKind kind = node->common.kind;

	if (kind < XMLTEMPLATE_ROOT)
		elog(ERROR, "template node expected for iteration, %u received instead", kind);

	/*
	 * Set fake value so there is no error when reusing the simpler
	 * initializer.
	 */
	node->common.kind = XMLNODE_ELEMENT;
	/* Let's reuse the simpler initializer now. */
	initXMLNodeIterator(iterator, node, true);
	/* Finally restore the original kind. */
	iterator->node->common.kind = node->common.kind = kind;

	if (kind > XMLTEMPLATE_ROOT && !attrsSpecial)
	{
		unsigned int i;
		XNodeSpecAttributes *specAttrInfo = getXNodeAttrInfo(kind);
		unsigned int attrsToSkip = specAttrInfo->number;

		Assert(attrsToSkip <= node->children);

		for (i = 0; i < attrsToSkip; i++)
			readXMLNodeOffset(&iterator->childOffPtr, iterator->bwidth, true);
	}
}

/*
 * Return the next child of XML element (next in the document order) or NULL if
 * the iteration is finished.
 *
 * If special node is iterated and special attributes are not excluded, pointer
 * to the iterated node is returned when we hit missing optional node. This is
 * because 'XMLNodeOffsetInvalid' (0) represents reference to such a node. NULL
 * can't be used in such a case as special value because it already has
 * specaila meaning: end of iteration.
 *
 * IMPORTANT
 *
 * Node returned by this function may only be used as *read-only*.
 * The data is typically located in shared buffer and any change lead to unforeseen effects.
 *
 */
XMLNodeHdr
getNextXMLNodeChild(XMLNodeIterator iterator)
{
	XMLNodeHdr	childNode;
	XMLNodeOffset childOff;

	if (iterator->childrenLeft == 0)
		return NULL;

	childOff = readXMLNodeOffset(&iterator->childOffPtr, iterator->bwidth, true);
	childNode = (XMLNodeHdr) ((char *) iterator->node - childOff);

	/*
	 * The following does not make sense for special node because that can
	 * have 'childOff == XMLNodeOffsetInvalid' and therefore 'childNode->kind'
	 * may be undefined.
	 */
	if (iterator->node->common.kind < XMLTEMPLATE_ROOT)
	{

		if (childNode->kind == XMLNODE_ATTRIBUTE && !iterator->attributes)
		{
			bool		attrOrig;

			/* Skip attribute nodes. */
			attrOrig = iterator->attributes;
			iterator->attributes = true;

			while (childNode != NULL && childNode->kind == XMLNODE_ATTRIBUTE)
			{
				childNode = getNextXMLNodeChild(iterator);
			}
			iterator->attributes = attrOrig;
		}
	}
	iterator->childrenLeft--;
	return childNode;
}


/*
 * Document root may have multiple children, but exactly one element.
 * If the document is special (e.g. XNT), 'required' specifies kind of
 * the element.
 */
XMLCompNodeHdr
getXMLDocRootElement(XMLCompNodeHdr docRoot, XMLNodeKind required)
{
	XMLNodeIteratorData iterator;
	XMLNodeHdr	child;

	initXMLNodeIterator(&iterator, docRoot, false);

	while ((child = getNextXMLNodeChild(&iterator)) != NULL)
	{

		if (child->kind == required)
			return (XMLCompNodeHdr) child;
	}

	elog(ERROR, "failed to find root element.");
	return NULL;
}

/*
 * 'node' is either a single node or a root of a subtree. In the 2nd case it has to be
 * used to determine padding and not the children.
 *
 * Note that 1-byte aligned child node (e.g. attribute) can be at the beginning of the block.
 * That does not restrict the target address, however the parent (element) does.
 * On the other hand, If any child demands 2 B or higher alignment too, it's not our problem:
 * whoever created such structure must have ensured the correct distance between
 * parent and children.
 *
 * 'start' and 'offAbs' - the intended position of the node before alignment was considered.
 */
static unsigned int
getNodePadding(char *start, XMLNodeOffset offAbs, XMLNodeHdr node)
{
	unsigned int result = 0;

	if (XNODE_IS_COMPOUND(node))
	{

		char	   *ptrUnaligned,
				   *ptrAligned;

		/* Where is the node (not its descendants) going to fall? */
		ptrUnaligned = start + offAbs;

		/*
		 * Exactly this node has to be used to determine the padding.
		 *
		 *
		 */
		ptrAligned = (char *) TYPEALIGN(XNODE_ALIGNOF_COMPNODE, ptrUnaligned);

		result = ptrAligned - ptrUnaligned;
	}

	return result;
}
