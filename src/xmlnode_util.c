/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_update.h"

static void xmlTreeWalker(XMLTreeWalkerContext *context);

static void checkNodeNamespaces(XMLNodeHdr *stack, unsigned int depth, void *userData);
static char *getNamespaceName(char *name, char *colon);
static void addUniqueNamespace(XMLNodeContainer result, char *nmspName);

static unsigned int getNodePadding(char *start, XMLNodeOffset offAbs, XMLNodeHdr node);

struct XMLNodeDumpInfo
{
	StringInfo	output;
	char	   *start;			/* where the tree storage starts */
};

static void visitXMLNodeForDump(XMLNodeHdr *stack, unsigned int depth, void *userData);

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
	if (node->kind == XMLNODE_ELEMENT || node->kind == XMLNODE_DOC || node->kind == XMLNODE_DOC_FRAGMENT)
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

		if (childNode->kind == XMLNODE_ELEMENT || childNode->kind == XMLNODE_DOC ||
			childNode->kind == XMLNODE_DOC_FRAGMENT)
		{
			return getFirstXMLNodeLeaf((XMLCompNodeHdr) childNode);
		}
		else
		{
			return childNode;
		}
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
			elog(ERROR, "unable to compare node set element of type %u", node->kind);
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

void
dumpXMLNodeDebug(StringInfo output, char *data, XMLNodeOffset off)
{
	XMLNodeHdr	root = (XMLNodeHdr) (data + off);
	struct XMLNodeDumpInfo userData;

	userData.output = output;
	userData.start = data;
	walkThroughXMLTree(root, visitXMLNodeForDump, true, (void *) &userData);
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
	XMLNodeKind rootKind = treeRoot->common.kind;

	if (node == (XMLNodeHdr) treeRoot)
	{
		return true;
	}

	if (rootKind != XMLNODE_DOC && rootKind != XMLNODE_ELEMENT &&
		rootKind != XMLNODE_DOC_FRAGMENT)
	{
		return false;
	}

	firstLeaf = getFirstXMLNodeLeaf(treeRoot);
	return (node >= firstLeaf && node < (XMLNodeHdr) treeRoot);
}

static void
xmlTreeWalker(XMLTreeWalkerContext *context)
{
	XMLNodeHdr	node = context->stack[context->depth];

	context->visitor(context->stack, context->depth, context->userData);

	if (node->kind == XMLNODE_DOC || node->kind == XMLNODE_ELEMENT ||
		node->kind == XMLNODE_DOC_FRAGMENT || node->kind >= XNTNODE_ROOT)
	{

		XMLCompNodeHdr compNode = (XMLCompNodeHdr) node;
		XMLNodeIteratorData iterator;
		XMLNodeHdr	child;

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

		initXMLNodeIterator(&iterator, compNode, true);

		while ((child = getNextXMLNodeChild(&iterator)) != NULL)
		{
			if (child->kind == XMLNODE_ATTRIBUTE && !context->attributes)
			{
				continue;
			}
			context->stack[context->depth] = child;
			xmlTreeWalker(context);
		}
		context->depth--;
	}
}

static void
visitXMLNodeForDump(XMLNodeHdr *stack, unsigned depth, void *userData)
{
	XMLNodeHdr	node = (XMLNodeHdr) stack[depth];
	struct XMLNodeDumpInfo *ud = (struct XMLNodeDumpInfo *) userData;
	StringInfo	output = ud->output;
	char	   *str;
	unsigned int size;
	XMLNodeOffset offAbs = (char *) node - ud->start;
	XMLNodeOffset offRel = 0;

	appendStringInfoSpaces(output, depth);

	if (depth > 0)
	{
		XMLNodeHdr	parent = (XMLNodeHdr) stack[depth - 1];

		offRel = (char *) parent - (char *) node;
	}

	switch (node->kind)
	{
		case XMLNODE_ELEMENT:
			size = getXMLNodeSize(node, true);
			appendStringInfo(output, "%s (abs: %u , rel: %u , size: %u)\n",
			XNODE_ELEMENT_NAME((XMLCompNodeHdr) node), offAbs, offRel, size);
			break;

		case XMLNODE_ATTRIBUTE:
			str = XNODE_CONTENT(node);
			break;

		case XMLNODE_COMMENT:
			str = "<comment>";
			break;

		case XMLNODE_CDATA:
			str = "CDATA";
			break;

		case XMLNODE_PI:
			str = "PI";
			break;

		case XMLNODE_TEXT:
			str = "<text>";
			break;

		default:
			elog(ERROR, "unrecognized node kind %u", node->kind);
			break;

	}

	if (node->kind != XMLNODE_ELEMENT)
	{
		size = getXMLNodeSize(node, true);
		if (node->kind == XMLNODE_ATTRIBUTE)
		{
			appendStringInfoChar(output, '@');
		}
		appendStringInfo(output, "%s (abs: %u , rel: %u , size: %u)\n", str, offAbs, offRel, size);
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
 * Returns vector of (unique) namespaces used by 'node' and its descendants but not declared
 * at the appropriate level or above.
 *
 * 'count' receives length of the array.
 */
char	  **
getUnresolvedXMLNamespaces(XMLNodeHdr node, unsigned int *count)
{
	XMLCompNodeHdr element;
	XMLNamespaceCheckState stateData;
	char	  **result = NULL;
	unsigned int resultSize;
	unsigned int i;
	XNodeListItem *item;

	*count = 0;

	if (node->kind == XMLNODE_ATTRIBUTE && (node->flags & XNODE_NMSP_PREFIX))
	{
		char	   *attrName = XNODE_CONTENT(node);
		unsigned int nmspPrefLen = strlen(XNODE_NAMESPACE_DEF_PREFIX);

		if ((strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX, nmspPrefLen) == 0) &&
			attrName[nmspPrefLen] == XNODE_CHAR_COLON)
		{
			/* 'xmlns:...' does not depend on any namespace declaration. */
			return NULL;
		}

		/*
		 * If the prefix is not 'xmlns' then it represents an unbound
		 * namespace.
		 *
		 * This is a special case when we're for example adding (prefixed)
		 * attribute to element. Attributes already pertaining to the element
		 * are checked elsewhere (i.e. while checking the owning element
		 * itself).
		 */
		result = (char **) palloc(sizeof(char *));
		result[0] = strtok(attrName, ":");
		*count = 1;
		return result;
	}

	element = (XMLCompNodeHdr) node;
	if (element->common.kind != XMLNODE_DOC && element->common.kind != XMLNODE_ELEMENT)
	{
		return NULL;
	}

	xmlnodeContainerInit(&stateData.declarations);
	xmlnodeContainerInit(&stateData.result);
	walkThroughXMLTree((XMLNodeHdr) node, checkNodeNamespaces, false, (void *) &stateData);
	resultSize = stateData.result.position;
	xmlnodeContainerFree(&stateData.declarations);

	result = (char **) palloc(resultSize * sizeof(char *));
	item = stateData.result.content;
	for (i = 0; i < resultSize; i++)
	{
		char	   *nmspName;

		Assert(item->kind == XNODE_LIST_ITEM_SINGLE_PTR);
		nmspName = (char *) item->value.singlePtr;
		result[i] = nmspName;
		item++;
	}
	xmlnodeContainerFree(&stateData.result);
	*count = resultSize;
	return result;
}

void
resolveNamespaces(XMLNodeContainer declarations, unsigned int declsActive, char *elNmspName,
				  bool *elNmspNameResolved, XMLNodeHdr *attrsPrefixed, unsigned int attrsPrefixedCount, bool *attrFlags,
				  unsigned short *attrsUnresolved, char *specNmspName, char *specNmspValue, bool *elNmspIsSpecial)
{

	unsigned int i;
	XNodeListItem *decls = declarations->content;

	/*
	 * Search the stack bottom-up for the first appropriate declaration.
	 */
	for (i = declsActive; i > 0; i--)
	{
		char	   *declNmspName;
		XNodeListItem *item = decls + i - 1;
		unsigned int declNmspLength;
		XMLNodeHdr	declNode;

		Assert(item->kind == XNODE_LIST_ITEM_SINGLE_PTR);
		declNode = (XMLNodeHdr) item->value.singlePtr;
		declNmspName = (char *) XNODE_CONTENT(declNode) + strlen(XNODE_NAMESPACE_DEF_PREFIX) + 1;
		declNmspLength = strlen(declNmspName);

		if (!(*elNmspNameResolved))
		{
			if (strncmp(elNmspName, declNmspName, declNmspLength) == 0 &&
				elNmspName[declNmspLength] == XNODE_CHAR_COLON)
			{
				*elNmspNameResolved = true;

				/*
				 * If seems to be a special namespace (e.g. 'xnt'), check
				 * whether the namespace value matches. If it does not, it
				 * must be considered an 'ordinary namespace'.
				 */
				if (specNmspName != NULL && specNmspValue != NULL && elNmspIsSpecial != NULL)
				{
					char	   *declNmspValue = (char *) declNmspName + declNmspLength + 1;

					*elNmspIsSpecial = (strcmp(declNmspName, specNmspName) == 0 &&
								  strcmp(declNmspValue, specNmspValue) == 0);
				}
			}
		}

		if (*attrsUnresolved > 0)
		{
			unsigned int j;

			/*
			 * Try to match any unresolved (prefixed) attribute to the current
			 * namespace declaration.
			 */
			for (j = 0; j < attrsPrefixedCount; j++)
			{
				if (!attrFlags[j])
				{
					XMLNodeHdr	attrNode = attrsPrefixed[j];
					char	   *attrNmspName = XNODE_CONTENT(attrNode);

					Assert(attrNode->flags & XNODE_NMSP_PREFIX);

					if (strncmp(attrNmspName, declNmspName, declNmspLength) == 0 &&
						attrNmspName[declNmspLength] == XNODE_CHAR_COLON)
					{
						attrFlags[j] = true;
						(*attrsUnresolved)--;
					}
				}
			}
		}

		if (*elNmspNameResolved && *attrsUnresolved == 0)
		{
			break;
		}
	}
}

/*
 * Collect names of all namespaces used in the current node (element) and find out which
 * are have no declaration neither in this node nor above it.
 */
static void
checkNodeNamespaces(XMLNodeHdr *stack, unsigned int depth, void *userData)
{
	XMLNamespaceCheckState *state;
	XMLCompNodeHdr currentNode = (XMLCompNodeHdr) stack[depth];
	char	   *elNmspName = XNODE_ELEMENT_NAME(currentNode);
	char	   *elNmspColon;
	bool		elNmspNameResolved = true;
	XMLNodeHdr *attrsPrefixed = NULL;
	bool	   *attrFlags = NULL;
	unsigned short attrsUnresolved = 0;
	unsigned int attrsPrefixedCount = 0;
	unsigned int attrCount = 0;
	unsigned int nmspDeclCount = 0;

	/*
	 * The tree walker is invoked with 'attributes=false' so it does not
	 * descend to attributes.
	 */
	Assert(currentNode->common.kind != XMLNODE_ATTRIBUTE);

	if (currentNode->common.kind != XMLNODE_ELEMENT && currentNode->common.kind != XMLNODE_DOC_FRAGMENT)
	{
		return;
	}

	state = (XMLNamespaceCheckState *) userData;

	if (currentNode->children > 0)
	{
		collectXMLNamespaceDeclarations(currentNode, &attrCount, &nmspDeclCount, &state->declarations, false,
										&attrsPrefixed, &attrsPrefixedCount);
		attrsUnresolved = attrsPrefixedCount;
	}

	if (attrCount > 0)
	{
		unsigned int flagsSize = currentNode->children * sizeof(bool);

		attrFlags = (bool *) palloc(flagsSize);
		memset(attrFlags, false, flagsSize);
	}

	/*
	 * 'nmspStack->counts[depth]' says how many declarations (counting from
	 * position 0 in the stack) are valid for the current xml element.
	 */
	state->counts[depth] = nmspDeclCount;;
	if (depth > 0)
	{
		state->counts[depth] += state->counts[depth - 1];
	}

	/* And now finally check prefixes used in the current element. */

	Assert(elNmspName[0] != XNODE_CHAR_COLON);
	elNmspColon = strchr(elNmspName, XNODE_CHAR_COLON);
	if (elNmspColon != NULL)
	{
		elNmspNameResolved = false;
	}

	if (!elNmspNameResolved || attrsUnresolved > 0)
	{
		resolveNamespaces(&state->declarations, state->counts[depth], elNmspName, &elNmspNameResolved, attrsPrefixed,
		  attrsPrefixedCount, attrFlags, &attrsUnresolved, NULL, NULL, NULL);
	}

	if (!elNmspNameResolved)
	{
		addUniqueNamespace(&state->result, getNamespaceName(elNmspName, elNmspColon));
	}

	if (attrsUnresolved > 0)
	{
		unsigned int j;

		for (j = 0; j < attrsPrefixedCount; j++)
		{
			if (!attrFlags[j])
			{
				XMLNodeHdr	attrNode = attrsPrefixed[j];
				char	   *attrName = XNODE_CONTENT(attrNode);
				char	   *attrNmspColon;

				Assert(attrName[0] != XNODE_CHAR_COLON);
				attrNmspColon = strchr(attrName, XNODE_CHAR_COLON);
				Assert(attrNmspColon != NULL);
				addUniqueNamespace(&state->result, getNamespaceName(attrName, attrNmspColon));
			}
		}
	}

	if (attrCount > 0)
	{
		pfree(attrsPrefixed);
		pfree(attrFlags);
	}
}

/*
 * Turn namespace prefix into NULL-terminated string.
 */
static char *
getNamespaceName(char *name, char *colon)
{
	unsigned int elNmspLen = colon - name;
	char	   *nmspNameCp;

	Assert(elNmspLen > 0);
	nmspNameCp = (char *) palloc(elNmspLen + 1);
	memcpy(nmspNameCp, name, elNmspLen);
	nmspNameCp[elNmspLen] = '\0';
	return nmspNameCp;
}

static void
addUniqueNamespace(XMLNodeContainer result, char *nmspName)
{
	XNodeListItem *item = result->content;
	unsigned int i;

	for (i = 0; i < result->position; i++)
	{
		char	   *nameStored;

		Assert(item->kind == XNODE_LIST_ITEM_SINGLE_PTR);
		nameStored = (char *) item->value.singlePtr;
		if (strcmp(nameStored, nmspName) == 0)
		{
			return;
		}
	}

	/* Not in the container yet, so add it. */
	xmlnodePushSinglePtr(result, (void *) nmspName);
}

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
	if (node->common.kind != XMLNODE_ELEMENT && node->common.kind != XMLNODE_DOC &&
		node->common.kind != XMLNODE_DOC_FRAGMENT && node->common.kind < XNTNODE_ROOT)
	{
		elog(ERROR, "iterator can only be used for compound node. node kind %u was not recognized", node->common.kind);
	}
	iterator->node = node;
	iterator->bwidth = XNODE_GET_REF_BWIDTH(node);
	iterator->childrenLeft = node->children;
	iterator->childOffPtr = XNODE_FIRST_REF(node);
	iterator->attributes = attributes;
}

/*
 * Return the next child of XML element (next in the document order) or NULL if the iteration is finished.
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
	{
		return NULL;
	}
	childOff = readXMLNodeOffset(&iterator->childOffPtr, iterator->bwidth, true);
	childNode = (XMLNodeHdr) ((char *) iterator->node - childOff);

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
	iterator->childrenLeft--;
	return childNode;
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

	if (node->kind == XMLNODE_ELEMENT || node->kind == XMLNODE_DOC ||
		node->kind == XMLNODE_DOC_FRAGMENT)
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
