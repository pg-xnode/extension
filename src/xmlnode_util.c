/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_update.h"

static void xmlTreeWalker(XMLTreeWalkerContext *context);

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
xmlnodePushSingle(XMLNodeContainer cont, XMLNodeOffset singleNode)
{
	XNodeListItem itemNew;

	itemNew.kind = XNODE_LIST_ITEM_SINGLE;
	itemNew.valid = true;
	itemNew.value.single = singleNode;

	xmlnodeAddListItem(cont, &itemNew);
}

void
xmlnodeAddListItem(XMLNodeContainer cont, XNodeListItem *itemNew)
{
	unsigned int pos = cont->position;
	XNodeListItem *item = cont->content + pos;

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

XMLNodeOffset
xmlnodePop(XMLNodeContainer cont)
{

	if (cont->position == 0)
	{
		elog(ERROR, "Stack is empty.");
	}
	cont->position--;
	return cont->content[cont->position].value.single;
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
				char	   *childOffPtr = XNODE_FIRST_REF((XMLCompNodeHdr) node);
				unsigned int children = ((XMLCompNodeHdr) node)->children;
				char		bwidth = XNODE_GET_REF_BWIDTH((XMLCompNodeHdr) node);
				unsigned int i;

				result = sizeof(XMLCompNodeHdrData);
				if (subtree)
				{
					for (i = 0; i < children; i++)
					{
						XMLNodeOffset childOff = readXMLNodeOffset(&childOffPtr, bwidth, true);
						XMLNodeHdr	childNode = (XMLNodeHdr) ((char *) node - childOff);

						result += getXMLNodeSize(childNode, true);
					}
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

	result.maxlen = 32;
	result.data = (char *) palloc(result.maxlen);
	resetStringInfo(&result);

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
 * 'target' - if not NULL, it's assumed that sufficient space is available and the copy is written there.
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
			   *result,
			   *data;
	unsigned int dataLength,
				resultLength;
	XMLNodeOffset *offPtr;

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

	if (root != NULL)
	{
		*root = (char *) node - start;
	}
	end = content + cntLen;

	if (xmlnode)
	{
		dataLength = end - start + sizeof(XMLNodeOffset);
		resultLength = dataLength + VARHDRSZ;
		result = (target != NULL) ? target : (char *) palloc(resultLength);
		data = VARDATA(result);
		offPtr = (XMLNodeOffset *) (result + resultLength - sizeof(XMLNodeOffset));
		*offPtr = (char *) node - start;
		memcpy(data, start, dataLength - sizeof(XMLNodeOffset));
		SET_VARSIZE(result, resultLength);
		return result;
	}
	else
	{
		dataLength = end - start;
		result = (target != NULL) ? target : (char *) palloc(dataLength);
		memcpy(result, start, dataLength);
		return result;
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
char	  **
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

void
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
		XMLNodeHdr	childNode = (XMLNodeHdr) ((char *) (compNode) - readXMLNodeOffset(&firstRef,
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
	unsigned short int i,
				elIndex,
				dtdIndex;
	char	   *refStream = XNODE_FIRST_REF(root);
	unsigned short int elements = 0;
	unsigned short int dtds = 0;

	if (root->common.kind != XMLNODE_DOC && root->common.kind != XMLNODE_DOC_FRAGMENT)
	{
		elog(ERROR, "well-formedness  can't be checked for node type %u", root->common.kind);
	}
	elIndex = 0;
	dtdIndex = 0;
	for (i = 0; i < root->children; i++)
	{
		XMLNodeOffset ref = readXMLNodeOffset(&refStream, XNODE_GET_REF_BWIDTH(root), true);
		XMLNodeHdr	currNode = (XMLNodeHdr) ((char *) root - ref);

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
	}
	if (elements != 1)
	{
		elog(ERROR, "well-formed document must contain exactly one root element");
	}
	if (dtds > 1)
	{
		elog(ERROR, "well-formed document must not contain more than one DTD node");
	}
	if (dtds > 0 && dtdIndex > elIndex)
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

	/*
	 * Set the size to something smaller than what 'initStringInfo()' does
	 */
	si.maxlen = 32;
	si.data = (char *) palloc(si.maxlen);
	resetStringInfo(&si);

	while ((textNode = getNextXMLNode(&textScan, false)) != NULL)
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
walkThroughXMLTree(XMLNodeHdr rootNode, VisitXMLNode visitor, void *userData)
{
	XMLTreeWalkerContext context;

	context.stack = (XMLNodeHdr *) palloc(XMLTREE_STACK_CHUNK * sizeof(XMLNodeHdr));
	context.stackSize = XMLTREE_STACK_CHUNK;
	context.depth = 0;
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
	walkThroughXMLTree(root, visitXMLNodeForDump, (void *) &userData);
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
	unsigned char bwidth;
	char	   *refPtr;
	XMLNodeHdr	child;
	unsigned short i;

	Assert(fragment->common.kind == XMLNODE_DOC_FRAGMENT);
	bwidth = XNODE_GET_REF_BWIDTH(fragment);
	refPtr = XNODE_FIRST_REF(fragment);
	for (i = 0; i < fragment->children; i++)
	{
		child = (XMLNodeHdr) ((char *) fragment - readXMLNodeOffset(&refPtr, bwidth, true));
		if (child->kind == XMLNODE_ATTRIBUTE)
		{
			return true;
		}
	}
	return false;
}

static void
xmlTreeWalker(XMLTreeWalkerContext *context)
{
	XMLNodeHdr	node = context->stack[context->depth];

	context->visitor(context->stack, context->depth, context->userData);

	if (node->kind == XMLNODE_DOC || node->kind == XMLNODE_ELEMENT ||
		node->kind == XMLNODE_DOC_FRAGMENT)
	{

		XMLCompNodeHdr compNode = (XMLCompNodeHdr) node;
		unsigned char bwidth = XNODE_GET_REF_BWIDTH(compNode);
		unsigned short i;
		char	   *refPtr = XNODE_FIRST_REF(compNode);

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
		for (i = 0; i < compNode->children; i++)
		{
			XMLNodeHdr	child = (XMLNodeHdr) ((char *) compNode - readXMLNodeOffset(&refPtr, bwidth, true));

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
