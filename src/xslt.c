/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "template.h"
#include "xml_parser.h"
#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xslt.h"

/*
 * See comments in 'xnt.c' about ordering.
 */
XNodeSpecAttributes xslAttributeInfo[] = {
	/* XSLNODE_SHEET */
	{1, {"version"}, {true}},
	/* XSLNODE_TEMPLATE */
	{1, {"match"}, {false}}
};

/*
 * A single node or subtree of the input XML document to be transformed.
 */
typedef struct XMLNodeToTransform
{
	/* Root of the subtree to be transformed. */
	XMLNodeOffset root;

	/*
	 * The lowest node of the subtree in terms of position in the binary
	 * storage. If the structure points to a single node, it's equal to
	 * 'root'.
	 */
	XMLNodeOffset firstLeaf;

	/*
	 * Template to be applied to the node/subtree.
	 */
	XMLCompNodeHdr template;
} XMLNodeToTransform;

static XMLNodeKind getXSLNodeKind(char *name);
static char *getXSLNodeName(XMLNodeKind kind, char *nmspPrefix);
static void validateXSLTree(XMLNodeHdr root);
static void validateXSLNode(XMLNodeHdr *stack, unsigned int depth, void *userData);
static XMLNodeToTransform *getNodesForTransformation(xmldoc doc,
						  XMLCompNodeHdr sheet, unsigned int *nodeCount);
static void addTemplateContent(XMLNodeContainer resultNodes,
				   XMLCompNodeHdr template, unsigned int *resSizeEstimate);

PG_FUNCTION_INFO_V1(xnode_xsl_in);

Datum
xnode_xsl_in(PG_FUNCTION_ARGS)
{
	XMLParserStateData parserState;
	char	   *input = PG_GETARG_CSTRING(0);
	XMLNodeOffset *rootOffPtr;
	XMLNodeHdr	root;

	if (strlen(input) == 0)
		elog(ERROR, "zero length input string");

	initXMLParserState(&parserState, input, XMLTEMPLATE_ROOT, XSLNODE_NAMESPACE_URI,
					   getXSLNodeKind, getXSLNodeName);

	xmlnodeParseDoc(&parserState);

	rootOffPtr = (XMLNodeOffset *) (parserState.tree + parserState.dstPos - sizeof(XMLNodeOffset));
	root = (XMLNodeHdr) (parserState.tree + *rootOffPtr);
	Assert(root->kind == XMLTEMPLATE_ROOT);
	validateXSLTree(root);

	finalizeXMLParserState(&parserState);
	PG_RETURN_POINTER(parserState.result);
}


PG_FUNCTION_INFO_V1(xnode_xsl_out);

Datum
xnode_xsl_out(PG_FUNCTION_ARGS)
{
	xsl			template = (xsl) PG_GETARG_VARLENA_P(0);
	char	   *data = (char *) VARDATA(template);
	XMLNodeOffset rootNdOff = XNODE_ROOT_OFFSET(template);

	PG_RETURN_CSTRING(dumpXMLNode(data, rootNdOff, VARSIZE(template),
								  XSLNODE_NAMESPACE_URI, getXSLNodeName));
}

PG_FUNCTION_INFO_V1(xsl_transform);

Datum
xsl_transform(PG_FUNCTION_ARGS)
{
	xsl			template = (xsl) PG_GETARG_VARLENA_P(0);
	XMLCompNodeHdr templDocRoot = (XMLCompNodeHdr) XNODE_ROOT(template);
	XMLTemplateHeader templHdr = getXMLTemplateHeader(templDocRoot);
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(1);
	XMLCompNodeHdr docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);
	char	   *docData = VARDATA(doc);
	XMLCompNodeHdr sheetNode;
	unsigned int toTransformCount = 0;
	XMLNodeToTransform *toTransform;
	XMLScanData nodeScan;
	XMLNodeHdr	nodeSrc;
	XNodeInternal treeTransformed;
	unsigned int resSizeEstimate;
	char	   *result = NULL;
	XMLNodeHdr	fragment;
	XMLNodeToTransform *currentTarget = NULL;

	if (templHdr->paramCount > 0)
		elog(ERROR, "this function does not support parameterized stylesheets");

	sheetNode = getXMLDocRootElement(templDocRoot, XSLNODE_SHEET);

	if (sheetNode == NULL)

		/*
		 * Such a template document shouldn't get through validation, but
		 * check nevertheless.
		 */
		elog(ERROR, "sheet node not found");

	toTransform = getNodesForTransformation(doc, sheetNode, &toTransformCount);

	/* varlena header + root node offset */
	resSizeEstimate = VARHDRSZ + MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET) + sizeof(XMLNodeOffset);

	treeTransformed = (XNodeInternal) palloc(sizeof(XNodeInternalData));
	treeTransformed->copy = false;
	treeTransformed->node = NULL;

	/*
	 * Node kind is the only information we need to pass to the writer
	 * utility. The other should be ignored downstream. That's why the node
	 * doesn't even to be compound (no need to add unnecessary casts).
	 */
	fragment = (XMLNodeHdr) palloc(sizeof(XMLNodeHdrData));
	fragment->kind = XMLNODE_DOC_FRAGMENT;
	treeTransformed->node = fragment;
	treeTransformed->copy = true;
	xmlnodeContainerInit(&treeTransformed->children);

	/*
	 * The default logic of the transformation is to convert the whole input
	 * document to text (i.e. remove all markup).
	 */
	initScanForSingleXMLNodeKind(&nodeScan, docRoot, XMLNODE_NODE);
	while ((nodeSrc = getNextXMLNode(&nodeScan)) != NULL)
	{
		char	   *text;
		XMLNodeHdr	nodeCopy;
		unsigned int copySize = sizeof(XMLNodeHdrData) + 1;
		XNodeInternal nodeInt;

		Assert(nodeSrc->kind != XMLNODE_ATTRIBUTE);
		Assert(nodeSrc->kind != XMLNODE_DOC);

		if (toTransformCount > 0)
		{
			XMLNodeOffset srcNdOff;
			unsigned int i;

			srcNdOff = (XMLNodeOffset) ((char *) nodeSrc - docData);

			if (currentTarget != NULL && srcNdOff > currentTarget->root)
				/* The set of nodes to be replaced by template is over. */
				currentTarget = NULL;

			if (currentTarget == NULL)
			{
				/* Does another template affect this node? */
				for (i = 0; i < toTransformCount; i++)
				{
					XMLNodeToTransform *toTransfTmp = &toTransform[i];

					if (srcNdOff <= toTransfTmp->root &&
						srcNdOff >= toTransfTmp->firstLeaf)
					{
						/*
						 * The node is subject to transformation. Instead of
						 * copying it from the source document we must receive
						 * it from the template.
						 */
						currentTarget = toTransfTmp;
						break;
					}
				}
			}

			if (currentTarget != NULL)
			{
				Assert(srcNdOff >= currentTarget->firstLeaf &&
					   srcNdOff <= currentTarget->root);

				if (srcNdOff == currentTarget->firstLeaf)
				{
					/*
					 * The first node of the current template's range. Content
					 * of the template must be applied right now.
					 *
					 * If the following source node(s) will fall into the
					 * range of the current template, we'll just skip them.
					 */
					addTemplateContent(&treeTransformed->children,
								  currentTarget->template, &resSizeEstimate);
				}
				continue;
			}
		}

		if (nodeSrc->kind != XMLNODE_CDATA && nodeSrc->kind != XMLNODE_TEXT)
			continue;

		text = XNODE_CONTENT(nodeSrc);

		copySize += strlen(text);
		nodeCopy = (XMLNodeHdr) palloc(copySize);
		nodeCopy->kind = XMLNODE_TEXT;
		nodeCopy->flags = nodeSrc->flags;
		strcpy(XNODE_CONTENT(nodeCopy), text);

		nodeInt = (XNodeInternal) palloc(sizeof(XNodeInternalData));
		nodeInt->node = nodeCopy;
		nodeInt->copy = true;
		nodeInt->children.content = NULL;
		nodeInt->children.position = 0;

		xmlnodePushSinglePtr(&treeTransformed->children, nodeInt);
		/* No padding for simple node. */
		resSizeEstimate += copySize;
	}
	finalizeScanForSingleXMLNodeKind(&nodeScan);

	if (treeTransformed->children.position > 0)
	{
		unsigned int children = treeTransformed->children.position;
		unsigned int resultSize;
		char	   *resTmp;
		XMLNodeOffset offRoot = 0;
		XMLNodeOffset *offRootPtr;

		if (children > 1)
			/* Document fragment node: the node itself + references.  */
			resSizeEstimate += MAX_PADDING(XNODE_ALIGNOF_COMPNODE) +
				sizeof(XMLCompNodeHdrData) + children * sizeof(XMLNodeOffset);

		result = (char *) palloc(resSizeEstimate);
		resTmp = result + VARHDRSZ;

		/*
		 * If the output structure contains exactly one nod
		 */
		if (children == 1)
			writeXMLNodeInternal(treeTransformed->children.content->value.singlePtr,
								 true, &resTmp, &offRoot);
		else
			writeXMLNodeInternal(treeTransformed, true, &resTmp, &offRoot);

		resTmp = (char *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, resTmp);
		offRootPtr = (XMLNodeOffset *) resTmp;
		*offRootPtr = offRoot;
		resTmp += sizeof(XMLNodeOffset);
		resultSize = (char *) resTmp - result;
		SET_VARSIZE(result, resultSize);
	}

	freeXMLNodeInternal(treeTransformed);
	if (toTransform != NULL)
		pfree(toTransform);

	if (result != NULL)
		PG_RETURN_POINTER(result);
	else
		PG_RETURN_NULL();
}

static XMLNodeKind
getXSLNodeKind(char *name)
{
	char	   *colon = strrchr(name, XNODE_CHAR_COLON);

	if (colon != NULL)
		/* skip the prefix */
		name = colon + 1;

	if (strcmp(name, XSL_SHEET) == 0)
		return XSLNODE_SHEET;
	else if (strcmp(name, XSL_TEMPLATE) == 0)
		return XSLNODE_TEMPLATE;

	elog(ERROR, "unrecognized XSL node '%s'", name);
	return 0;
}

static char *
getXSLNodeName(XMLNodeKind kind, char *nmspPrefix)
{
	StringInfoData out;
	char	   *name;

	xnodeInitStringInfo(&out, 32);

	switch (kind)
	{
		case XSLNODE_SHEET:
			name = XSL_SHEET;
			break;

		case XSLNODE_TEMPLATE:
			name = XSL_TEMPLATE;
			break;

		default:
			elog(ERROR, "unrecognized xnt node kind %u", kind);
			return NULL;
	}

	if (nmspPrefix != NULL && strlen(nmspPrefix) > 0)
	{
		appendStringInfo(&out, "%s:", nmspPrefix);
	}
	appendStringInfoString(&out, name);
	return out.data;
}

static void
validateXSLTree(XMLNodeHdr root)
{
	bool		hasSheet = false;

	walkThroughXMLTree(root, validateXSLNode, false, (void *) &hasSheet);
	if (!hasSheet)
		elog(ERROR, "valid stylesheet (bound to the correct namespace) must be the root element");
}

static void
validateXSLNode(XMLNodeHdr *stack, unsigned int depth, void *userData)
{
	XMLNodeHdr	node = stack[depth];
	XMLNodeHdr	parent = NULL;

	if (depth >= 1)
	{
		parent = stack[depth - 1];
	}

	switch (node->kind)
	{
		case XMLNODE_CDATA:
		case XMLNODE_TEXT:
			if (parent != NULL && parent->kind == XSLNODE_SHEET)
			{
				char	   *content = XNODE_CONTENT(node);

				if (!xmlStringWhitespaceOnly(content))
					elog(ERROR, "text node child of the stylesheet may only be a whitespace");
			}
			break;

		case XSLNODE_SHEET:
			if (depth == 1)
			{
				XMLNodeIteratorData iterator;
				XMLNodeHdr	attrNode;
				char	   *version;
				bool	   *hasSheet = (bool *) userData;

				*hasSheet = true;

				initXMLNodeIteratorSpecial(&iterator, (XMLCompNodeHdr) node, true);
				attrNode = getNextXMLNodeChild(&iterator);
				version = XNODE_CONTENT(attrNode);
				if (strcmp(version, XSL_VERSION_STR) != 0)
					elog(ERROR, "only XSL version %s supported", XSL_VERSION_STR);
				return;
			}
			else
				elog(ERROR, "'stylesheet' element is only allowed as the root element");

			break;

		case XSLNODE_TEMPLATE:
			if (parent != NULL && parent->kind != XSLNODE_SHEET)
				elog(ERROR, "XSL template may only be child of stylesheet");
			break;

		default:
			if (parent != NULL && parent->kind == XSLNODE_SHEET &&
				node->kind != XMLNODE_COMMENT)
				elog(ERROR, "%s must not be a child of xsl stylesheet", getXMLNodeKindStr(node->kind));
			break;
	}

}

/*
 * Evaluates 'match' attribute of each template that 'sheet' contains.
 * Returns array of 'XMLNodeToTransform' instances, representing every
 * *unique* matching node of 'doc'.
 *
 * 'nodeCount' receives size of the array.
 *
 * If the new node matches multiple templates, (only) the latest template
 * (in terms of order within the sheet) is effective.
 */
static XMLNodeToTransform *
getNodesForTransformation(xmldoc doc,
						  XMLCompNodeHdr sheet, unsigned int *nodeCount)
{
	XMLNodeIteratorData sheetIterator;
	XMLNodeHdr	sheetChild;
	unsigned int templateCount = 0;
	XMLNodeContainerData preResult;
	char	   *docData = VARDATA(doc);
	XMLNodeToTransform *result = NULL;
	unsigned int resNodes = 0;

	xmlnodeContainerInit(&preResult);

	initXMLNodeIteratorSpecial(&sheetIterator, sheet, false);
	while ((sheetChild = getNextXMLNodeChild(&sheetIterator)) != NULL)
	{
		if (sheetChild->kind == XSLNODE_TEMPLATE)
		{
			XMLCompNodeHdr templateNode,
						docRoot;
			XMLNodeHdr	matchNode,
						sourceNode;
			XPathHeader xpHdr;
			XPathExpression expr;
			XPath		locPath;
			XMLScanData xscan;

			if (templateCount > XSLT_MAX_TEMPLATES_PER_SHEET)
				elog(ERROR, "maximum number of templates per sheet is %u",
					 XSLT_MAX_TEMPLATES_PER_SHEET);

			templateNode = (XMLCompNodeHdr) sheetChild;
			docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);

			matchNode = getSpecialXMLNodeAttribute(templateNode, XSL_TEMPLATE_MATCH);
			Assert(matchNode->kind == XMLNODE_ATTRIBUTE);
			Assert(matchNode->flags & XNODE_ATTR_VALUE_BINARY);

			xpHdr = (XPathHeader) XNODE_CONTENT(matchNode);
			expr = getXPathExpressionFromStorage(xpHdr);
			locPath = getSingleXPath(expr, xpHdr);

			/* Find all nodes matching the current template. */
			initXMLScan(&xscan, NULL, locPath, xpHdr, docRoot, doc, locPath->descendants > 0);
			while ((sourceNode = getNextXMLNode(&xscan)) != NULL)
			{
				unsigned int i;
				XNodeListItem *item;
				bool		srcNodeResolved = false;

				for (i = 0; i < preResult.position; i++)
				{
					XMLNodeToTransform *transfTarget;
					XMLNodeHdr	transfTargNode;

					item = preResult.content + i;
					if (item->value.singlePtr == NULL)
						/* Node invalidated by another one. */
						continue;

					transfTarget = (XMLNodeToTransform *) item->value.singlePtr;
					transfTargNode = (XMLNodeHdr) (docData + transfTarget->root);

					if (sourceNode == transfTargNode)
					{
						/*
						 * The node was already selected, but using another
						 * template. The later template should win.
						 */
						transfTarget->template = templateNode;
						srcNodeResolved = true;
						break;
					}

					if (isXMLNodeDescendant(sourceNode, (XMLCompNodeHdr) transfTargNode))
					{
						/*
						 * Some other template already selected ascendant of
						 * this node and will replace the whole subtree.
						 */
						srcNodeResolved = true;
						break;
					}

					if (XNODE_IS_COMPOUND(sourceNode) &&
						isXMLNodeDescendant((XMLNodeHdr) transfTargNode,
											(XMLCompNodeHdr) sourceNode))
					{
						/*
						 * 'transfTargNode' selected earlier by another
						 * template but now becomes descendant of the current
						 * template's target.
						 */
						pfree(transfTarget);
						item->value.singlePtr = NULL;
						resNodes--;
					}
				}

				if (!srcNodeResolved)
				{
					XMLNodeToTransform *targNew;
					XMLNodeHdr	firstLeaf;

					/*
					 * The node needs to be transformed (unless some later
					 * template covers it or its ascendant), so add it to the
					 * list.
					 */
					firstLeaf = XNODE_IS_COMPOUND(sourceNode) ?
						getFirstXMLNodeLeaf((XMLCompNodeHdr) sourceNode) :
						sourceNode;

					targNew = (XMLNodeToTransform *) palloc(sizeof(XMLNodeToTransform));
					targNew->root = (XMLNodeOffset) ((char *) sourceNode - docData);
					targNew->firstLeaf = (XMLNodeOffset) ((char *) firstLeaf - docData);
					targNew->template = templateNode;
					xmlnodePushSinglePtr(&preResult, targNew);
					resNodes++;
				}
			}
			templateCount++;
		}
	}

	if (resNodes > 0)
	{
		/* Copy all valid target nodes/subtrees to array. */
		XNodeListItem *item = preResult.content;
		unsigned int i,
					j;

		result = (XMLNodeToTransform *) palloc(resNodes * sizeof(XMLNodeToTransform));

		j = 0;
		for (i = 0; i < resNodes; i++)
		{
			XMLNodeToTransform *node = item->value.singlePtr;

			if (node != NULL)
			{
				memcpy(&result[j++], node, sizeof(XMLNodeToTransform));
				pfree(node);
			}
			item++;
		}
	}

	xmlnodeContainerFree(&preResult);
	*nodeCount = resNodes;
	return result;
}

static void
addTemplateContent(XMLNodeContainer resultNodes,
				   XMLCompNodeHdr template, unsigned int *resSizeEstimate)
{
	XMLNodeIteratorData iterator;
	XMLNodeHdr	templateContent;

	initXMLNodeIteratorSpecial(&iterator, template, false);
	while ((templateContent = getNextXMLNodeChild(&iterator)) != NULL)
	{
		XNodeInternal nodeInt;
		XMLNodeKind kind = templateContent->kind;

		if (kind == XMLNODE_ATTRIBUTE)
			/* The template might have namespace declaration. */
			continue;

		if (kind == XMLNODE_COMMENT)
			continue;

		if (kind == XMLNODE_TEXT || kind == XMLNODE_CDATA)
		{
			char	   *str = XNODE_CONTENT(templateContent);

			if (xmlStringWhitespaceOnly(str))
				continue;
		}

		nodeInt = (XNodeInternal) palloc(sizeof(XNodeInternalData));
		nodeInt->node = templateContent;
		nodeInt->copy = false;
		nodeInt->children.content = NULL;
		nodeInt->children.position = 0;
		xmlnodePushSinglePtr(resultNodes, nodeInt);

		*resSizeEstimate += getXMLNodeSize(templateContent, true);
		if (XNODE_IS_COMPOUND(templateContent))
		{
			*resSizeEstimate += MAX_PADDING(XNODE_ALIGNOF_COMPNODE);
		}
	}
}
