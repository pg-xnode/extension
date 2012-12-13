/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "template.h"
#include "xmlnode.h"
#include "xml_parser.h"
#include "xmlnode_util.h"
#include "xnt.h"

/*
 * 1. Order must exactly follow that of XNT nodes in 'XMLNodeKind' enumeration.
 * 2. For each element the required attributes must be at lower positions than
 * the optional. (This restriction only applies to 'xmlAttributInfo', not to
 * the actual input document.)
 */
XNodeSpecAttributes xntAttributeInfo[] = {
	/* XNTNODE_TEMPLATE */
	{1, {"preserve-space"}, {false}},
	/* XNTNODE_COPY_OF */
	{1, {"expr"}, {true}},
	/* XNTNODE_ELEMENT */
	{1, {"name"}, {true}},
	/* XNTNODE_ATTRIBUTE */
	{2, {"name", "value"}, {true, true}}
};

static XMLNodeKind getXNTNodeKind(char *name);
static char *getXNTNodeName(XMLNodeKind kind, char *nmspPrefix);
static void validateXNTTree(XMLNodeHdr root);
static void validateXNTNode(XMLNodeHdr *stack, unsigned int depth, void *userData);
static void buildNewNodeTree(XMLNodeHdr node, XNodeInternal parent, unsigned int *storageSize,
				 XPathExprOperandValue paramValues, unsigned short *paramMap, XPathExprState exprState,
				 bool preserveSpace);
static XNodeInternal getInternalNode(XMLNodeHdr node, bool copy);

static XNodeInternal xntProcessCopyOf(XMLNodeHdr node, XNodeInternal parent,
				 bool preserveSpace, XPathExprOperandValue paramValues,
				 unsigned short *paramMap, XPathExprState exprState,
				 unsigned int *storageSize);
static XNodeInternal xntProcessElement(XMLNodeHdr node,
				  bool preserveSpace, XPathExprOperandValue paramValues,
				  unsigned short *paramMap, XPathExprState exprState,
				  unsigned int *storageSize);
static XNodeInternal xntProcessAttribute(XMLNodeHdr node,
				 XPathExprOperandValue paramValues, unsigned short *paramMap,
					XPathExprState exprState, unsigned int *storageSize);


PG_FUNCTION_INFO_V1(xnode_template_in);

Datum
xnode_template_in(PG_FUNCTION_ARGS)
{
	XMLParserStateData parserState;
	char	   *input = PG_GETARG_CSTRING(0);
	XMLNodeOffset *rootOffPtr;
	XMLNodeHdr	root;

	if (strlen(input) == 0)
	{
		elog(ERROR, "zero length input string");
	}
	initXMLParserState(&parserState, input, XMLTEMPLATE_ROOT, XNTNODE_NAMESPACE_URI,
					   getXNTNodeKind, getXNTNodeName);
	xmlnodeParseDoc(&parserState);

	rootOffPtr = (XMLNodeOffset *) (parserState.tree + parserState.dstPos - sizeof(XMLNodeOffset));
	root = (XMLNodeHdr) (parserState.tree + *rootOffPtr);
	Assert(root->kind == XMLTEMPLATE_ROOT);
	validateXNTTree(root);

	finalizeXMLParserState(&parserState);
	PG_RETURN_POINTER(parserState.result);
}

PG_FUNCTION_INFO_V1(xnode_template_out);

Datum
xnode_template_out(PG_FUNCTION_ARGS)
{
	xnt			template = (xnt) PG_GETARG_VARLENA_P(0);
	char	   *data = (char *) VARDATA(template);
	XMLNodeOffset rootNdOff = XNODE_ROOT_OFFSET(template);

	PG_RETURN_CSTRING(dumpXMLNode(data, rootNdOff, VARSIZE(template),
								  XNTNODE_NAMESPACE_URI, getXNTNodeName));
}


PG_FUNCTION_INFO_V1(xnode_from_template);

Datum
xnode_from_template(PG_FUNCTION_ARGS)
{
	xnt			template;
	XMLCompNodeHdr templDocRoot,
				templNode;
	char	   *cursor;
	char	  **templParNames = NULL;
	XMLTemplateHeader templHdr;
	ArrayType  *parNameArr;
	int			parNameCount = 0;
	XMLParamNameSorted *parNames = NULL;
	Datum		row;
	XPathExprOperandValue parValues = NULL;
	unsigned short *paramMap = NULL;
	XNodeInternal newTreeRoot = NULL;
	XNodeInternal templRootInternal;
	unsigned int resSizeEstimate = 0;
	char	   *result = NULL;
	XMLNodeOffset offRoot = 0;
	XPathExprState exprState = NULL;

	/* Retrieve parameter names out of the template. */
	template = (xnt) PG_GETARG_VARLENA_P(0);
	templDocRoot = (XMLCompNodeHdr) XNODE_ROOT(template);
	templHdr = getXMLTemplateHeader(templDocRoot);

	cursor = (char *) templHdr;
	if (templHdr->paramCount > 0 || templHdr->substNodesCount > 0)
		cursor += sizeof(XMLTemplateHeaderData);

	if (templHdr->paramCount > 0)
	{
		unsigned short i;

		templParNames = (char **) palloc(templHdr->paramCount * sizeof(char *));

		for (i = 0; i < templHdr->paramCount; i++)
		{
			templParNames[i] = cursor;
			cursor += strlen(cursor) + 1;
		}
	}

	/* Retrieve names of the attributes that the function caller provides. */
	parNameArr = PG_GETARG_ARRAYTYPE_P(1);
	paramMap = (unsigned short *) palloc(templHdr->paramCount * sizeof(unsigned short));
	parNames = getXMLTemplateParamNames(parNameArr, templHdr->paramCount, templParNames, paramMap);

	/*
	 * More than the default size because it will be used for all expressions
	 * in the template. (We can't free the cache when going to process another
	 * expression because parameters have to be available for all.)
	 */
	exprState = createXPathVarCache(4 * XPATH_VAR_CACHE_DEF_SIZE);

	/* Retrieve values of the parameters. */
	row = PG_GETARG_DATUM(2);
	parValues = getXMLTemplateParamValues(row, templHdr->paramCount, exprState, fcinfo->flinfo->fn_oid);


	templNode = getXMLDocRootElement(templDocRoot, XNTNODE_TEMPLATE);

	newTreeRoot = (XNodeInternal) palloc(sizeof(XNodeInternalData));
	newTreeRoot->node = (XMLNodeHdr) templNode;
	newTreeRoot->copy = false;
	xmlnodeContainerInit(&newTreeRoot->children);

	buildNewNodeTree((XMLNodeHdr) templNode, newTreeRoot, &resSizeEstimate,
					 parValues, paramMap, exprState, false);

	/* Template node is the only child. */
	Assert(newTreeRoot->children.position == 1);

	templRootInternal = (XNodeInternal) newTreeRoot->children.content->value.singlePtr;

	/*
	 * Empty template is not valid at parse time but the resulting document
	 * might get empty yet. For example whitespace-only text nodes or comments
	 * don't find their way to the result.
	 */
	if (templRootInternal->children.position > 0)
	{
		unsigned int resultSize;
		char	   *resTmp;
		XMLNodeOffset *offRootPtr;

		/* varlena header + root node offset */
		resSizeEstimate += VARHDRSZ + MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET) + sizeof(XMLNodeOffset);
		if (templRootInternal->children.position > 1)
		{
			/* Document fragment node in addition. */
			resSizeEstimate += (XNODE_ALIGNOF_COMPNODE - 1) + sizeof(XMLCompNodeHdrData) +
				templNode->children * sizeof(XMLNodeOffset);
		}
		result = (char *) palloc(resSizeEstimate);

		resTmp = result + VARHDRSZ;

		if (templRootInternal->children.position == 1)
		{
			writeXMLNodeInternal(templRootInternal->children.content->value.singlePtr, true, &resTmp, &offRoot);
		}
		else
		{
			/*
			 * The template will be written too in addition to its children
			 * and finally turned into a document fragment.
			 */
			writeXMLNodeInternal(templRootInternal, true, &resTmp, &offRoot);
		}

		resTmp = (char *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, resTmp);
		offRootPtr = (XMLNodeOffset *) resTmp;
		*offRootPtr = offRoot;
		resTmp += sizeof(XMLNodeOffset);
		resultSize = (char *) resTmp - result;
		SET_VARSIZE(result, resultSize);
	}

	freeXMLNodeInternal(newTreeRoot);
	freeExpressionState(exprState);

	if (paramMap != NULL)
	{
		pfree(paramMap);
	}
	if (parNames != NULL)
	{
		unsigned short i;

		for (i = 0; i < parNameCount; i++)
		{
			pfree(parNames[i].name);
		}
		pfree(parNames);
	}
	if (templParNames != NULL)
	{
		pfree(templParNames);
	}
	if (parValues != NULL)
	{
		pfree(parValues);
	}

	if (result != NULL)
	{
		PG_RETURN_POINTER(result);
	}
	else
	{
		PG_RETURN_NULL();
	}
}

static XMLNodeKind
getXNTNodeKind(char *name)
{
	char	   *colon = strrchr(name, XNODE_CHAR_COLON);

	if (colon != NULL)
	{
		/* skip the prefix */
		name = colon + 1;
	}

	if (strcmp(name, XNT_TEMPLATE) == 0)
	{
		return XNTNODE_TEMPLATE;
	}
	else if (strcmp(name, XNT_COPY_OF) == 0)
	{
		return XNTNODE_COPY_OF;
	}
	else if (strcmp(name, XNT_ELEMENT) == 0)
	{
		return XNTNODE_ELEMENT;
	}
	else if (strcmp(name, XNT_ATTRIBUTE) == 0)
	{
		return XNTNODE_ATTRIBUTE;
	}
	elog(ERROR, "unrecognized XNT node '%s'", name);
	return 0;
}

static char *
getXNTNodeName(XMLNodeKind kind, char *nmspPrefix)
{
	StringInfoData out;
	char	   *name;

	xnodeInitStringInfo(&out, 32);

	switch (kind)
	{
		case XNTNODE_TEMPLATE:
			name = XNT_TEMPLATE;
			break;

		case XNTNODE_COPY_OF:
			name = XNT_COPY_OF;
			break;

		case XNTNODE_ELEMENT:
			name = XNT_ELEMENT;
			break;

		case XNTNODE_ATTRIBUTE:
			name = XNT_ATTRIBUTE;
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
validateXNTTree(XMLNodeHdr root)
{
	bool		hasRootTemplate = false;

	walkThroughXMLTree(root, validateXNTNode, false, (void *) &hasRootTemplate);
	if (!hasRootTemplate)
	{
		elog(ERROR, "valid template (bound to the correct namespace) must be the root element");
	}
}

static void
validateXNTNode(XMLNodeHdr *stack, unsigned int depth, void *userData)
{
	XMLNodeHdr	node = stack[depth];
	XMLNodeHdr	parent = NULL;

	if (node->kind < XNTNODE_TEMPLATE)
		return;

	if (depth >= 1)
	{
		parent = stack[depth - 1];
	}

	switch (node->kind)
	{
		case XNTNODE_TEMPLATE:
			if (depth == 1)
			{
				bool	   *hasRootTemplate = (bool *) userData;

				if (node->flags & XNODE_EMPTY)
				{
					elog(ERROR, "root template must not be empty");
				}
				*hasRootTemplate = true;
				return;
			}
			else
			{
				elog(ERROR, "'xnt:template' element is only allowed as the root element");
			}
			break;

		case XNTNODE_COPY_OF:
			if (!(node->flags & XNODE_EMPTY))
			{
				elog(ERROR, "'xnt:copy-of' element must be empty");
			}
			break;

		case XNTNODE_ELEMENT:
			break;

		case XNTNODE_ATTRIBUTE:
			if (!(parent != NULL && parent->kind == XNTNODE_ELEMENT))
			{
				elog(ERROR, "'xnt:element' must be parent of 'xnt:attribute'");
			}

			if (!(node->flags & XNODE_EMPTY))
			{
				/*
				 * It doesn't seem to bring additional value if node content
				 * could represent the attribute value, for example:
				 *
				 * <xnt:attribute name="x">1</xnt:attribute>
				 */
				elog(ERROR, "'xnt:attribute' element must be empty");
			}
			break;

		default:
			elog(ERROR, "unrecognized node kind %u found during validation", node->kind);
			break;
	}
}

/*
 * Generates the template tree with parameters substituted.
 *
 * XNTNODE_TEMPLATE must be the root for the outermost call.
 *
 *	'node' - template node to be processed.
 *
 *	'parent' - the new (in-memory) node will be child of this
 *
 *	'storageSize' gets incremented by the storage size required for the current
 *	node plus that of children. For template only children and the corresponding
 *	references are accounted, while the template node itself is ignored.
 *
 *	'paramValues', 'paramMap' and 'exprState' - data to evaluate expression if
 *	the node contains some.
 *
 *	'preserveSpace' - if 'true' then text node is copied to the resulting
 *	(in-memory) node. Otherwise text node is ignored.
 *
 */
static void
buildNewNodeTree(XMLNodeHdr node, XNodeInternal parent, unsigned int *storageSize,
				 XPathExprOperandValue paramValues, unsigned short *paramMap, XPathExprState exprState,
				 bool preserveSpace)
{
	XNodeInternal nodeInternal = NULL;

	/* Process children if the node has some. */
	if (node->kind == XMLNODE_ELEMENT || node->kind == XNTNODE_TEMPLATE)
	{
		XMLCompNodeHdr compNode = (XMLCompNodeHdr) node;
		XMLNodeIteratorData iterator;
		XMLNodeHdr	childNode;
		unsigned int children = 0;

		nodeInternal = getInternalNode(node, false);

		xmlnodeContainerInit(&nodeInternal->children);

		if (node->kind == XMLNODE_ELEMENT)
			initXMLNodeIterator(&iterator, compNode, true);
		else
			initXMLNodeIteratorSpecial(&iterator, compNode, true);

		while ((childNode = getNextXMLNodeChild(&iterator)) != NULL)
		{
			children++;

			if (node->kind == XNTNODE_TEMPLATE)
			{
				XNodeSpecAttributes *attrInfo = xntAttributeInfo;
				XMLNodeOffset childRef;

				if (children <= attrInfo->number)
				{
					childRef = (char *) node - (char *) childNode;

					if (childRef == XMLNodeOffsetInvalid)
						/* Unused optional attribute. */
						continue;

					if ((children - 1) == XNT_TEMPLATE_PRESERVE)
					{
						char	   *valueStr = XNODE_CONTENT(childNode);
						Datum		boolDatum = DirectFunctionCall1Coll(boolin, InvalidOid, CStringGetDatum(valueStr));

						preserveSpace = DatumGetBool(boolDatum);
						continue;
					}
				}
				else if (childNode->kind == XMLNODE_ATTRIBUTE)
					/* Only namespace declaration should make this happen. */
					continue;
			}

			if ((childNode->kind == XMLNODE_TEXT || childNode->kind == XMLNODE_CDATA) &&
				!preserveSpace && xmlStringWhitespaceOnly(XNODE_CONTENT(childNode)))
			{
				continue;
			}

			if (childNode->kind == XMLNODE_COMMENT)
			{
				continue;
			}

			buildNewNodeTree(childNode, nodeInternal, storageSize, paramValues, paramMap, exprState, preserveSpace);
		}
	}

	if (node->flags & XNODE_EL_SPECIAL)
	{
		/*
		 * The resulting node (meaning storage) will not contain the template
		 * node itself, so we don't have to process (construct) it, neither
		 * evaluate its size.
		 */
		if (node->kind != XNTNODE_TEMPLATE)
		{
			switch (node->kind)
			{
				case XNTNODE_COPY_OF:
					nodeInternal = xntProcessCopyOf(node, parent, preserveSpace, paramValues,
										   paramMap, exprState, storageSize);
					break;

				case XNTNODE_ELEMENT:
					nodeInternal = xntProcessElement(node, preserveSpace, paramValues,
										   paramMap, exprState, storageSize);
					break;

				case XNTNODE_ATTRIBUTE:
					nodeInternal = xntProcessAttribute(node, paramValues, paramMap,
													 exprState, storageSize);
					break;

				default:
					elog(ERROR, "unrecognized special node %u", node->kind);
					break;
			}
		}
	}
	else
	{							/* Not a special) element. */

		if (node->kind == XMLNODE_ELEMENT)
		{
			char	   *name = XNODE_ELEMENT_NAME((XMLCompNodeHdr) node);

			/*
			 * getXMLNodeSize() is not used here because child references
			 * might need more space in the new tree. When constructing the
			 * node from template we treat the references separate, see above.
			 */
			*storageSize += MAX_PADDING(XNODE_ALIGNOF_COMPNODE) +
				sizeof(XMLCompNodeHdrData) + strlen(name) +1;
		}
		else if (node->kind == XMLNODE_ATTRIBUTE && (node->flags & XNODE_ATTR_VALUE_BINARY))
		{
			XMLNodeHdr	attrOrig,
						attrNew;
			char	   *name,
					   *valueOrig,
					   *valueNew;
			unsigned int sizeNew = 0;

			/* Construct attribute as 2 NULL-terminated strings (name, value). */
			attrOrig = node;
			name = XNODE_CONTENT(attrOrig);
			valueOrig = name + strlen(name) + 1;
			valueNew = dumpXMLAttrBinaryValue(valueOrig, NULL, paramValues, paramMap, exprState);
			attrNew = getNewXMLAttribute(name, attrOrig->flags, valueNew, false, &sizeNew);
			nodeInternal = getInternalNode(attrNew, true);
			pfree(valueNew);
			*storageSize += sizeNew;
		}
		else
		{
			nodeInternal = getInternalNode(node, false);
			*storageSize += getXMLNodeSize(node, false);
		}

		*storageSize += sizeof(XMLNodeOffset);
	}

	if (nodeInternal != NULL)
	{
		xmlnodePushSinglePtr(&parent->children, nodeInternal);
	}
}


static XNodeInternal
getInternalNode(XMLNodeHdr node, bool copy)
{
	XNodeInternal result = (XNodeInternal) palloc(sizeof(XNodeInternalData));

	result->node = node;
	result->copy = copy;
	return result;
}

static XNodeInternal
xntProcessCopyOf(XMLNodeHdr node, XNodeInternal parent,
				 bool preserveSpace, XPathExprOperandValue paramValues,
				 unsigned short *paramMap, XPathExprState exprState,
				 unsigned int *storageSize)
{
	XMLCompNodeHdr xntNode;
	XPathHeader xpHdr;
	XPathExpression expr,
				exprCopy;
	XPathExprOperandValueData exprResult;
	XMLNodeHdr	attrNode,
				resultNode;
	unsigned int resultNodeSize;
	XNodeInternal resultInternal = NULL;

	xntNode = (XMLCompNodeHdr) node;
	attrNode = getSpecialXMLNodeAttribute(xntNode, XNT_COPY_OF_EXPR);

	Assert(attrNode->kind == XMLNODE_ATTRIBUTE);
	Assert(attrNode->flags & XNODE_ATTR_VALUE_BINARY);

	/*
	 * Special attributes only contain the value. (Name is determined by the
	 * position.)
	 */
	xpHdr = (XPathHeader) XNODE_CONTENT(attrNode);
	expr = getXPathExpressionFromStorage(xpHdr);
	exprCopy = substituteXMLTemplateParams(exprState, expr, paramValues, paramMap);
	evaluateXPathExpression(exprState, exprCopy, 0, &exprResult);

	if (!exprResult.isNull)
	{
		if (exprResult.type == XPATH_VAL_NODESET)
		{
			XPathNodeSet ns = &exprResult.v.nodeSet;
			XMLNodeHdr *nodes;
			unsigned short j;

			Assert(ns->count > 0);

			if (ns->count == 1)
			{
				XMLNodeHdr	single = getXPathOperandValue(exprState, ns->nodes.nodeId, XPATH_VAR_NODE_SINGLE);

				nodes = &single;
			}
			else
			{
				nodes = (XMLNodeHdr *) getXPathOperandValue(exprState, ns->nodes.arrayId, XPATH_VAR_NODE_ARRAY);
			}

			for (j = 0; j < ns->count; j++)
			{
				XMLNodeHdr	singleNode = nodes[j];

				/*
				 * Fragment must have been turned into node-set during
				 * parameter substitution.
				 */
				Assert(singleNode->kind != XMLNODE_DOC_FRAGMENT);

				if ((singleNode->kind == XMLNODE_TEXT && xmlStringWhitespaceOnly(XNODE_CONTENT(singleNode))) ||
					singleNode->kind == XMLNODE_COMMENT)
				{
					continue;
				}

				/*
				 * No need to initialize 'nodeInternal'. We're only interested
				 * in the child node.
				 */
				buildNewNodeTree(singleNode, parent, storageSize, paramValues, paramMap, exprState,
								 preserveSpace);
			}
		}
		else
		{
			XPathExprOperandValueData resStr;
			char	   *strValue,
					   *content;

			castXPathExprOperandToStr(exprState, &exprResult, &resStr);
			strValue = getXPathOperandValue(exprState, resStr.v.stringId, XPATH_VAR_STRING);

			resultNodeSize = sizeof(XMLNodeHdr) + strlen(strValue) +1;
			resultNode = (XMLNodeHdr) palloc(resultNodeSize);
			resultNode->kind = XMLNODE_TEXT;

			/*
			 * TODO 1. Ensure there are no invalid chars 2. Some flags might
			 * need to be set, especially XNODE_TEXT_SPEC_CHARS
			 */
			resultNode->flags = 0;

			content = XNODE_CONTENT(resultNode);
			strcpy(content, strValue);

			resultInternal = getInternalNode(resultNode, true);
			*storageSize += resultNodeSize + sizeof(XMLNodeOffset);
		}
	}
	pfree(exprCopy);

	return resultInternal;
}

static XNodeInternal
xntProcessElement(XMLNodeHdr node, bool preserveSpace,
				  XPathExprOperandValue paramValues, unsigned short *paramMap,
				  XPathExprState exprState, unsigned int *storageSize)
{
	XMLCompNodeHdr xntNode;
	XMLNodeHdr	attrNode,
				childNode,
				resultNode;
	unsigned int resultNodeSize;
	char	   *elName;
	bool		elNameCopy = false;
	unsigned short elNodeAttrs = 0;
	XMLNodeIteratorData iterator;
	XNodeInternal resultInternal = NULL;

	xntNode = (XMLCompNodeHdr) node;
	attrNode = getSpecialXMLNodeAttribute(xntNode, XNT_ELEMENT_NAME);

	Assert(attrNode->kind == XMLNODE_ATTRIBUTE);

	elName = XNODE_CONTENT(attrNode);

	if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
	{
		elName = dumpXMLAttrBinaryValue(elName, NULL, paramValues, paramMap, exprState);
		elNameCopy = true;
	}
	if (!isValidXMLName(elName))
	{
		elog(ERROR, "'%s' is not a valid element name", elName);
	}

	resultNodeSize = sizeof(XMLCompNodeHdrData) + strlen(elName) +1;
	resultNode = (XMLNodeHdr) palloc(resultNodeSize);
	resultNode->kind = XMLNODE_ELEMENT;

	/*
	 * This won't be effective anyway. The flag values will be determined when
	 * the template is going to be written to storage.
	 */
	resultNode->flags = 0;

	/*
	 * 'resultNode' is just temporary. As such it does not have to store
	 * references. 'nodeInternal->children' will hold them instead, until the
	 * 'real' storage is created. by writeXMLNodeInternal().
	 *
	 * 'children' is set to 0 regardless the actual number of children. The
	 * consequence is that XNODE_ELEMENT_NAME() does not leave any space for
	 * references when creating the temporary element, and in turn
	 * XNODE_ELEMENT_NAME() can be consistent in reading the name when
	 * inserting the node into the storage.
	 */
	((XMLCompNodeHdr) resultNode)->children = 0;

	strcpy(XNODE_ELEMENT_NAME((XMLCompNodeHdr) resultNode), elName);

	if (elNameCopy)
	{
		pfree(elName);
	}

	resultInternal = getInternalNode(resultNode, true);

	xmlnodeContainerInit(&resultInternal->children);

	/*
	 * TODO Possible optional attributes (which should only be namespace
	 * declarations) are currently skipped. Such namespaces may need to be
	 * applied to the the nested nodes.
	 */


	/*
	 * Add non-attribute children to the new node. This refers to
	 * non-attribute child nodes of the 'xnt:element' node, which may include
	 * 'xnt:attribute' tags (i.e. attrbutes of the node being constructed)
	 */
	initXMLNodeIteratorSpecial(&iterator, (XMLCompNodeHdr) node, false);
	while ((childNode = getNextXMLNodeChild(&iterator)) != NULL)
	{
		if ((childNode->kind == XMLNODE_TEXT && xmlStringWhitespaceOnly(XNODE_CONTENT(childNode))) ||
			childNode->kind == XMLNODE_COMMENT)
		{
			continue;
		}

		buildNewNodeTree(childNode, resultInternal, storageSize, paramValues, paramMap, exprState,
						 preserveSpace);
	}

	if (resultInternal->children.position == elNodeAttrs)
	{
		resultNode->flags |= XNODE_EMPTY;
	}

	*storageSize += MAX_PADDING(XNODE_ALIGNOF_COMPNODE) +
		resultNodeSize + sizeof(XMLNodeOffset);

	return resultInternal;
}

static XNodeInternal
xntProcessAttribute(XMLNodeHdr node,
				 XPathExprOperandValue paramValues, unsigned short *paramMap,
					XPathExprState exprState, unsigned int *storageSize)
{
	XMLCompNodeHdr xntNode;
	unsigned int resultNodeSize;
	XMLNodeHdr	attrNode,
				resultNode;
	char	   *attrName,
			   *attrValue;
	bool		attrNameCopy = false;
	bool		attrValueCopy = false;
	char	   *cnt;
	XNodeInternal resultInternal = NULL;

	xntNode = (XMLCompNodeHdr) node;;
	attrNode = getSpecialXMLNodeAttribute(xntNode, XNT_ATTRIBUTE_NAME);
	attrName = XNODE_CONTENT(attrNode);
	if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
	{
		attrName = dumpXMLAttrBinaryValue(attrName, NULL, paramValues, paramMap, exprState);
		attrNameCopy = true;
	}
	if (!isValidXMLName(attrName))
	{
		elog(ERROR, "'%s' is not a valid attribute name", attrName);
	}

	attrNode = getSpecialXMLNodeAttribute(xntNode, XNT_ATTRIBUTE_VALUE);
	attrValue = XNODE_CONTENT(attrNode);
	if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
	{
		attrValue = dumpXMLAttrBinaryValue(attrValue, NULL, paramValues, paramMap, exprState);
		resultNodeSize = 0;
		resultNode = getNewXMLAttribute(attrName, 0, attrValue, true, &resultNodeSize);
		attrValueCopy = true;
	}
	else
	{
		/*
		 * No validation of the value required in this case: parser must have
		 * done it when parsing the template.
		 */
		resultNodeSize = sizeof(XMLNodeHdrData) + strlen(attrName) +strlen(attrValue) + 2;
		resultNode = (XMLNodeHdr) palloc(resultNodeSize);
		resultNode->kind = XMLNODE_ATTRIBUTE;
		resultNode->flags = attrNode->flags;

		cnt = XNODE_CONTENT(resultNode);
		strcpy(cnt, attrName);
		cnt += strlen(attrName) + 1;
		strcpy(cnt, attrValue);
	}

	if (attrNameCopy)
	{
		pfree(attrName);
	}
	if (attrValueCopy)
	{
		pfree(attrValue);
	}

	resultInternal = getInternalNode(resultNode, true);
	*storageSize += resultNodeSize + sizeof(XMLNodeOffset);

	return resultInternal;
}
