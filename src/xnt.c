/*
 * Copyright (C) 2012, Antonin Houska
 */

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
XNTAttrNames xntAttributeInfo[] = {
	/* XNTNODE_TEMPLATE */
	{0, {}, {}},
	/* XNTNODE_COPY_OF */
	{1, {"expr"}, {true}},
	/* XNTNODE_ELEMENT */
	{1, {"name"}, {true}},
	/* XNTNODE_ATTRIBUTE */
	{2, {"name", "value"}, {true, true}}
};

static int	paramNameComparator(const void *left, const void *right);
static XPathExpression getXPathExpression(char *src, unsigned int termFlags, XMLNodeContainer paramNames,
				   unsigned short *endPos);
static char *getAttrValueTokenized(char *attrValue, unsigned int *valueSize, XMLNodeContainer paramNames);
static void visitXMLNodeForValidation(XMLNodeHdr *stack, unsigned int depth, void *userData);
static Datum castParameterValue(Datum value, Oid sourceType, Oid targetType);
static void buildNewNodeTree(XMLNodeHdr node, XNodeInternal parent, unsigned int *storageSize,
				 XPathExprOperandValue paramValues, unsigned short *paramMap, XPathExprState exprState);
static XNodeInternal getInternalNode(XMLNodeHdr node, bool copy);
static void freeTemplateTree(XNodeInternal root);
static XMLCompNodeHdr getTemplateFromDoc(XMLCompNodeHdr docRoot);
static void writeXNodeInternal(XNodeInternal node, char **output, XMLNodeOffset *root);
static XPathExpression substituteParameters(XPathExprState exprState, XPathExpression expression,
				XPathExprOperandValue paramValues, unsigned short *paramMap);
static bool whitespacesOnly(char *str);

PG_FUNCTION_INFO_V1(xnode_template_in);

Datum
xnode_template_in(PG_FUNCTION_ARGS)
{
	pg_enc		dbEnc;
	XMLNodeParserStateData parserState;
	char	   *input = PG_GETARG_CSTRING(0);
	XMLNodeOffset *rootOffPtr;
	XMLNodeHdr	root;

	if (strlen(input) == 0)
	{
		elog(ERROR, "zero length input string");
	}
	dbEnc = GetDatabaseEncoding();
	if (dbEnc != PG_UTF8)
	{
		elog(ERROR, "The current version of xmlnode requires both database encoding to be UTF-8.");
	}
	initXMLParserState(&parserState, input, XNTNODE_ROOT, getXNTNodeKind);
	xmlnodeParseDoc(&parserState);

	rootOffPtr = (XMLNodeOffset *) (parserState.tree + parserState.dstPos - sizeof(XMLNodeOffset));
	root = (XMLNodeHdr) (parserState.tree + *rootOffPtr);
	Assert(root->kind == XNTNODE_ROOT);
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

	PG_RETURN_CSTRING(dumpXMLNode(data, rootNdOff));
}

XMLNodeKind
getXNTNodeKind(char *name)
{
	unsigned int xslNmspPrefLen = strlen(XNTNODE_NAMESPACE_PREFIX);

	Assert(strncmp(name, XNTNODE_NAMESPACE_PREFIX, xslNmspPrefLen) == 0 && name[xslNmspPrefLen] == XNODE_CHAR_COLON);
	/* skip the 'xsl */
	name += xslNmspPrefLen + 1;

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
	elog(ERROR, "unrecognized xnt node '%s'", name);
	return 0;
}

char *
getXNTNodeName(XMLNodeKind kind)
{
	StringInfoData out;
	char	   *name;

	initStringInfo(&out);
	appendStringInfo(&out, "%s:", XNTNODE_NAMESPACE_PREFIX);

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

	appendStringInfoString(&out, name);
	return out.data;
}

/*
 * Returns name of special (reserved) attribute.
 * Name of such an attribute is determined by position in the array of
 * children. The name is not stored in the document tree.
 */
char *
getXNTAttributeName(XMLNodeKind kind, unsigned short attrNr)
{
	XNTAttrNames *attrInfo = xntAttributeInfo + (kind - XNTNODE_TEMPLATE);

	if (attrNr >= attrInfo->number)
	{
		return NULL;
	}
	return attrInfo->names[attrNr];
}

void
validateXNTTree(XMLNodeHdr root)
{
	bool		hasRootTemplate = false;

	walkThroughXMLTree(root, visitXMLNodeForValidation, false, (void *) &hasRootTemplate);
	if (!hasRootTemplate)
	{
		elog(ERROR, "'xnt:template' must be the root element");
	}
}

/*
 * Ensure that:
 *
 * 1. Reserved attribute are stored at defined positions.
 * 2. Name is not stored for the reserved attributes.
 * 3. Where appropriate, the attribute value is stored in special binary format
 * as opposed to NULL-terminated string.
 *
 * 'attrOffsets' - both input and output. It's expected to provide offsets of
 * attributes to be checked. Offsets of the processed attributes are returned.
 * Therefore there has to be sufficient space for slots for optional attributes
 * that the input document does not contain.
 * 'attrCount + XNT_SPECIAL_ATTRS_MAX' is always the safe size.
 *
 * 'attrCount' - the actual number of attributes that the template element contains.
 *
 * 'offsetsValid' - if given attribute is optional and the input element does not
 * contain it, then the corresponding position in 'attrOfsets' will be marked
 * as invalid. Size must be equal to that of 'attrOffsets'.
 *
 */
char *
preprocessXNTAttributes(XNodeListItem *attrOffsets, unsigned short attrCount, char *parserOutput,
						XMLNodeKind specNodeKind, bool *offsetsValid, unsigned int *specAttrCount, unsigned int *outSize,
						unsigned int *outCount, XMLNodeContainer paramNames)
{

	unsigned short i;
	XNTAttrNames *attrInfo = xntAttributeInfo + (specNodeKind - XNTNODE_TEMPLATE);
	XMLNodeHdr *attrsSorted = NULL;

	/*
	 * Space for the reserved attributes must always be there, even if all
	 * attributes of the given element are optional and user does not use any
	 * (namespace declarations are always allowed so the corner case is that
	 * user only specifies namespace declarations and nothing else).
	 */
	unsigned int outAttrsMax = attrCount + attrInfo->number;
	bool	   *attrValsToFree = NULL;
	char	   *resTmp,
			   *result = NULL;
	unsigned int *attrSizes = NULL;
	unsigned int indGen;

	*outSize = 0;
	*outCount = 0;
	*specAttrCount = attrInfo->number;

	if (outAttrsMax > 0)
	{
		unsigned int size;

		size = outAttrsMax * sizeof(bool);;
		attrValsToFree = (bool *) palloc(size);
		memset(attrValsToFree, false, size);

		size = outAttrsMax * sizeof(unsigned int);
		attrSizes = (unsigned int *) palloc(size);
		memset(attrSizes, 0, size);

		size = outAttrsMax * sizeof(XMLNodeHdr);
		attrsSorted = (XMLNodeHdr *) palloc(size);
		memset(attrsSorted, 0, size);
	}

	/*
	 * 'attrsSorted' will start with the special (reserved) attrs, while those
	 * 'generic' will follow.
	 */
	indGen = attrInfo->number;

	/*
	 * Sort the attributes, i.e. put the reserved into the slots at the start
	 * of 'attrsSorted'.
	 */
	for (i = 0; i < attrCount; i++)
	{
		unsigned short j;
		XNodeListItem *attrItem = attrOffsets + i;
		XMLNodeHdr	attr = (XMLNodeHdr) (parserOutput + attrItem->value.singleOff);
		char	   *attrName = XNODE_CONTENT(attr);
		char	   *attrValue = attrName + strlen(attrName) + 1;
		bool		found = false;

		/* Is this a reserved attribute? */
		for (j = 0; j < attrInfo->number; j++)
		{
			if (strcmp(attrName, attrInfo->names[j]) == 0)
			{
				XMLNodeHdr	specNode;
				char	   *specNodeValue;
				unsigned int valueSize,
							newAttrSize;
				XPathExpression expr = NULL;
				char	   *attrValueTokenized = NULL;

				if (specNodeKind == XNTNODE_COPY_OF && strcmp(attrName, attrInfo->names[XNT_COPY_OF_EXPR]) == 0)
				{
					expr = getXPathExpression(attrValue, XPATH_TERM_NULL, paramNames, NULL);
					valueSize = expr->common.size;
				}
				else if (strlen(attrValue) > 0 && strchr(attrValue, XNODE_CHAR_LBRKT_CUR) != NULL)
				{
					attrValueTokenized = getAttrValueTokenized(attrValue, &valueSize, paramNames);
				}
				else
				{
					/* Plain string or empty attribute. */
					valueSize = strlen(attrValue) + 1;
				}

				newAttrSize = valueSize + sizeof(XMLNodeHdrData);
				specNode = (XMLNodeHdr) palloc(newAttrSize);
				specNode->kind = attr->kind;
				specNode->flags = attr->flags;
				/* There's no need to store special attribute's name. */
				specNodeValue = XNODE_CONTENT(specNode);

				if (expr != NULL)
				{
					memcpy(specNodeValue, expr, valueSize);
					pfree(expr);
					specNode->flags |= XNODE_ATTR_VALUE_BINARY;
				}
				else if (attrValueTokenized != NULL)
				{
					memcpy(specNodeValue, attrValueTokenized, valueSize);
					pfree(attrValueTokenized);
					specNode->flags |= XNODE_ATTR_VALUE_BINARY;
				}
				else
				{
					strcpy(specNodeValue, attrValue);
				}

				attrsSorted[j] = specNode;
				*outSize += newAttrSize;
				attrSizes[j] = newAttrSize;
				attrValsToFree[j] = true;
				found = true;
				break;
			}
		}

		if (!found)
		{
			unsigned int nmspDefPrefLen;

			/* Namespace declaration is the only generic attribute allowed. */
			if ((attr->flags & XNODE_NMSP_PREFIX) &&
				strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX,
			   (nmspDefPrefLen = strlen(XNODE_NAMESPACE_DEF_PREFIX))) == 0 &&
				(attrName[nmspDefPrefLen] == XNODE_CHAR_COLON || attrName[nmspDefPrefLen] == '\0'))
			{

				attrValue = attrName + strlen(attrName) + 1;

				/* The whole attribute is stored in this case. */
				attrsSorted[indGen] = attr;
				attrSizes[indGen] = sizeof(XMLNodeHdrData) + strlen(attrName) +strlen(attrValue) + 2;
				*outSize += attrSizes[indGen];
				indGen++;
			}
			else
			{
				elog(ERROR, "element '%s' does not accept attribute '%s'", getXNTNodeName(specNodeKind), attrName);
			}
		}
	}

	/* Check for missing required attributes */
	for (i = 0; i < attrInfo->number; i++)
	{
		if (attrInfo->required[i] && (i >= attrCount || attrsSorted[i] == NULL))
		{
			elog(ERROR, "required attribute '%s' missing in element '%s'", attrInfo->names[i],
				 getXNTNodeName(specNodeKind));
		}
	}

	if (outAttrsMax > 0)
	{
		XNodeListItem *attrItem = attrOffsets;

		/*
		 * The first offset does not change so we can use it as initial new
		 * value.
		 */
		XMLNodeOffset offNew = attrItem->value.singleOff;

		result = resTmp = (char *) palloc(*outSize * sizeof(char));

		/*
		 * Construct the new sequence of attributes and adjust parent's
		 * offsets.
		 */
		for (i = 0; i < indGen; i++)
		{
			unsigned int currentSize = attrSizes[i];

			if (currentSize > 0)
			{
				Assert(attrsSorted[i] != NULL);

				memcpy(resTmp, attrsSorted[i], currentSize);
				resTmp += currentSize;
				offsetsValid[i] = true;
			}
			else
			{
				Assert(attrsSorted[i] == NULL);
				offsetsValid[i] = false;
			}

			if (i > 0)
			{					/* The first offset does not change. */
				attrItem->value.singleOff = offNew;
			}
			offNew += currentSize;
			attrItem++;
		}

		*outCount = indGen;

		for (i = 0; i < outAttrsMax; i++)
		{
			if (attrValsToFree[i])
			{
				pfree(attrsSorted[i]);
			}
		}
		pfree(attrValsToFree);
		pfree(attrSizes);
		pfree(attrsSorted);
	}
	return result;
}

/*
 * Attributes of 'ordinary' elements (i.e. those having neither 'xnt' nor 'xmlns' prefix) might reference parameters.
 * For example: '<section name="{$par1}"/>
 */
char *
preprocessXNTAttrValues(XNodeListItem *attrOffsets, unsigned short attrCount, char *parserOutput,
						unsigned int *outSize, XMLNodeContainer paramNames, XMLNodeContainer substNodes)
{
	unsigned short i;
	bool		workToDo = false;
	XMLNodeHdr *attrNodes = (XMLNodeHdr *) palloc(attrCount * sizeof(XMLNodeHdr));
	unsigned int arrSize;
	bool	   *toReplace;
	unsigned int *valueSizes,
			   *fixedSizes;
	char	  **valuesNew = NULL;
	char	   *result = NULL;
	char	   *resCursor;
	XNodeListItem *attrItem;
	XMLNodeOffset offNew;

	*outSize = 0;

	arrSize = attrCount * sizeof(bool);
	toReplace = (bool *) palloc(arrSize);
	memset(toReplace, false, arrSize);

	arrSize = attrCount * sizeof(unsigned int);
	valueSizes = (unsigned int *) palloc(arrSize);
	fixedSizes = (unsigned int *) palloc(arrSize);;

	arrSize = attrCount * sizeof(char *);
	valuesNew = (char **) palloc(arrSize);
	memset(valuesNew, 0, arrSize);

	for (i = 0; i < attrCount; i++)
	{
		XNodeListItem *attrItem = attrOffsets + i;
		XMLNodeHdr	attr = (XMLNodeHdr) (parserOutput + attrItem->value.singleOff);
		char	   *attrName = XNODE_CONTENT(attr);
		char	   *attrValue;

		attrNodes[i] = attr;
		attrValue = attrName + strlen(attrName) + 1;

		if (strlen(attrValue) > 0 && strchr(attrValue, XNODE_CHAR_LBRKT_CUR) != NULL)
		{
			toReplace[i] = true;
			if (!workToDo)
			{
				workToDo = true;
			}
		}
		fixedSizes[i] = sizeof(XMLNodeHdrData) + strlen(attrName) +1;
		valueSizes[i] = strlen(attrValue) + 1;
		valuesNew[i] = attrValue;
	}

	if (!workToDo)
	{
		pfree(attrNodes);
		pfree(toReplace);
		pfree(fixedSizes);
		pfree(valueSizes);
		pfree(valuesNew);
		return NULL;
	}


	for (i = 0; i < attrCount; i++)
	{
		if (toReplace[i])
		{
			XMLNodeHdr	attrOrig = attrNodes[i];
			char	   *attrName = XNODE_CONTENT(attrOrig);
			char	   *attrValue = attrName + strlen(attrName) + 1;
			unsigned int valueSizeNew = 0;

			valuesNew[i] = getAttrValueTokenized(attrValue, &valueSizeNew, paramNames);
			valueSizes[i] = valueSizeNew;
		}
		*outSize += fixedSizes[i] + valueSizes[i];
	}

	result = resCursor = (char *) palloc(*outSize);

	attrItem = attrOffsets;
	offNew = attrItem->value.singleOff;

	for (i = 0; i < attrCount; i++)
	{
		XMLNodeHdr	attr = attrNodes[i];
		XMLNodeHdr	attrNew;

		/*
		 * Header and attribute name is always copied from the original
		 * attribute.
		 */
		memcpy(resCursor, attr, fixedSizes[i]);
		attrNew = (XMLNodeHdr) resCursor;
		resCursor += fixedSizes[i];

		/*
		 * The value is either modified (binary) or the original (plain
		 * string).
		 */
		if (toReplace[i])
		{
			attrNew->flags |= XNODE_ATTR_VALUE_BINARY;
			memcpy(resCursor, valuesNew[i], valueSizes[i]);
			pfree(valuesNew[i]);

			/*
			 * This node needs special attention when the template is being
			 * used.
			 */
			xmlnodePushSingleNode(substNodes, offNew);
		}
		else
		{
			strcpy(resCursor, valuesNew[i]);
		}
		resCursor += valueSizes[i];

		attrItem->value.singleOff = offNew;
		offNew += fixedSizes[i] + valueSizes[i];
		attrItem++;
	}

	pfree(attrNodes);
	pfree(toReplace);
	pfree(fixedSizes);
	pfree(valueSizes);
	pfree(valuesNew);
	return result;
}

/*
 * If 'paramNames' is not-NULL, then 'paramValues', 'paramMap' and 'exprState' must be passed
 * and the function evaluates each expression that the attribute value contains and returns
 * the value as string.
 *
 * If 'paramNames' is NULL then the function returns source text of the contained expression(s).
 */
char *
dumpBinaryAttrValue(char *binValue, char **paramNames, XPathExprOperandValue paramValues,
					unsigned short *paramMap, XPathExprState exprState)
{
	StringInfoData output;
	char	   *cursor = binValue;
	unsigned short tokenCount,
				i;

	tokenCount = *((uint8 *) cursor);
	Assert(tokenCount > 0);
	output.maxlen = 32;
	output.data = (char *) palloc(output.maxlen);
	resetStringInfo(&output);

	cursor++;
	for (i = 0; i < tokenCount; i++)
	{
		bool		isExpr = *((bool *) cursor++);

		if (isExpr)
		{
			XPathExpression expr = (XPathExpression) cursor;

			if (paramNames != NULL)
			{
				appendStringInfoChar(&output, XNODE_CHAR_LBRKT_CUR);
				dumpXPathExpression(expr, NULL, &output, true, paramNames, false);
				appendStringInfoChar(&output, XNODE_CHAR_RBRKT_CUR);
			}
			else
			{
				XPathExprOperandValueData result,
							resultCast;
				XPathExpression exprCopy;

				exprCopy = substituteParameters(exprState, expr, paramValues, paramMap);
				evaluateXPathExpression(exprState, exprCopy, 0, &result);

				if (result.type == XPATH_VAL_NODESET)
				{
					elog(ERROR, "node-set is not expected in attribute value");
				}
				if (!result.isNull)
				{
					char	   *valueStr;

					castXPathExprOperandToStr(exprState, &result, &resultCast);
					valueStr = (char *) getXPathOperandValue(exprState, resultCast.v.stringId, XPATH_VAR_STRING);
					appendStringInfoString(&output, valueStr);
				}

				pfree(exprCopy);
			}
			cursor += expr->common.size;
		}
		else
		{
			appendStringInfoString(&output, cursor);
			cursor += strlen(cursor) + 1;
		}
	}
	return output.data;
}

static int
paramNameComparator(const void *left, const void *right)
{
	return strcmp(((XNTParamNameSorted *) left)->name, ((XNTParamNameSorted *) right)->name);
}

static XPathExpression
getXPathExpression(char *src, unsigned int termFlags, XMLNodeContainer paramNames,
				   unsigned short *endPos)
{
	XPathExpression expr = (XPathExpression) palloc(XPATH_EXPR_BUFFER_SIZE);
	XPathOffset outPos = sizeof(XPathExpressionData) + XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);
	XPathParserStateData state;

	state.c = src;
	state.cWidth = 0;
	state.pos = 0;

	parseXPathExpression(expr, &state, termFlags, NULL, (char *) expr, &outPos, false, false, NULL, NULL, true,
						 paramNames);

	if (expr->variables > 0)
	{
		XPathOffset *varOffPtr;
		unsigned short k;

		/*
		 * (Location paths are excluded by having passed NULL as 'paths' to
		 * parseXPathExpression().
		 */
		Assert(expr->npaths == 0);

		/*
		 * Make sure the XPath expression does not contain attributes.
		 */
		varOffPtr = (XPathOffset *) ((char *) expr + sizeof(XPathExpressionData));
		for (k = 0; k < expr->variables; k++)
		{
			XPathExprOperand opnd = (XPathExprOperand) ((char *) expr + *varOffPtr);

			if (opnd->common.type == XPATH_OPERAND_ATTRIBUTE)
			{
				elog(ERROR, "template expression must not contain attributes");
			}

			/*
			 * Functions having no arguments are excluded by passing
			 * 'mainExpr=true' to parseXPathExpression()
			 */
			Assert(opnd->common.type != XPATH_OPERAND_FUNC_NOARG);
		}
	}

	if (endPos != NULL)
	{
		*endPos = state.pos;
	}
	return expr;
}

static char *
getAttrValueTokenized(char *attrValue, unsigned int *valueSize, XMLNodeContainer paramNames)
{
	unsigned short tokenCount = 0;
	char	   *tokens[XNT_ATTR_VALUE_MAX_TOKENS];
	bool		tokenIsExpr[XNT_ATTR_VALUE_MAX_TOKENS];
	unsigned int tokenSizes[XNT_ATTR_VALUE_MAX_TOKENS];
	char	   *c = attrValue;
	unsigned short k;
	char	   *result,
			   *resCursor;

	/*
	 * Attribute value contains xpath expressions and therefore must be
	 * tokenized.
	 */

	*valueSize = 0;

	/* Each iteration processes 1 token. */
	while (*c != '\0')
	{
		if (tokenCount == XNT_ATTR_VALUE_MAX_TOKENS)
		{
			elog(ERROR, "xnt attribute value must not consist of more than %u tokens",
				 XNT_ATTR_VALUE_MAX_TOKENS);
		}

		if (*c == XNODE_CHAR_LBRKT_CUR)
		{
			XPathExpression tokenExpr;
			unsigned short endPos = 0;

			c++;				/* Skip the left curly bracket. */

			/* Parse the xpath expression, ending with '}' */
			tokenExpr = getXPathExpression(c, XPATH_TERM_RBRKT_CRL, paramNames, &endPos);
			tokens[tokenCount] = (char *) tokenExpr;
			tokenSizes[tokenCount] = tokenExpr->common.size;
			tokenIsExpr[tokenCount] = true;
			tokenCount++;
			*valueSize += tokenExpr->common.size;
			c += endPos;
		}
		else
		{
			char	   *tokStart = c;
			unsigned int tokLen = 0;

			/* plain string */
			while (*c != '\0' && *c != XNODE_CHAR_LBRKT_CUR)
			{
				unsigned int cWidth = pg_utf_mblen((unsigned char *) c);

				c += cWidth;
				tokLen += cWidth;
			}

			if (tokLen > 0)
			{
				unsigned int size = tokLen + 1;
				char	   *token = (char *) palloc(size);

				memcpy(token, tokStart, tokLen);
				token[tokLen] = '\0';
				tokens[tokenCount] = token;
				tokenSizes[tokenCount] = size;
				tokenIsExpr[tokenCount] = false;
				tokenCount++;
				*valueSize += size;
			}
		}
	}

	Assert(tokenCount > 0);

	/* Add space for the total number of tokens as well as type of each token. */
	*valueSize += tokenCount * sizeof(bool) + sizeof(uint8);

	/* Construct the tokenized binary value. */
	result = resCursor = (char *) palloc(*valueSize);
	*((uint8 *) resCursor) = tokenCount;
	resCursor++;

	for (k = 0; k < tokenCount; k++)
	{
		*((bool *) resCursor) = tokenIsExpr[k];
		resCursor++;
		memcpy(resCursor, tokens[k], tokenSizes[k]);
		resCursor += tokenSizes[k];
		pfree(tokens[k]);
	}
	return result;
}

static void
visitXMLNodeForValidation(XMLNodeHdr *stack, unsigned int depth, void *userData)
{
	XMLNodeHdr	node = stack[depth];
	XMLNodeHdr	parent = NULL;

	if ((node->flags & XNODE_EL_SPECIAL) == 0)
	{
		return;
	}

	if (depth >= 2)
	{
		parent = stack[depth - 1];
	}

	switch (node->kind)
	{
		case XNTNODE_ROOT:
			Assert(depth == 0);
			break;

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


PG_FUNCTION_INFO_V1(xnode_from_template);

Datum
xnode_from_template(PG_FUNCTION_ARGS)
{
	xnt			template;
	XMLCompNodeHdr templDocRoot,
				templNode;
	char	   *templHdrData,
			   *templData;
	char	  **templParNames = NULL;
	XNTHeader	templHdr;
	ArrayType  *parNameArr;
	int			parNameCount = 0;
	XNTParamNameSorted *parNames = NULL;
	unsigned int substNodeCount = 0;
	XMLNodeOffset *substNodes = NULL;
	Datum		row;
	HeapTupleHeader tupHdr;
	Oid			tupType;
	int32		tupTypmod;
	TupleDesc	tupDesc;
	unsigned int i;
	HeapTupleData tmpTup,
			   *tup;
	XPathExprOperandValue parValues = NULL;
	unsigned short *paramMap = NULL;
	Form_pg_proc procStruct;
	Oid			nodeOid;
	XNodeInternal newTreeRoot = NULL;
	XNodeInternal templRootInternal;
	unsigned int resSizeEstimate = 0;
	char	   *result = NULL;
	XMLNodeOffset offRoot = 0;
	XPathExprState exprState = NULL;

	/* Retrieve parameter names out of the template. */
	template = (xnt) PG_GETARG_VARLENA_P(0);
	templData = VARDATA(template);
	templDocRoot = (XMLCompNodeHdr) XNODE_ROOT(template);
	Assert(templDocRoot->common.kind == XNTNODE_ROOT);
	templHdrData = XNODE_ELEMENT_NAME(templDocRoot);
	templHdr = (XNTHeader) templHdrData;

	if (templHdr->paramCount > 0)
	{
		templHdrData += sizeof(XNTHeaderData);
		templParNames = getXPathParameterArray(&templHdrData, templHdr->paramCount);
	}
	substNodeCount = templHdr->substNodesCount;
	if (substNodeCount > 0)
	{
		substNodes = (XMLNodeOffset *) templHdrData;
	}

	/* Retrieve names of the attributes that the function caller provides. */
	parNameArr = PG_GETARG_ARRAYTYPE_P(1);

	if (ARR_NDIM(parNameArr) == 0)
	{
		parNameCount = 0;
	}
	else if (ARR_NDIM(parNameArr) == 1)
	{
		int		   *attrArrDims = ARR_DIMS(parNameArr);

		parNameCount = attrArrDims[0];
	}
	else
	{
		elog(ERROR, "parameter names must be passed in 1-dimensional array");
	}

	if (parNameCount != templHdr->paramCount)
	{
		elog(ERROR, "number of parameter names passed must be equal to the number of parameters that the template references");
	}

	if (parNameCount > 0)
	{
		Oid			elType,
					arrType;
		int16		arrTypLen,
					elTypLen;
		bool		elByVal;
		char		elAlign;
		XNTParamNameSorted *parNamesTmp;

		elType = parNameArr->elemtype;
		arrType = get_array_type(elType);
		Assert(arrType != InvalidOid);
		arrTypLen = get_typlen(arrType);
		get_typlenbyvalalign(elType, &elTypLen, &elByVal, &elAlign);

		/*
		 * Get the parameter names from the array. There's no need to validate
		 * characters. The validation is performed when parsing the template,
		 * so invalid parameter names simply won't match.
		 */
		parNames = parNamesTmp = (XNTParamNameSorted *) palloc(parNameCount * sizeof(XNTParamNameSorted));

		for (i = 0; i < parNameCount; i++)
		{
			int			subscr[1];
			Datum		elDatum;
			bool		elIsNull;
			char	   *parName;

			subscr[0] = i + 1;
			elDatum = array_ref(parNameArr, 1, subscr, arrTypLen, elTypLen, elByVal, elAlign, &elIsNull);
			if (elIsNull)
			{
				elog(ERROR, "all parameter names must be not-NULL");
			}
			parName = TextDatumGetCString(elDatum);
			if (strlen(parName) == 0)
			{
				elog(ERROR, "all parameter names must have non-zero length");
			}
			parNamesTmp->order = i;
			parNamesTmp->name = parName;
			parNamesTmp++;
		}

		if (parNameCount > 0)
		{
			char	   *prev;

			if (parNameCount > 1)
			{
				/* Sort by the name. */
				qsort(parNames, parNameCount, sizeof(XNTParamNameSorted), paramNameComparator);

				/* Check if there's no duplicate. */
				parNamesTmp = parNames;
				prev = parNamesTmp->name;
				parNamesTmp++;

				for (i = 1; i < parNameCount; i++)
				{
					if (strcmp(prev, parNamesTmp->name) == 0)
					{
						elog(ERROR, "parameter '%s' is passed more than once", prev);
					}
					prev = parNamesTmp->name;
					parNamesTmp++;
				}
			}

			Assert(parNameCount == templHdr->paramCount);

			paramMap = (unsigned short *) palloc(templHdr->paramCount * sizeof(unsigned short));

			/* Match the template parameters to those passed to the function. */
			for (i = 0; i < templHdr->paramCount; i++)
			{
				XNTParamNameSorted key;
				XNTParamNameSorted *matching;

				key.name = templParNames[i];
				matching = (XNTParamNameSorted *) bsearch(&key, parNames, parNameCount,
							sizeof(XNTParamNameSorted), paramNameComparator);

				if (matching != NULL)
				{
					/*
					 * 'i' is the order that template expressions use to
					 * reference parameter names (i. e. the order of given
					 * parameter in the list stored in template header).
					 *
					 * paramMap[i] says at which position the user passes the
					 * corresponding parameter in the function argument list.
					 */
					paramMap[i] = matching->order;
				}
				else
				{
					elog(ERROR, "the template references parameter '%s' but it's not passed", key.name);
				}
			}
		}
	}

	/* Retrieve values of the parameters. */
	row = PG_GETARG_DATUM(2);
	tupHdr = DatumGetHeapTupleHeader(row);
	tupType = HeapTupleHeaderGetTypeId(tupHdr);
	tupTypmod = HeapTupleHeaderGetTypMod(tupHdr);
	tupDesc = lookup_rowtype_tupdesc(tupType, tupTypmod);

	if (tupDesc->natts != parNameCount)
	{
		elog(ERROR, "the number of parameter names must be equal to that of values");
	}

	/*
	 * As there are no fixed OIDs for our types, let's get it from catalog.
	 * 'node' is the return type.
	 */
	tup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fcinfo->flinfo->fn_oid));
	Assert(HeapTupleIsValid(tup));
	procStruct = (Form_pg_proc) GETSTRUCT(tup);
	nodeOid = procStruct->prorettype;
	ReleaseSysCache(tup);

	tmpTup.t_len = HeapTupleHeaderGetDatumLength(tupHdr);
	tmpTup.t_data = tupHdr;
	tup = &tmpTup;

	parValues = (XPathExprOperandValue) palloc(tupDesc->natts * sizeof(XPathExprOperandValueData));

	/*
	 * More than the default size because it will be used for all expressions
	 * in the template. (We can't free the cache when going to process another
	 * expression because parameters have to be available for all.)
	 */
	exprState = createXPathVarCache(4 * XPATH_VAR_CACHE_DEF_SIZE);

	for (i = 0; i < tupDesc->natts; i++)
	{
		Oid			attTyp;
		Datum		attValue;
		bool		isnull = false;
		XPathExprOperandValue parValue;

		attTyp = tupDesc->attrs[i]->atttypid;
		attValue = heap_getattr(tup, i + 1, tupDesc, &isnull);

		if (isnull)
		{
			elog(ERROR, "NULL value passed to parameter %u", i + 1);
		}
		parValue = parValues + i;

		/*
		 * Set default values. 'negative' is not set because
		 * substituteParameters() ignores it anyway, in order to preserver
		 * sign that the source expression might contain.
		 */
		parValue->isNull = false;
		parValue->castToNumber = false;

		/* 'nodeOid' is not a constant so can't be used with 'switch'. */
		if (attTyp == nodeOid)
		{
			xmlnode		node = PG_DETOAST_DATUM(attValue);
			XMLNodeHdr	root = XNODE_ROOT(node);

			parValue->v.nodeSet.count = 1;
			parValue->v.nodeSet.isDocument = false;
			parValue->v.nodeSet.nodes.nodeId = getXPathOperandId(exprState, (void *) root, XPATH_VAR_NODE_SINGLE);
			parValue->type = XPATH_VAL_NODESET;
		}
		else
		{
			switch (attTyp)
			{
				case BOOLOID:
					parValue->v.boolean = DatumGetBool(castParameterValue(attValue, attTyp, BOOLOID));
					parValue->type = XPATH_VAL_BOOLEAN;
					break;

				case INT2OID:
				case INT4OID:
				case INT8OID:
				case NUMERICOID:
				case FLOAT4OID:
				case FLOAT8OID:
					parValue->v.num = DatumGetFloat8(castParameterValue(attValue, attTyp, FLOAT8OID));
					parValue->type = XPATH_VAL_NUMBER;
					break;

				case BPCHAROID:
				case VARCHAROID:
				case TEXTOID:
					{
						char	   *valStr;

						valStr = TextDatumGetCString(castParameterValue(attValue, attTyp, TEXTOID));

						/*
						 * Maybe too restrictive, but that's not a problem.
						 * Only the resulting document is restricted by XML
						 * specification, whereas the XNT parameters are not.
						 * Thus so this check may be more strict if simplicity
						 * demands it.
						 */
						if (strchr(valStr, XNODE_CHAR_LARROW) || strchr(valStr, XNODE_CHAR_RARROW) ||
							strchr(valStr, XNODE_CHAR_AMPERSAND))
						{
							elog(ERROR, "neither '<' nor '>' allowed in text parameter");
						}
						parValue->v.stringId = getXPathOperandId(exprState, (void *) valStr, XPATH_VAR_STRING);
						parValue->type = XPATH_VAL_STRING;
						break;
					}

				default:
					elog(ERROR, "value of parameter %u has unrecognized type", i + 1);
					break;
			}
		}
	}
	ReleaseTupleDesc(tupDesc);

	templNode = getTemplateFromDoc(templDocRoot);

	newTreeRoot = (XNodeInternal) palloc(sizeof(XNodeInternalData));
	newTreeRoot->node = (XMLNodeHdr) templNode;
	newTreeRoot->copy = false;
	xmlnodeContainerInit(&newTreeRoot->children);

	buildNewNodeTree((XMLNodeHdr) templNode, newTreeRoot, &resSizeEstimate, parValues, paramMap, exprState);

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
		char	   *resData,
				   *resTmp;
		XMLNodeOffset *offRootPtr;

		/* varlena header + root node offset */
		resSizeEstimate += VARHDRSZ + sizeof(XMLNodeOffset);
		if (templRootInternal->children.position > 1)
		{
			/* Document fragment node in addition. */
			resSizeEstimate += sizeof(XMLCompNodeHdrData) + templNode->children * sizeof(XMLNodeOffset);
		}
		result = (char *) palloc(resSizeEstimate);

		resData = resTmp = result + VARHDRSZ;

		if (templRootInternal->children.position == 1)
		{
			writeXNodeInternal(templRootInternal->children.content->value.singlePtr, &resTmp, &offRoot);
		}
		else
		{
			writeXNodeInternal(templRootInternal, &resTmp, &offRoot);
		}
		offRootPtr = (XMLNodeOffset *) resTmp;
		*offRootPtr = offRoot;
		resTmp += sizeof(XMLNodeOffset);
		resultSize = (char *) resTmp - result;
		SET_VARSIZE(result, resultSize);
	}

	freeTemplateTree(newTreeRoot);
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

/*
 * Caller must not pass combination of source and target types for which
 * there's no entry in pg_cast.
 */
static Datum
castParameterValue(Datum value, Oid sourceType, Oid targetType)
{
	HeapTuple	tup;
	Form_pg_cast castStruct;
	Oid			castFuncOid;

	if (sourceType == targetType)
	{
		return value;
	}

	tup = SearchSysCache2(CASTSOURCETARGET, ObjectIdGetDatum(sourceType), ObjectIdGetDatum(targetType));
	Assert(HeapTupleIsValid(tup));
	castStruct = (Form_pg_cast) GETSTRUCT(tup);
	castFuncOid = castStruct->castfunc;
	ReleaseSysCache(tup);

	if (castFuncOid == 0)
	{
		return value;
	}

	return OidFunctionCall1(castFuncOid, value);
}

/*
 * Generates the template tree with parameters substituted.
 * XNTNODE_TEMPLATE must be the root for the outermost call.
 *
 * 'storageSize' gets incremented by the storage size required for the current node plus that of children.
 * For template only children and the corresponding references are accounted, while the template
 * node itself is ignored.
 */
static void
buildNewNodeTree(XMLNodeHdr node, XNodeInternal parent, unsigned int *storageSize,
				 XPathExprOperandValue paramValues, unsigned short *paramMap, XPathExprState exprState)
{

	XNodeInternal nodeInternal = NULL;

	/* Process children if the node has some. */
	if (node->kind == XMLNODE_ELEMENT || node->kind == XNTNODE_TEMPLATE)
	{
		char	   *childOffPtr;
		unsigned char bwidth;
		XMLNodeOffset offRel;
		unsigned short i;
		XMLCompNodeHdr compNode = (XMLCompNodeHdr) node;

		nodeInternal = getInternalNode(node, false);

		xmlnodeContainerInit(&nodeInternal->children);

		childOffPtr = XNODE_FIRST_REF(compNode);
		bwidth = XNODE_GET_REF_BWIDTH(compNode);

		for (i = 0; i < compNode->children; i++)
		{
			XMLNodeHdr	childNode;

			offRel = readXMLNodeOffset(&childOffPtr, bwidth, true);
			childNode = (XMLNodeHdr) ((char *) node - offRel);

			/*
			 * The template can currently have no other attribute than
			 * 'xmlns:...'. For other (non-root) special nodes such cases will
			 * be treated separate.
			 */
			if (node->kind == XNTNODE_TEMPLATE && childNode->kind == XMLNODE_ATTRIBUTE)
			{
				continue;
			}

			if (childNode->kind == XMLNODE_TEXT && whitespacesOnly(XNODE_CONTENT(childNode)))
			{
				continue;
			}

			if (childNode->kind == XMLNODE_COMMENT)
			{
				continue;
			}

			buildNewNodeTree(childNode, nodeInternal, storageSize, paramValues, paramMap, exprState);
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
			XMLCompNodeHdr xntNode;
			char	   *refPtr;
			char		bwidth;
			XMLNodeOffset offRel;
			XMLNodeHdr	attrNode;
			XMLNodeHdr	resultNode;
			unsigned int resultNodeSize = 0;

			switch (node->kind)
			{
				case XNTNODE_COPY_OF:
					{
						XPathExpression expr,
									exprCopy;
						XPathExprOperandValueData exprResult;

						/* Expression must be the first attribute. */
						xntNode = (XMLCompNodeHdr) node;
						refPtr = XNODE_FIRST_REF(xntNode);
						bwidth = XNODE_GET_REF_BWIDTH(xntNode);
						offRel = readXMLNodeOffset(&refPtr, bwidth, false);
						attrNode = (XMLNodeHdr) ((char *) xntNode - offRel);

						Assert(attrNode->kind == XMLNODE_ATTRIBUTE);
						Assert(attrNode->flags & XNODE_ATTR_VALUE_BINARY);

						/*
						 * Special attributes only contain the value. (Name is
						 * determined by the position.)
						 */
						expr = (XPathExpression) XNODE_CONTENT(attrNode);
						exprCopy = substituteParameters(exprState, expr, paramValues, paramMap);
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
									 * Fragment must have been turned into
									 * node-set during parameter substitution.
									 */
									Assert(singleNode->kind != XMLNODE_DOC_FRAGMENT);

									if ((singleNode->kind == XMLNODE_TEXT && whitespacesOnly(XNODE_CONTENT(singleNode))) ||
										singleNode->kind == XMLNODE_COMMENT)
									{
										continue;
									}

									/*
									 * No need to initialize 'nodeInternal'.
									 * We're only interested in the child
									 * node.
									 */
									buildNewNodeTree(singleNode, parent, storageSize, paramValues, paramMap, exprState);
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
								 * TODO 1. Ensure there are no invalid chars
								 * 2. Some flags might need to be set,
								 * especially XNODE_TEXT_SPEC_CHARS
								 */
								resultNode->flags = 0;

								content = XNODE_CONTENT(resultNode);
								strcpy(content, strValue);

								nodeInternal = getInternalNode(resultNode, true);

								*storageSize += resultNodeSize + sizeof(XMLNodeOffset);
							}
						}
						pfree(exprCopy);
					}
					break;

				case XNTNODE_ELEMENT:
					{
						XMLNodeHdr	childNode;
						char	   *elName;
						bool		elNameCopy = false;
						unsigned short i,
									elNodeAttrs = 0;


						/* Element name is the first attribute. */
						xntNode = (XMLCompNodeHdr) node;
						refPtr = XNODE_FIRST_REF(xntNode);
						bwidth = XNODE_GET_REF_BWIDTH(xntNode);
						offRel = readXMLNodeOffset(&refPtr, bwidth, true);
						attrNode = (XMLNodeHdr) ((char *) xntNode - offRel);

						Assert(attrNode->kind == XMLNODE_ATTRIBUTE);

						elName = XNODE_CONTENT(attrNode);
						if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
						{
							elName = dumpBinaryAttrValue(elName, NULL, paramValues, paramMap, exprState);
							elNameCopy = true;
						}

						resultNodeSize = sizeof(XMLCompNodeHdrData) + strlen(elName) +1;
						resultNode = (XMLNodeHdr) palloc(resultNodeSize);
						resultNode->kind = XMLNODE_ELEMENT;
						resultNode->flags = 0;

						/*
						 * 'resultNode' is just temporary. As such it does not
						 * have to store references. 'nodeInternal->children'
						 * will hold them instead, until the 'real' storage is
						 * created. by writeXNodeInternal().
						 *
						 * 'children' is set to 0 regardless the actual number
						 * of children. The consequence is that
						 * XNODE_ELEMENT_NAME() does not leave any space for
						 * references when creating the temporary element, and
						 * in turn XNODE_ELEMENT_NAME() can be consistent in
						 * reading the name when inserting the node into the
						 * storage.
						 */
						((XMLCompNodeHdr) resultNode)->children = 0;

						strcpy(XNODE_ELEMENT_NAME((XMLCompNodeHdr) resultNode), elName);

						if (elNameCopy)
						{
							pfree(elName);
						}

						nodeInternal = getInternalNode(resultNode, true);

						xmlnodeContainerInit(&nodeInternal->children);

						/*
						 * TODO Possible optional attributes (which should
						 * only be namespace declarations) are currently
						 * skipped. Such namespaces may need to be applied to
						 * the the nested nodes.
						 */
						for (i = 1; i < xntNode->children; i++)
						{
							/*
							 * 'step=false' so that we stay at the node if
							 * it's the first non-attribute.
							 */
							offRel = readXMLNodeOffset(&refPtr, bwidth, false);
							childNode = (XMLNodeHdr) ((char *) xntNode - offRel);
							if (childNode->kind != XMLNODE_ATTRIBUTE)
							{
								break;
							}
							refPtr += bwidth;
						}

						/*
						 * Add non-attribute children to the new node. This
						 * refers to non-attribute child nodes of the
						 * 'xnt:element' node, which may include
						 * 'xnt:attribute' tags (i.e. attrbutes of the node
						 * being constructed)
						 */

						/*
						 * TODO if 'xnt:attribute' nodes are there, make sure
						 * the names are unique.
						 */
						for (; i < xntNode->children; i++)
						{
							offRel = readXMLNodeOffset(&refPtr, bwidth, true);
							childNode = (XMLNodeHdr) ((char *) xntNode - offRel);

							if ((childNode->kind == XMLNODE_TEXT && whitespacesOnly(XNODE_CONTENT(childNode))) ||
								childNode->kind == XMLNODE_COMMENT)
							{
								continue;
							}
							buildNewNodeTree(childNode, nodeInternal, storageSize, paramValues, paramMap, exprState);
						}

						if (nodeInternal->children.position == elNodeAttrs)
						{
							resultNode->flags |= XNODE_EMPTY;
						}

						*storageSize += resultNodeSize + sizeof(XMLNodeOffset);
					}

					break;
				case XNTNODE_ATTRIBUTE:
					{
						char	   *attrName,
								   *attrValue;
						bool		attrNameCopy = false;
						bool		attrValueCopy = false;
						char	   *cnt;

						xntNode = (XMLCompNodeHdr) node;
						refPtr = XNODE_FIRST_REF(xntNode);
						bwidth = XNODE_GET_REF_BWIDTH(xntNode);

						offRel = readXMLNodeOffset(&refPtr, bwidth, true);
						attrNode = (XMLNodeHdr) ((char *) xntNode - offRel);

						attrName = XNODE_CONTENT(attrNode);
						if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
						{
							attrName = dumpBinaryAttrValue(attrName, NULL, paramValues, paramMap, exprState);
							attrNameCopy = true;
						}

						/*
						 * TODO check validity
						 */

						offRel = readXMLNodeOffset(&refPtr, bwidth, true);
						attrNode = (XMLNodeHdr) ((char *) xntNode - offRel);

						attrValue = XNODE_CONTENT(attrNode);
						if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
						{
							attrValue = dumpBinaryAttrValue(attrValue, NULL, paramValues, paramMap, exprState);
							attrValueCopy = true;
						}

						/*
						 * TODO check validity
						 */

						resultNodeSize = sizeof(XMLNodeHdrData) + strlen(attrName) +strlen(attrValue) + 2;
						resultNode = (XMLNodeHdr) palloc(resultNodeSize);
						resultNode->kind = XMLNODE_ATTRIBUTE;

						/*
						 * TODO set the flags as appropriate
						 */
						resultNode->flags = 0;

						cnt = XNODE_CONTENT(resultNode);
						strcpy(cnt, attrName);
						cnt += strlen(attrName) + 1;
						strcpy(cnt, attrValue);

						if (attrNameCopy)
						{
							pfree(attrName);
						}
						if (attrValueCopy)
						{
							pfree(attrValue);
						}

						nodeInternal = getInternalNode(resultNode, true);

						*storageSize += resultNodeSize + sizeof(XMLNodeOffset);
					}
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
			*storageSize += sizeof(XMLCompNodeHdrData) + strlen(name) +1;
		}
		else if (node->kind == XMLNODE_ATTRIBUTE && (node->flags & XNODE_ATTR_VALUE_BINARY))
		{
			XMLNodeHdr	attrOrig,
						attrNew;
			char	   *name,
					   *valueOrig,
					   *valueNew,
					   *c;
			unsigned int sizeNew;

			/* Construct attribute as 2 NULL-terminated strings (name, value). */
			attrOrig = node;
			name = XNODE_CONTENT(attrOrig);
			valueOrig = name + strlen(name) + 1;
			valueNew = dumpBinaryAttrValue(valueOrig, NULL, paramValues, paramMap, exprState);
			sizeNew = sizeof(XMLNodeHdr) + strlen(name) +strlen(valueNew) + 2;
			attrNew = (XMLNodeHdr) palloc(sizeNew);
			attrNew->kind = XMLNODE_ATTRIBUTE;

			/*
			 * TODO copy the original header and (un)set the appropriate flags
			 */
			attrNew->flags = 0;
			c = XNODE_CONTENT(attrNew);
			strcpy(c, name);
			c += strlen(name) + 1;
			strcpy(c, valueNew);

			nodeInternal = getInternalNode(attrNew, true);

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

static void
freeTemplateTree(XNodeInternal root)
{
	if (root->node->kind == XMLNODE_ELEMENT || root->node->kind == XNTNODE_TEMPLATE)
	{
		unsigned short i;
		XMLNodeContainer children;
		XNodeListItem *child;

		children = &root->children;
		child = children->content;

		for (i = 0; i < children->position; i++)
		{
			freeTemplateTree(child->value.singlePtr);
			child++;
		}

		xmlnodeContainerFree(&root->children);
	}

	if (root->copy)
	{
		pfree(root->node);
	}
	pfree(root);
}

/*
 * The template document might contain multiple root nodes, even though there's
 * exactly one root element). This function returns that root element, i.e. 'xnt:template'
 */
static XMLCompNodeHdr
getTemplateFromDoc(XMLCompNodeHdr docRoot)
{
	char	   *childOffPtr;
	unsigned char bwidth;
	unsigned short i;
	XMLNodeHdr	child = NULL;

	Assert(docRoot->common.kind == XNTNODE_ROOT);

	childOffPtr = XNODE_FIRST_REF(docRoot);
	bwidth = XNODE_GET_REF_BWIDTH(docRoot);
	for (i = 0; i < docRoot->children; i++)
	{
		child = (XMLNodeHdr) ((char *) docRoot - readXMLNodeOffset(&childOffPtr, bwidth, true));

		if (child->kind == XNTNODE_TEMPLATE)
		{
			break;
		}
	}

	Assert(child != NULL && child->kind == XNTNODE_TEMPLATE);
	return (XMLCompNodeHdr) child;
}

/*
 * Write 'node' and it's descendants to storage, starting at '*output'.
 * When finished, '*output' points to the first byte following the storage.
 */
static void
writeXNodeInternal(XNodeInternal node, char **output, XMLNodeOffset *root)
{
	XMLNodeOffset *offs = NULL;
	char	   *start = *output;
	unsigned short i;
	unsigned int childCount = 0;
	XNodeListItem *childItem = NULL;
	XNodeInternal *childrenTmp = NULL;
	bool		mustReorder = false;

	if (node->node->kind == XMLNODE_ELEMENT || node->node->kind == XNTNODE_TEMPLATE)
	{
		childCount = node->children.position;
	}

	if (childCount > 0)
	{
		Assert(node->node->kind == XMLNODE_ELEMENT || node->node->kind == XNTNODE_TEMPLATE);

		offs = (XMLNodeOffset *) palloc(childCount * sizeof(XMLNodeOffset));

		if (node->node->kind == XMLNODE_ELEMENT)
		{
			bool		seenNonAttr = false;

			childrenTmp = (XNodeInternal *) palloc(childCount * sizeof(XNodeInternal));
			childItem = (XNodeListItem *) (&node->children)->content;

			for (i = 0; i < childCount; i++)
			{
				XNodeInternal child = (XNodeInternal) childItem->value.singlePtr;

				if (child->node->kind != XMLNODE_ATTRIBUTE)
				{
					seenNonAttr = true;
				}

				if (!mustReorder && seenNonAttr && child->node->kind == XMLNODE_ATTRIBUTE)
				{
					mustReorder = true;
				}

				childrenTmp[i] = child;
				childItem++;
			}
		}
	}

	if (mustReorder)
	{
		unsigned short i,
					nonAttr;

		/*
		 * Ensure that attributes are at the lowest positions in the reference
		 * list.
		 */

		/* Start with finding the first non-attribute node. */
		for (i = 0; i < childCount; i++)
		{
			if (childrenTmp[i]->node->kind != XMLNODE_ATTRIBUTE)
			{
				break;
			}
		}
		nonAttr = i++;

		/*
		 * If attribute is found anywhere after that, move to position
		 * immediately following the last correctly located (i.e. near the
		 * reference array beginning) attribute.
		 */
		for (; i < childCount; i++)
		{
			if (childrenTmp[i]->node->kind == XMLNODE_ATTRIBUTE)
			{
				XNodeInternal attr;
				XNodeInternal *src,
						   *dst;
				unsigned int size = (i - nonAttr) * sizeof(XNodeInternal);

				attr = childrenTmp[i];
				src = childrenTmp + nonAttr;
				dst = src + 1;
				memmove(dst, src, size);
				childrenTmp[nonAttr] = attr;

				nonAttr++;
			}
		}
	}

	if (node->node->kind != XMLNODE_ELEMENT)
	{
		/*
		 * 'childrenTmp' is not initialized, we're iterating the children
		 * first time.
		 */
		childItem = (XNodeListItem *) (&node->children)->content;
	}

	for (i = 0; i < childCount; i++)
	{
		XNodeInternal child;
		XMLNodeOffset root = 0;

		if (childrenTmp != NULL)
		{
			child = childrenTmp[i];
		}
		else
		{
			child = (XNodeInternal) childItem->value.singlePtr;
			childItem++;
		}

		offs[i] = *output - start;
		writeXNodeInternal(child, output, &root);
		offs[i] += root;
	}

	if (childrenTmp != NULL)
	{
		pfree(childrenTmp);
	}

	if (node->node->kind == XMLNODE_ELEMENT || node->node->kind == XNTNODE_TEMPLATE)
	{
		XMLCompNodeHdr compNode = (XMLCompNodeHdr) *output;
		char	   *name;

		/* The header */
		if (node->node->kind == XMLNODE_ELEMENT)
		{
			memcpy(*output, node->node, sizeof(XMLCompNodeHdr));
		}
		else
		{
			compNode->common.kind = XMLNODE_DOC_FRAGMENT;
			compNode->common.flags = 0;
		}
		compNode->children = childCount;

		/*
		 * TODO adjust flags if appropriate. 'empty element' flag shouldn't be
		 * the case. Valid value is expected in the header just copied.
		 */

		*output += sizeof(XMLCompNodeHdrData);

		/* References */
		if (childCount > 0)
		{
			XMLNodeOffset relOffMax = (char *) compNode - (start + offs[0]);
			char		bwidth = getXMLNodeOffsetByteWidth(relOffMax);

			for (i = 0; i < childCount; i++)
			{
				XMLNodeOffset offRel = (char *) compNode - (start + offs[i]);

				writeXMLNodeOffset(offRel, output, bwidth, true);
			}
		}

		if (node->node->kind == XMLNODE_ELEMENT)
		{
			XMLCompNodeHdr elNode = (XMLCompNodeHdr) node->node;

			/* Element name */
			name = XNODE_ELEMENT_NAME(elNode);
			strcpy(*output, name);
			*output += strlen(name) + 1;
		}
		*root = (char *) compNode - start;
	}
	else
	{
		unsigned int size = getXMLNodeSize(node->node, false);

		memcpy(*output, node->node, size);
		*output += size;
		*root = 0;
	}

	if (offs != NULL)
	{
		pfree(offs);
	}
}

static XPathExpression
substituteParameters(XPathExprState exprState, XPathExpression expression, XPathExprOperandValue paramValues,
					 unsigned short *paramMap)
{
	unsigned short i;
	XPathExpression result = (XPathExpression) palloc(expression->common.size);
	XPathOffset *varOffPtr = (XPathOffset *) ((char *) result + sizeof(XPathExpressionData));

	memcpy(result, expression, expression->common.size);

	for (i = 0; i < result->variables; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) ((char *) result + *varOffPtr);

		if (opnd->common.type == XPATH_OPERAND_PARAMETER)
		{
			unsigned short valueId;
			XPathExprOperandValue value;
			XMLCompNodeHdr fragment = NULL;
			XPathExprGenericValue *valDst;

			valueId = paramMap[opnd->value.v.paramId];
			value = paramValues + valueId;

			/*
			 * 'negative' is not set, that of the source expression must be
			 * preserved.
			 */
			opnd->value.castToNumber = value->castToNumber;
			opnd->value.isNull = value->isNull;
			opnd->value.type = value->type;

			/*
			 * Document fragment has to be turned into node set.
			 *
			 * This only makes sense if the operand contains exactly one node.
			 * There's no known case where array of document fragments could
			 * be constructed.
			 */

			if (!value->isNull && value->type == XPATH_VAL_NODESET && value->v.nodeSet.count == 1)
			{
				XMLNodeHdr	node = getXPathOperandValue(exprState, value->v.nodeSet.nodes.nodeId, XPATH_VAR_NODE_SINGLE);

				if (node->kind == XMLNODE_DOC_FRAGMENT)
				{
					fragment = (XMLCompNodeHdr) node;
				}
			}

			valDst = &opnd->value.v;

			if (fragment != NULL)
			{
				XPathNodeSet ns = &valDst->nodeSet;
				char	   *refPtr = XNODE_FIRST_REF(fragment);
				char		bwidth = XNODE_GET_REF_BWIDTH(fragment);
				unsigned short j;
				XMLNodeHdr *children;

				Assert(fragment->children > 1);
				children = (XMLNodeHdr *) palloc(fragment->children * sizeof(XMLNodeHdr));
				ns->isDocument = false;
				ns->count = fragment->children;
				for (j = 0; j < fragment->children; j++)
				{
					XMLNodeOffset offRel;
					XMLNodeHdr	child;

					offRel = readXMLNodeOffset(&refPtr, bwidth, true);
					child = (XMLNodeHdr) ((char *) fragment - offRel);
					children[j] = child;
				}
				ns->nodes.arrayId = getXPathOperandId(exprState, (void *) children, XPATH_VAR_NODE_ARRAY);
			}
			else
			{
				XPathExprGenericValue *valSrc;

				valSrc = &value->v;
				memcpy(valDst, valSrc, sizeof(XPathExprGenericValue));
			}

		}
		varOffPtr++;
	}
	return result;

}

static bool
whitespacesOnly(char *str)
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