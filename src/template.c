/*
 * Support for templates, such as XNT or XSL.
 *
 * Copyright (C) 2012-2013, Antonin Houska
 */

#include "postgres.h"

#include "template.h"
#include "xmlnode.h"
#include "xmlnode_util.h"

static int	paramNameComparator(const void *left, const void *right);
static Datum castParameterValue(Datum value, Oid sourceType, Oid targetType);

/*
 * Get template header out of template document root.
 */
XMLTemplateHeader
getXMLTemplateHeader(XMLCompNodeHdr docRoot)
{
	char	   *resData;

	Assert(docRoot->common.kind == XMLTEMPLATE_ROOT);

	resData = XNODE_ELEMENT_NAME(docRoot);

	/* XML declaration: currently just the structure size, no padding. */
	if (docRoot->common.flags & XNODE_DOC_XMLDECL)
		resData += sizeof(XMLDeclData);

	resData = (char *) TYPEALIGN(XNODE_ALIGNOF_TEMPL_HDR, resData);
	return (XMLTemplateHeader) resData;
}

/*
 * Get parameter values out of the input row.
 *
 *
 * 'parNameArray' - the array of parameter names that user passes.
 *
 * 'paraNameCount' - how many elements that array contains.
 *
 * 'templateParamCount' - how many parameters the template contains.
 *
 *	'paramMap' - receives array mapping template parameters to names of the
 *	parameters passed by user for substitution. Size of this array must not
 *	be less than 'templateParamCount'
 *
 *	Returns array of parameter names passed by user for substitution.
 */
XMLParamNameSorted *
getXMLTemplateParamNames(ArrayType *parNameArray, unsigned int templateParamCount,
						 char **templParNames, unsigned short *paramMap)
{
	XMLParamNameSorted *parNames = NULL;
	unsigned int parNameCount;

	if (ARR_NDIM(parNameArray) == 0)
	{
		parNameCount = 0;
	}
	else if (ARR_NDIM(parNameArray) == 1)
	{
		int		   *attrArrDims = ARR_DIMS(parNameArray);

		parNameCount = attrArrDims[0];
	}
	else
	{
		elog(ERROR, "parameter names must be passed in 1-dimensional array");
	}

	if (parNameCount != templateParamCount)
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
		XMLParamNameSorted *parNamesTmp;
		unsigned int i;

		elType = parNameArray->elemtype;
		arrType = get_array_type(elType);
		Assert(arrType != InvalidOid);
		arrTypLen = get_typlen(arrType);
		get_typlenbyvalalign(elType, &elTypLen, &elByVal, &elAlign);

		/*
		 * Get the parameter names from the array. There's no need to validate
		 * characters. The validation is performed when parsing the template,
		 * so invalid parameter names simply won't match.
		 */
		parNames = parNamesTmp = (XMLParamNameSorted *) palloc(parNameCount *
												 sizeof(XMLParamNameSorted));

		for (i = 0; i < parNameCount; i++)
		{
			int			subscr[1];
			Datum		elDatum;
			bool		elIsNull;
			char	   *parName;

			subscr[0] = i + 1;
			elDatum = array_ref(parNameArray, 1, subscr, arrTypLen, elTypLen, elByVal, elAlign, &elIsNull);
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
				qsort(parNames, parNameCount, sizeof(XMLParamNameSorted), paramNameComparator);

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

			/* Match the template parameters to those passed to the function. */
			for (i = 0; i < templateParamCount; i++)
			{
				XMLParamNameSorted key,
						   *matching;

				key.name = templParNames[i];
				matching = (XMLParamNameSorted *) bsearch(&key, parNames, parNameCount,
							sizeof(XMLParamNameSorted), paramNameComparator);

				if (matching != NULL)
				{
					/*
					 * 'i' is the order that template expressions use to
					 * reference parameter names (i. e. the order of given
					 * parameter in the list stored in template header).
					 *
					 * paramMap[i] says at which position user passes the
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

	return parNames;
}

char	  **
getXMLTemplateParamNamesFromStorage(XMLCompNodeHdr templateNode)
{
	XMLTemplateHeader templateHdr;
	char	  **result = NULL;

	templateHdr = getXMLTemplateHeader(templateNode);
	if (templateHdr->paramCount > 0)
	{
		unsigned short i;
		char	   *srcCursor;

		srcCursor = (char *) templateHdr + sizeof(XMLTemplateHeaderData);
		result = (char **) palloc(templateHdr->paramCount * sizeof(char *));
		for (i = 0; i < templateHdr->paramCount; i++)
		{
			result[i] = srcCursor;
			srcCursor += strlen(srcCursor) + 1;
		}
	}
	return result;
}

/*
 * Get parameter values out of the input row.
 *
 *
 * 'templateParamCount' - number of parameters the template requires.
 *
 * 'exprState' - variable cache to be used to evaluate template expressions.
 *
 * 'fnOid' - OID of the function that performs the substitution. It's needed
 * to identify type of the input row.
 *
 */
XPathExprOperandValue
getXMLTemplateParamValues(Datum row,
		unsigned int templateParamCount, XPathExprState exprState, Oid fnOid)
{
	Form_pg_proc procStruct;
	HeapTupleData tmpTup,
			   *tup;
	Oid			nodeOid;
	unsigned int i;
	HeapTupleHeader tupHdr = DatumGetHeapTupleHeader(row);
	Oid			tupType = HeapTupleHeaderGetTypeId(tupHdr);
	int32		tupTypmod = HeapTupleHeaderGetTypMod(tupHdr);
	TupleDesc	tupDesc = lookup_rowtype_tupdesc(tupType, tupTypmod);
	XPathExprOperandValue parValues;

	/*
	 * It must have already been verified that the template has the number of
	 * parameters equal to the number of parameter names passed by user.
	 */
	if (tupDesc->natts != templateParamCount)
	{
		elog(ERROR, "the number of parameter values must be equal to that of names");
	}

	/*
	 * As there are no fixed OIDs for our types, let's get it from catalog.
	 * 'node' is the return type.
	 */
	tup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fnOid));
	Assert(HeapTupleIsValid(tup));
	procStruct = (Form_pg_proc) GETSTRUCT(tup);
	nodeOid = procStruct->prorettype;
	ReleaseSysCache(tup);

	tmpTup.t_len = HeapTupleHeaderGetDatumLength(tupHdr);
	tmpTup.t_data = tupHdr;
	tup = &tmpTup;

	parValues = (XPathExprOperandValue) palloc(tupDesc->natts * sizeof(XPathExprOperandValueData));

	for (i = 0; i < tupDesc->natts; i++)
	{
		Oid			attTyp;
		Datum		attValue;
		bool		isnull = false;
		XPathExprOperandValue parValue;

		attTyp = tupDesc->attrs[i]->atttypid;
		attValue = heap_getattr(tup, i + 1, tupDesc, &isnull);
		parValue = parValues + i;

		if (isnull)
		{
			parValue->isNull = true;
			continue;
		}

		/*
		 * Set default values. 'negative' is not set because
		 * substituteParameters() ignores it anyway, in order to preserve sign
		 * that the source expression might contain.
		 */
		parValue->isNull = false;
		parValue->castToNumber = false;

		/* 'nodeOid' is not a constant so can't be used with 'switch'. */
		if (attTyp == nodeOid)
		{
			xmlnode		node = PG_DETOAST_DATUM(attValue);
			XMLNodeHdr	root = XNODE_ROOT(node);

			parValue->v.nodeSet.count = 1;
			parValue->v.nodeSet.nodes.nodeId = getXPathOperandId(exprState,
									   (void *) root, XPATH_VAR_NODE_SINGLE);
			parValue->type = XPATH_VAL_NODESET;
		}
		else
		{
			switch (attTyp)
			{
				case BOOLOID:
					parValue->v.boolean = DatumGetBool(
							  castParameterValue(attValue, attTyp, BOOLOID));
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
							elog(ERROR, "the following characters are is not allowed in text parameter: '<', '>', '&'.");
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
	return parValues;
}

XPathExpression
substituteXMLTemplateParams(XPathExprState exprState, XPathExpression expression, XPathExprOperandValue paramValues,
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
				unsigned short j = 0;
				XMLNodeHdr *children;
				XMLNodeIteratorData iterator;
				XMLNodeHdr	child;

				Assert(fragment->children > 1);

				children = (XMLNodeHdr *) palloc(fragment->children * sizeof(XMLNodeHdr));
				ns->count = fragment->children;

				initXMLNodeIterator(&iterator, fragment, true);

				while ((child = getNextXMLNodeChild(&iterator)) != NULL)
				{
					children[j++] = child;
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

/*
 * Attributes of 'ordinary' elements (i.e. those having neither 'xnt' nor 'xmlns' prefix) might reference parameters.
 * For example: '<section name="{$par1}"/>
 */
char *
preprocessXMLTemplateAttrValues(XNodeListItem *attrOffsets, unsigned short attrCount, char *parserOutput,
						  unsigned int *outSize, XMLNodeContainer paramNames,
								bool acceptLocPaths)
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
	unsigned int outSizeMax = 0;

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
			unsigned short valueSizeNew = 0;

			valuesNew[i] = getAttrValueForXMLTemplate(attrValue, &valueSizeNew, paramNames,
													  acceptLocPaths);
			valueSizes[i] = valueSizeNew;

			outSizeMax += MAX_PADDING(XPATH_ALIGNOF_EXPR);
		}
		outSizeMax += fixedSizes[i] + valueSizes[i];
	}

	result = resCursor = (char *) palloc(outSizeMax);

	attrItem = attrOffsets;
	offNew = attrItem->value.singleOff;

	for (i = 0; i < attrCount; i++)
	{
		XMLNodeHdr	attr = attrNodes[i];
		XMLNodeHdr	attrNew;
		char	   *ptr,
				   *ptrAligned;
		unsigned int padding = 0;

		if (toReplace[i])
		{
			/*
			 * 'ptr' will now point to address immediately after attribute
			 * (node) header, i.e. where the attribute value would start w/o
			 * alignment.
			 */
			ptr = resCursor + fixedSizes[i];

			/*
			 * However the attribute value may be binary and may contain XPath
			 * expression.
			 */
			ptrAligned = (char *) TYPEALIGN(XPATH_ALIGNOF_EXPR, ptr);

			/*
			 * So the whole attribute (including its header) has to be shifted
			 * by 'padding' to ensure that the value is aligned and the node
			 * structure is preserved at the same time.
			 */
			padding = ptrAligned - ptr;
		}

		/*
		 * Header and attribute name is always copied from the original
		 * attribute.
		 */
		resCursor += padding;
		memcpy(resCursor, attr, fixedSizes[i]);
		attrNew = (XMLNodeHdr) resCursor;
		resCursor += fixedSizes[i];
		offNew += padding;

		/*
		 * The value is either modified (binary) or the original (plain
		 * string).
		 */
		if (toReplace[i])
		{
			attrNew->flags |= XNODE_ATTR_VALUE_BINARY;
			memcpy(resCursor, valuesNew[i], valueSizes[i]);
			pfree(valuesNew[i]);
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

	*outSize = resCursor - result;

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
 * If 'exprState' is NULL then the function returns source text of the contained expression(s).
 * Otherwise it uses 'exprState' to evaluate the expression(s).
 */
char *
dumpXMLAttrBinaryValue(char *binValue, char **paramNames, XPathExprOperandValue paramValues,
					   unsigned short *paramMap, XPathExprState exprState)
{
	StringInfoData output;
	char	   *cursor = binValue;
	unsigned short tokenCount,
				i;

	tokenCount = *((uint8 *) cursor);
	Assert(tokenCount > 0);
	xnodeInitStringInfo(&output, 32);

	cursor++;
	for (i = 0; i < tokenCount; i++)
	{
		bool		isExpr = *((bool *) cursor++);

		if (isExpr)
		{
			XPathHeader hdr;
			XPathExpression expr;

			cursor = (char *) TYPEALIGN(XPATH_ALIGNOF_EXPR, cursor);
			hdr = (XPathHeader) cursor;

			expr = getXPathExpressionFromStorage(hdr);
			cursor = (char *) expr;

			if (exprState == NULL)
			{
				appendStringInfoChar(&output, XNODE_CHAR_LBRKT_CUR);
				dumpXPathExpression(expr, hdr, &output, true, paramNames, false);
				appendStringInfoChar(&output, XNODE_CHAR_RBRKT_CUR);
			}
			else
			{
				XPathExprOperandValueData result,
							resultCast;
				XPathExpression exprCopy;

				exprCopy = substituteXMLTemplateParams(exprState, expr, paramValues, paramMap);
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

			/*
			 * If there are no location paths, the next expression is at the
			 * first (aligned) addres behind 'expr'. Otherwise we must skip
			 * the location paths too.
			 */
			if (hdr->pathCount == 0)
				cursor += expr->common.size;
			else
			{
				XPath		pathLast = XPATH_HDR_GET_PATH(hdr, hdr->paramCount - 1);

				cursor = (char *) pathLast + pathLast->size;
			}
		}
		else
		{
			appendStringInfoString(&output, cursor);
			cursor += strlen(cursor) + 1;
		}
	}
	return output.data;
}

/*
 * Returns a new attribute node where the value has been validated and flags set as appropriate.
 */
XMLNodeHdr
getNewXMLAttribute(char *name, uint8 flags, char *value, char decideOnDelim, unsigned int *size)
{
	XMLParserStateData state;
	char	   *valueValidated;
	bool		refs = false;
	XMLNodeHdr	result;
	char	   *c;
	char		delimiter;
	bool		seenApostrophe = false;
	bool		mixture = false;

	if (decideOnDelim)
	{
		delimiter = XNODE_CHAR_QUOTMARK;
	}
	else
	{
		delimiter = ((flags & XNODE_ATTR_APOSTROPHE) == 0) ? XNODE_CHAR_QUOTMARK : XNODE_CHAR_APOSTR;
	}

	initXMLParserState(&state, value, XMLNODE_ATTRIBUTE, NULL, NULL, NULL);

	/*
	 * If the value contains character references then the binary value will
	 * be different. That's why use 'valueNewChecked' in the next steps.
	 */
	valueValidated = readXMLAttValue(&state, true, &refs);
	c = valueValidated;

	while (*c != '\0')
	{
		if (decideOnDelim)
		{
			/*
			 * Check for mixture of apostrophes and quotation marks is not
			 * necessary, readXMLName() shouldn't accept it. However it's not
			 * a significant overhead to cross-check for such mixtures while
			 * determining the delimiter.
			 */
			if (*c == XNODE_CHAR_APOSTR)
			{
				if (delimiter == XNODE_CHAR_APOSTR)
				{
					/*
					 * We already switched the delimiter from quotation mark
					 * to apostrophe, so the quot. mark is there too.
					 */
					mixture = true;
					break;
				}
				seenApostrophe = true;
			}
			else if (*c == XNODE_CHAR_QUOTMARK)
			{
				/* Try to change the delimiter to apostrophe. */
				if (!seenApostrophe)
				{
					delimiter = XNODE_CHAR_APOSTR;
				}
				else
				{
					mixture = true;
					break;
				}
			}

		}
		else
		{
			/*
			 * If apostrophe is used as delimiter in the template, then it
			 * must not be inserted via parameter. The same is valid for
			 * quotation mark.
			 */
			if ((*c == XNODE_CHAR_QUOTMARK && ((flags & XNODE_ATTR_APOSTROPHE) == 0)) ||
				(*c == XNODE_CHAR_APOSTR && (flags & XNODE_ATTR_APOSTROPHE)))
			{
				elog(ERROR, "attribute value delimiter must not be used within the value");
			}
		}
		c += pg_utf_mblen((unsigned char *) c);
	}

	if (mixture)
	{
		elog(ERROR, "attribute value must not contain both quotation mark and apostrophe.");
	}

	*size = sizeof(XMLNodeHdr) + strlen(name) +strlen(valueValidated) + 2;
	result = (XMLNodeHdr) palloc(*size);
	result->kind = XMLNODE_ATTRIBUTE;
	c = XNODE_CONTENT(result);
	strcpy(c, name);
	c += strlen(name) + 1;
	strcpy(c, valueValidated);

	result->flags = getXMLAttributeFlags(valueValidated, refs, delimiter == XNODE_CHAR_APOSTR);
	finalizeXMLParserState(&state);
	return result;
}

XPathHeader
getXPathExpressionForXMLTemplate(char *src, unsigned int termFlags, XMLNodeContainer paramNames,
		unsigned short *endPos, unsigned short *outSize, bool acceptLocPaths)
{
	XPathExpression expr;
	XPath	   *locPaths;
	XPathOffset outPos = 0;
	XPathParserStateData state;
	unsigned short locPathCount = 0;
	XPathHeader result;

	state.c = src;
	state.cWidth = 0;
	state.pos = 0;

	expr = (XPathExpression) palloc(XPATH_EXPR_BUFFER_SIZE);
	expr->needsContext = false;
	locPaths = (XPath *) palloc(XPATH_EXPR_MAX_PATHS * sizeof(XPath));

	parseXPathExpression(expr, &state, termFlags, NULL, (char *) expr,
				 &outPos, false, false, locPaths, &locPathCount, paramNames);

	if (!acceptLocPaths && locPathCount > 0)
		elog(ERROR, "template attribute contains location path");

	/*
	 * No location path implies no context node. This is typical for XNT.
	 */
	if (!acceptLocPaths && expr->needsContext)
		elog(ERROR, "template attribute requires context node");

	if (endPos != NULL)
		*endPos = state.pos;

	/*
	 * NULL is passed for 'paramNames' because template stores them all
	 * together elsewhere (to ensure uniqueness), not with each expression.
	 */
	result = (XPathHeader) getXPathExpressionForStorage(expr, locPaths, locPathCount, NULL,
														false, outSize);
	return result;
}

char *
getAttrValueForXMLTemplate(char *attrValue, unsigned short *valueSize,
						   XMLNodeContainer paramNames, bool acceptLocPaths)
{
	unsigned short tokenCount = 0;
	char	   *tokens[XMLTEMPL_ATTR_VALUE_MAX_TOKENS];
	bool		tokenIsExpr[XMLTEMPL_ATTR_VALUE_MAX_TOKENS];
	unsigned int tokenSizes[XMLTEMPL_ATTR_VALUE_MAX_TOKENS];
	char	   *c = attrValue;
	unsigned short k;
	char	   *result,
			   *resCursor;
	unsigned int valueSizeMax = 0;

	/*
	 * Attribute value contains xpath expressions and therefore must be
	 * tokenized.
	 */

	/* Each iteration processes 1 token. */
	while (*c != '\0')
	{
		if (tokenCount == XMLTEMPL_ATTR_VALUE_MAX_TOKENS)
		{
			elog(ERROR, "template attribute value must not consist of more than %u tokens",
				 XMLTEMPL_ATTR_VALUE_MAX_TOKENS);
		}

		if (*c == XNODE_CHAR_LBRKT_CUR)
		{
			XPathHeader tokenExpr;
			unsigned short endPos = 0;
			unsigned short outSize = 0;

			c++;				/* Skip the left curly bracket. */

			/* Parse the xpath expression, ending with '}' */
			tokenExpr = getXPathExpressionForXMLTemplate(c, XPATH_TERM_RBRKT_CRL,
							  paramNames, &endPos, &outSize, acceptLocPaths);

			/*
			 * Even though XPathHeader is less restrictive, this alignment is
			 * needed to keep the contained expressions aligned during copying
			 * below.
			 */
			Assert(PointerIsAligned(tokenExpr, XPATH_ALIGNOF_EXPR));

			tokens[tokenCount] = (char *) tokenExpr;
			tokenSizes[tokenCount] = outSize;
			tokenIsExpr[tokenCount] = true;
			tokenCount++;
			valueSizeMax += MAX_PADDING(XPATH_ALIGNOF_EXPR) + outSize;
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
				valueSizeMax += size;
			}
		}
	}

	Assert(tokenCount > 0);

	/* Add space for the total number of tokens as well as type of each token. */
	valueSizeMax += tokenCount * sizeof(bool) + sizeof(uint8);

	/* Construct the tokenized binary value. */
	result = resCursor = (char *) palloc(valueSizeMax);
	*((uint8 *) resCursor) = tokenCount;
	resCursor++;

	for (k = 0; k < tokenCount; k++)
	{
		bool		isExpression = tokenIsExpr[k];

		*((bool *) resCursor) = isExpression;
		resCursor++;
		if (isExpression)
		{
			resCursor = (char *) TYPEALIGN(XPATH_ALIGNOF_EXPR, resCursor);
		}
		memcpy(resCursor, tokens[k], tokenSizes[k]);
		resCursor += tokenSizes[k];
		pfree(tokens[k]);
	}
	*valueSize = resCursor - result;
	return result;
}

static int
paramNameComparator(const void *left, const void *right)
{
	return strcmp(((XMLParamNameSorted *) left)->name, ((XMLParamNameSorted *) right)->name);
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
