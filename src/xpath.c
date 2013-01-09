/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#include <math.h>

#include "postgres.h"
#include "fmgr.h"
#include "funcapi.h"
#include "catalog/pg_type.h"
#include "utils/array.h"
#include "utils/builtins.h"
#include "utils/lsyscache.h"

#include "xmlnode.h"
#include "xpath.h"
#include "xml_parser.h"
#include "xmlnode_util.h"

static void retrieveColumnPaths(XMLScanContext xScanCtx, ArrayType *pathsColArr, int columns);
static ArrayType *getResultArray(XMLScanContext ctx, XMLNodeOffset baseNodeOff);
static xpathval getXPathExprValue(XPathExprState exprState, xmldoc document, bool *notNull,
				  XPathExprOperandValue res);
static XPathValue getXPathValue(xpathval raw);
static char *getBoolValueString(bool value);
static char *getStorageForXPathValue(unsigned int totalSize, XPathValue *xpv, char **cursor);

/* The order must follow XPathValueType */
char	   *xpathValueTypes[] = {"boolean", "number", "string", "nodeset"};


PG_FUNCTION_INFO_V1(xpath_in);

Datum
xpath_in(PG_FUNCTION_ARGS)
{
	char	   *xpathStr;
	unsigned short exprOutPos;
	unsigned short pathCount = 0;
	XPathExpression expr;
	XPathParserStateData state;
	XMLNodeContainerData paramNames;
	XPath	   *paths;
	char	   *result;

	xpathStr = PG_GETARG_CSTRING(0);
	expr = (XPathExpression) palloc(XPATH_EXPR_BUFFER_SIZE);
	expr->needsContext = false;

	state.c = xpathStr;

	/*
	 * With other expression types (sub-expressions or function arg. list),
	 * it's assumed that we're at '(' or '[' when calling the
	 * parseXPathExpression() function. In this specific case we fake such
	 * state by setting the char width to 0 so that the first call of
	 * 'nextChar()' function doesn't move the current character.
	 */
	state.cWidth = 0;
	state.pos = 0;

	xmlnodeContainerInit(&paramNames);

	/*
	 * 'isSubExpr=false' will be passed to the parser, so the parser will also
	 * set the 'exprOutPos'.
	 */
	exprOutPos = 0;
	paths = (XPath *) palloc(XPATH_MAX_SUBPATHS * sizeof(XPath));
	parseXPathExpression(expr, &state, XPATH_TERM_NULL, NULL, (char *) expr,
				  &exprOutPos, false, false, paths, &pathCount, &paramNames);


	result = getXPathExpressionForStorage(expr, paths, pathCount, &paramNames,
										  true, NULL);
	xmlnodeContainerFree(&paramNames);
	PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(xpath_out);

Datum
xpath_out(PG_FUNCTION_ARGS)
{
	xpath		xp = (xpath) PG_GETARG_POINTER(0);
	XPathHeader xpathHdr = getXPathHeader(xp);
	XPathExpression expr = getXPathExpressionFromStorage(xpathHdr);
	char	  **paramNamesArray = getXPathParameterArray(xpathHdr);
	StringInfoData output;

	xnodeInitStringInfo(&output, 64);
	dumpXPathExpression(expr, xpathHdr, &output, true, paramNamesArray, false);

	if (paramNamesArray != NULL)
	{
		pfree(paramNamesArray);
	}
	PG_RETURN_POINTER(output.data);
}

PG_FUNCTION_INFO_V1(xpath_debug_print);

Datum
xpath_debug_print(PG_FUNCTION_ARGS)
{
	xpath		xp = (xpath) PG_GETARG_POINTER(0);
	XPathHeader xpathHdr = getXPathHeader(xp);
	XPathExpression expr = getXPathExpressionFromStorage(xpathHdr);
	char	  **paramNamesArray = getXPathParameterArray(xpathHdr);
	StringInfoData output;

	initStringInfo(&output);
	dumpXPathExpression(expr, xpathHdr, &output, true, paramNamesArray, true);

	if (expr->npaths > 0)
	{
		unsigned short i;

		appendStringInfoChar(&output, '\n');
		for (i = 0; i < xpathHdr->pathCount; i++)
		{
			appendStringInfo(&output, "\n\n");
			dumpLocationPath(xpathHdr, i, &output, paramNamesArray, true);
			appendStringInfoChar(&output, '\n');
		}
	}

	if (paramNamesArray != NULL)
	{
		pfree(paramNamesArray);
	}
	PG_RETURN_TEXT_P(cstring_to_text(output.data));
}


/*
 * 'varlena' - if true, then leave space for varlena header and start writing
 * at the first aligned position. Otherwise start writing at the first address.
 *
 * 'expr', 'locPaths' and all the paths contained in 'locPaths' are all pfree'd.
 * On the other hand, 'paramNames' and the strings it points to are preserved.
 * The reason is that templates need a single set of parameter names to be
 * referenced by all expressions.
 */
char *
getXPathExpressionForStorage(XPathExpression expr, XPath *locPaths,
					unsigned short locPathCount, XMLNodeContainer paramNames,
							 bool varlena, unsigned short *sizeOut)
{
	unsigned short size,
				paramCount = 0;
	char	  **parNamesArray = NULL;
	char	   *result,
			   *out;
	XPathHeader xpathHdr;
	unsigned int offset = varlena ? VARHDRSZ : 0;
	unsigned int resSize = offset + MAX_PADDING(XPATH_ALIGNOF_PATH_HDR) +
	sizeof(XPathHeaderData) + MAX_PADDING(XPATH_ALIGNOF_EXPR) +
	expr->common.size;

	if (locPathCount > 0)
	{
		unsigned short i;

		for (i = 0; i < locPathCount; i++)
		{
			XPath		path = locPaths[i];

			/*
			 * We need to store the path itself as well as its offset
			 * (reference)
			 */
			resSize += MAX_PADDING(XPATH_ALIGNOF_LOC_PATH) + path->size;

			/*
			 * The first offset (i == 0) will be saved inside the
			 * XPathHeaderData structure
			 */
			if (i > 0)
			{
				resSize += sizeof(XPathOffset);
			}
		}
	}

	if (paramNames != NULL)
	{
		if (paramNames->position > 0)
		{
			unsigned short i;
			XNodeListItem *item = paramNames->content;

			paramCount = paramNames->position;
			parNamesArray = (char **) palloc(paramCount * sizeof(char *));

			for (i = 0; i < paramCount; i++)
			{
				char	   *parName = (char *) item->value.singlePtr;

				parNamesArray[i] = parName;
				resSize += strlen(parName) + 1;
				item++;
			}
		}
	}

	result = (char *) palloc(resSize);
	out = (char *) TYPEALIGN(XPATH_ALIGNOF_PATH_HDR, (result + offset));
	xpathHdr = (XPathHeader) out;
	xpathHdr->pathCount = locPathCount;
	xpathHdr->paramCount = paramCount;
	xpathHdr->paramFirst = 0;

	out += sizeof(XPathHeaderData);

	if (locPathCount > 0)
	{
		/*
		 * Leave space for path references. (pathCount - 1) means that the
		 * array starts inside XPathHeaderData.
		 */
		out += (locPathCount - 1) * sizeof(XPathOffset);
	}

	out = (char *) TYPEALIGN(XPATH_ALIGNOF_EXPR, out);
	memcpy(out, expr, expr->common.size);
	out += expr->common.size;

	if (locPathCount > 0)
	{
		/* Save the paths */
		unsigned short i;

		for (i = 0; i < locPathCount; i++)
		{
			XPath		path = locPaths[i];

			out = (char *) TYPEALIGN(XPATH_ALIGNOF_LOC_PATH, out);
			memcpy(out, path, path->size);
			xpathHdr->paths[i] = out - (char *) xpathHdr;
			out += path->size;
			pfree(path);
		}
	}
	pfree(locPaths);
	pfree(expr);

	if (paramCount > 0)
	{
		unsigned short i;

		xpathHdr->paramFirst = out - (char *) xpathHdr;
		for (i = 0; i < paramCount; i++)
		{
			char	   *name = parNamesArray[i];

			strcpy(out, name);
			out += strlen(name) + 1;
		}
		pfree(parNamesArray);
		xpathHdr->paramCount = paramCount;
	}

	size = out - result;

	if (varlena)
		SET_VARSIZE(result, size);

	if (sizeOut != NULL)
		*sizeOut = size;
	return result;
}

XPath
getAbsoluteLocationXPath(XPathExpression expr, XPathHeader xpHdr)
{
	char	   *opPtr;
	XPathExprOperand operand;
	XPath		path;

	if (expr->members != 1 || expr->npaths == 0)
		elog(ERROR, "a single (absolute) location path expected");

	opPtr = (char *) expr + sizeof(XPathExpressionData) + XPATH_EXPR_VAR_MAX *
		sizeof(XPathOffset);
	opPtr = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, opPtr);
	operand = (XPathExprOperand) opPtr;

	/*
	 * Valid path may be enclosed in round brackets
	 */
	while (operand->common.type == XPATH_OPERAND_EXPR_SUB)
	{
		expr = (XPathExpression) operand;
		opPtr = (char *) expr + sizeof(XPathExpressionData);
		opPtr = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, opPtr);
		operand = (XPathExprOperand) opPtr;
	}

	if (operand->common.type != XPATH_OPERAND_PATH || expr->members != 1)
		elog(ERROR, "xpath expression can't be used as a base path");

	path = XPATH_HDR_GET_PATH(xpHdr, operand->value.v.path);

	if (path->relative)
		elog(ERROR, "base location path must be absolute");
	return path;
}


PG_FUNCTION_INFO_V1(xpath_single);

Datum
xpath_single(PG_FUNCTION_ARGS)
{
	xpath		xpathIn = (xpath) PG_GETARG_POINTER(0);
	XPathHeader xpathHdr = getXPathHeader(xpathIn);
	XPathExpression expr;
	XPathExprState exprState;
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(1);
	xpathval	result;
	XPathExprOperandValueData resData;
	bool		notNull;

	if (xpathHdr->paramCount > 0)
	{
		elog(ERROR, "function does not accept parameterized xpath expression");
	}

	expr = getXPathExpressionFromStorage(xpathHdr);

	if (expr->needsContext)
	{
		elog(ERROR, "one or more operands of the XPath expression require context");
	}
	exprState = prepareXPathExpression(expr, (XMLCompNodeHdr) XNODE_ROOT(doc), doc, xpathHdr, NULL);
	evaluateXPathExpression(exprState, exprState->expr, 0, &resData);
	result = getXPathExprValue(exprState, doc, &notNull, &resData);
	freeExpressionState(exprState);

	if (notNull)
	{
		PG_RETURN_POINTER(result);
	}
	else
	{
		PG_RETURN_NULL();
	}
}


PG_FUNCTION_INFO_V1(xpath_array);

/*
 * TODO Check if it's safe to store pointers to elements of
 * PG_GETARG_POINTER(2) across calls or if the parameter should be copied to
 * the 'fctx';
 */
Datum
xpath_array(PG_FUNCTION_ARGS)
{
	FuncCallContext *fctx;
	XMLScan		baseScan;
	XMLScanContext xScanCtx;

	if (SRF_IS_FIRSTCALL())
	{
		int		   *dimv;
		Oid			resultType;

		xpath		xpathBasePtr = (xpath) PG_GETARG_POINTER(0);
		XPathHeader xpHdrBase = getXPathHeader(xpathBasePtr);
		XPathExpression exprBase;
		XPath		xpathBase;

		ArrayType  *pathsColArr = PG_GETARG_ARRAYTYPE_P(1);
		xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(2);
		XMLCompNodeHdr docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);
		MemoryContext oldcontext;

		if (xpHdrBase->paramCount > 0)
		{
			elog(ERROR, "this function does not accept parameterized xpath expression");
		}
		exprBase = getXPathExpressionFromStorage(xpHdrBase);
		xpathBase = getAbsoluteLocationXPath(exprBase, xpHdrBase);

		fctx = SRF_FIRSTCALL_INIT();
		if (get_call_result_type(fcinfo, &resultType, NULL) != TYPEFUNC_SCALAR)
		{
			elog(ERROR, "function called in a context that doesn't accept scalar.");
		}
		oldcontext = MemoryContextSwitchTo(fctx->multi_call_memory_ctx);

		if (ARR_NDIM(pathsColArr) != 1)
		{
			elog(ERROR, " 1-dimensional array must be used to pass column xpaths");
		}
		dimv = ARR_DIMS(pathsColArr);

		/*
		 * First dimension is the only one and as such it's length of the
		 * array
		 */
		if (*dimv > XMLNODE_SET_MAX_COLS)
		{
			elog(ERROR, "maximum number of xpath columns is %u", XMLNODE_SET_MAX_COLS);
		}

		xScanCtx = (XMLScanContext) palloc(sizeof(XMLScanContextData));
		xScanCtx->baseScan = (XMLScan) palloc(sizeof(XMLScanData));
		initXMLScan(xScanCtx->baseScan, NULL, xpathBase, xpHdrBase, docRoot, doc, xpathBase->descendants > 0);

		xScanCtx->columns = *dimv;
		xScanCtx->colHeaders = (XPathHeader *) palloc(xScanCtx->columns * sizeof(XPathHeader));
		xScanCtx->colExpressions = (XPathExpression *) palloc(xScanCtx->columns * sizeof(XPathExpression));
		xScanCtx->outArrayType = resultType;

		/*
		 * Output element OID to be identified later - first time a not null
		 * value is found for any column xpath.
		 */
		xScanCtx->outElmType = InvalidOid;

		retrieveColumnPaths(xScanCtx, pathsColArr, *dimv);

		fctx->user_fctx = xScanCtx;
		MemoryContextSwitchTo(oldcontext);
	}

	fctx = SRF_PERCALL_SETUP();
	xScanCtx = (XMLScanContext) fctx->user_fctx;
	xScanCtx->colResults = NULL;
	xScanCtx->colResNulls = NULL;
	baseScan = xScanCtx->baseScan;

	if (baseScan->xpath->targNdKind == XMLNODE_DOC && !baseScan->done)
	{
		ArrayType  *result = getResultArray(xScanCtx, XNODE_ROOT_OFFSET(xScanCtx->baseScan->document));

		baseScan->done = true;

		if (result != NULL)
		{
			SRF_RETURN_NEXT(fctx, PointerGetDatum(result));
		}
	}
	if (!baseScan->done)
	{
		XMLNodeHdr	baseNode;

		/* Only element makes sense as a base context for column paths. */
		while ((baseNode = getNextXMLNode(baseScan)) != NULL)
		{
			if (baseNode->kind == XMLNODE_ELEMENT)
				break;
		}

		if (baseNode != NULL)
		{
			XMLNodeOffset baseNdOff = (char *) baseNode - VARDATA(xScanCtx->baseScan->document);
			ArrayType  *result = getResultArray(xScanCtx, baseNdOff);

			if (result != NULL)
			{
				SRF_RETURN_NEXT(fctx, PointerGetDatum(result));
			}
			else
			{
				baseScan->done = true;
			}
		}
		else
		{
			baseScan->done = true;
		}
	}
	if (baseScan->done)
	{
		finalizeXMLScan(baseScan);
		pfree(baseScan);

		pfree(xScanCtx->colExpressions);
		pfree(xScanCtx->colHeaders);
		if (xScanCtx->colResults != NULL)
		{
			pfree(xScanCtx->colResults);
		}
		if (xScanCtx->colResNulls != NULL)
		{
			pfree(xScanCtx->colResNulls);
		}
		pfree(xScanCtx);
		SRF_RETURN_DONE(fctx);
	}
	SRF_RETURN_DONE(fctx);
}

/*
 * Functions to cast operand value.
 * A separate instance is used for the target value in order to preserve the original value.
 * (As long as union is used, the cast would invalidate the source value if done in-place.)
 */

void
castXPathExprOperandToBool(XPathExprState exprState, XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst)
{
	valueDst->type = XPATH_VAL_BOOLEAN;
	valueDst->isNull = valueSrc->isNull;
	if (valueSrc->isNull)
	{
		valueDst->v.boolean = false;
		valueDst->isNull = false;
		return;
	}

	switch (valueSrc->type)
	{
		case XPATH_VAL_BOOLEAN:
			valueDst->v.boolean = valueSrc->v.boolean;
			break;

		case XPATH_VAL_NUMBER:
			valueDst->v.boolean = (!valueSrc->isNull && valueSrc->v.num != 0.0);
			break;

		case XPATH_VAL_STRING:
			valueDst->v.boolean = (!valueSrc->isNull &&
								   (strlen((char *) getXPathOperandValue(exprState, valueSrc->v.stringId, XPATH_VAR_STRING)) > 0));
			break;

		case XPATH_VAL_NODESET:
			if (valueSrc->v.nodeSet.isDocument)
			{
				/* Document is always a non-empty node set. */
				valueDst->v.boolean = true;
			}
			else
			{
				valueDst->v.boolean = (valueSrc->v.nodeSet.count > 0);
			}
			break;

		default:
			elog(ERROR, "unable to cast type %u to boolean", valueSrc->type);
			break;
	}
}

/*
* Neither null value nor NaN strings expected.
*/
void
castXPathExprOperandToNum(XPathExprState exprState, XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst,
						  bool raiseError)
{
	bool		isNumber = false;

	valueDst->type = XPATH_VAL_NUMBER;
	valueDst->isNull = valueSrc->isNull;

	if (valueSrc->isNull)
	{
		if (raiseError)
		{
			elog(ERROR, "null value can't be cast to a number");
		}
		else
		{
			return;
		}
	}

	switch (valueSrc->type)
	{
		case XPATH_VAL_BOOLEAN:
			valueDst->v.num = valueSrc->v.boolean ? 1.0 : 0.0;
			break;

		case XPATH_VAL_NUMBER:
			valueDst->v.num = valueSrc->v.num;
			break;

		case XPATH_VAL_STRING:
			valueDst->v.num = xnodeGetNumValue((char *) getXPathOperandValue(exprState, valueSrc->v.stringId,
								   XPATH_VAR_STRING), raiseError, &isNumber);
			valueDst->isNull = !isNumber;
			break;

		case XPATH_VAL_NODESET:
			{
				XPathExprOperandValueData valueTmp;

				castXPathExprOperandToStr(exprState, valueSrc, &valueTmp);
				valueDst->v.num = xnodeGetNumValue((char *) getXPathOperandValue(exprState, valueTmp.v.stringId,
								   XPATH_VAR_STRING), raiseError, &isNumber);
				valueDst->isNull = !isNumber;
				break;
			}

		default:
			elog(ERROR, "unable to cast type %u to number", valueSrc->type);
			break;
	}

	if (valueSrc->negative)
	{
		valueDst->v.num *= -1.0f;
	}
	valueDst->negative = false;
}

void
castXPathExprOperandToStr(XPathExprState exprState, XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst)
{
	valueDst->type = XPATH_VAL_STRING;
	valueDst->isNull = valueSrc->isNull;
	if (valueSrc->isNull)
	{
		return;
	}

	switch (valueSrc->type)
	{

		case XPATH_VAL_BOOLEAN:
			valueDst->v.stringId = getXPathOperandId(exprState, getBoolValueString(valueSrc->v.boolean),
													 XPATH_VAR_STRING);
			break;

		case XPATH_VAL_NUMBER:
			{
				Datum		strDatum = DirectFunctionCall1Coll(float8out, InvalidOid, Float8GetDatum(valueSrc->v.num));

				valueDst->v.stringId = getXPathOperandId(exprState, DatumGetCString(strDatum), XPATH_VAR_STRING);
			}
			break;

		case XPATH_VAL_STRING:
			valueDst->v.stringId = valueSrc->v.stringId;
			break;

		case XPATH_VAL_NODESET:
			{
				XPathNodeSet ns = &valueSrc->v.nodeSet;
				char	   *nodeStr;
				XMLNodeHdr *nodes;
				XMLNodeHdr	node;

				nodes = getArrayFromNodeSet(exprState, ns);

				/*
				 * If the node-set contains multiple nodes, only the first one
				 * is used (http://www.w3.org/TR/1999/REC-xpath-19991116/#sect
				 * ion-String-Functions)
				 */
				node = nodes[0];
				Assert(node->kind != XMLNODE_DOC_FRAGMENT);
				nodeStr = XNODE_GET_STRING(node);
				valueDst->v.stringId = getXPathOperandId(exprState, nodeStr, XPATH_VAR_STRING);
			}
			break;

		default:
			elog(ERROR, "unable to cast node of kind %u to string", valueSrc->type);
			break;
	}
}

bool
castXPathValToBool(XPathValue src)
{
	switch (src->type)
	{
		case XPATH_VAL_BOOLEAN:
			return src->v.booVal;

		case XPATH_VAL_NUMBER:
			return !isnan(src->v.numVal) && src->v.numVal != 0.0;

		case XPATH_VAL_STRING:
			return strlen(src->v.strVal) > 0;

		case XPATH_VAL_NODESET:
			/* Empty XPathValue is never created. */
			return true;

		default:
			elog(ERROR, "unable to cast path value of type %u to boolean", src->type);
			return false;
	}
}

float8
castXPathValToNum(XPathValue src)
{
	bool		isNull = false;

	switch (src->type)
	{
		case XPATH_VAL_BOOLEAN:
			return src->v.booVal ? 1.0 : 0.0;

		case XPATH_VAL_NUMBER:
			return src->v.numVal;

		case XPATH_VAL_STRING:
			return xnodeGetNumValue(src->v.strVal, true, &isNull);

		case XPATH_VAL_NODESET:
			{
				XMLNodeHdr	node = (XMLNodeHdr) ((char *) src + src->v.nodeSetRoot);
				char	   *nodeStr;

				if (node->kind == XMLNODE_DOC)
				{
					elog(ERROR, "document can't be cast to number");
				}

				nodeStr = XNODE_GET_STRING(node);
				return xnodeGetNumValue(nodeStr, true, &isNull);
			}

		default:
			elog(ERROR, "unable to cast path value of type %u to number", src->type);
			return 0.0;
	}
}

char *
castXPathValToStr(XPathValue src)
{
	Datum		strDatum;

	switch (src->type)
	{
		case XPATH_VAL_BOOLEAN:
			return getBoolValueString(src->v.booVal);

		case XPATH_VAL_NUMBER:
			strDatum = DirectFunctionCall1Coll(float8out, InvalidOid, Float8GetDatum(src->v.numVal));
			return DatumGetCString(strDatum);

		case XPATH_VAL_STRING:
			{
				char	   *str = NULL;

				str = (char *) palloc(strlen(src->v.strVal) + 1);
				strcpy(str, src->v.strVal);
				return str;
			}

		case XPATH_VAL_NODESET:
			{
				char	   *data = (char *) src;
				XMLNodeOffset rootNdOff = src->v.nodeSetRoot;

				/* The 'nodeSetRoot' counts from the start of 'src'. */
				return dumpXMLNode(data, (char *) src - data + rootNdOff,
								   rootNdOff, NULL, NULL);
			}
			break;

		default:
			elog(ERROR, "unrecognized type of xpath value: %u", src->type);
			return NULL;
	}
}

unsigned short
getXPathOperandId(XPathExprState exprState, void *value, XPathExprVar varKind)
{
	if (exprState->count[varKind] == exprState->countMax[varKind])
	{
		elog(DEBUG1, "reallocating cache for variable kind %u", varKind);
		allocXPathExpressionVarCache(exprState, varKind, XPATH_VAR_CACHE_DEF_SIZE, false);
	}

	switch (varKind)
	{
		case XPATH_VAR_STRING:
			exprState->strings[exprState->count[varKind]] = (char *) value;
			break;

		case XPATH_VAR_NODE_SINGLE:
			exprState->nodes[exprState->count[varKind]] = (XMLNodeHdr) value;
			break;

		case XPATH_VAR_NODE_ARRAY:
			exprState->nodeSets[exprState->count[varKind]] = (XMLNodeHdr *) value;
			break;

		default:
			elog(ERROR, "unrecognized kind of variable: %u", varKind);
			break;
	}
	return exprState->count[varKind]++;
}

void *
getXPathOperandValue(XPathExprState exprState, unsigned short id, XPathExprVar varKind)
{
	Assert(id < exprState->countMax[varKind]);

	switch (varKind)
	{
		case XPATH_VAR_STRING:
			return exprState->strings[id];

		case XPATH_VAR_NODE_SINGLE:
			return exprState->nodes[id];

		case XPATH_VAR_NODE_ARRAY:
			return exprState->nodeSets[id];

		default:
			elog(ERROR, "unrecognized kind of variable: %u", varKind);
			return NULL;
	}
}

/*
 * Returns array of XMLNodeHdr, whether array is actually stored
 * or a single node.
 */
XMLNodeHdr *
getArrayFromNodeSet(XPathExprState exprState, XPathNodeSet ns)
{
	if (ns->count == 0)
		return NULL;

	if (ns->count == 1)
		return &(exprState->nodes[ns->nodes.nodeId]);
	else
		return getXPathOperandValue(exprState, ns->nodes.arrayId, XPATH_VAR_NODE_ARRAY);
}

XPathHeader
getXPathHeader(xpath xpathValue)
{
	char	   *inData;

	inData = VARDATA(xpathValue);
	inData = (char *) TYPEALIGN(XPATH_ALIGNOF_PATH_HDR, inData);
	return (XPathHeader) inData;
}

XPathExpression
getXPathExpressionFromStorage(XPathHeader xpathHeader)
{
	char	   *resPtr = (char *) xpathHeader;

	resPtr += sizeof(XPathHeaderData);

	/*
	 * If there's only one offset, it's hidden in the structure. Otherwise we
	 * need to get behind the last one.
	 */
	if (xpathHeader->pathCount > 1)
		resPtr = (char *) &xpathHeader->paths[xpathHeader->pathCount];

	resPtr = (char *) TYPEALIGN(XPATH_ALIGNOF_EXPR, resPtr);
	return (XPathExpression) resPtr;
}

/*
 * Extract parameter names from the storage and return them as an array of
 * NULL-terminated strings.
 */
char	  **
getXPathParameterArray(XPathHeader xpathHeader)
{
	char	  **result = NULL;

	if (xpathHeader->paramCount > 0)
	{
		unsigned short i;
		char	   *cursor;

		result = (char **) palloc(xpathHeader->paramCount * sizeof(char *));
		cursor = (char *) xpathHeader + xpathHeader->paramFirst;

		for (i = 0; i < xpathHeader->paramCount; i++)
		{
			result[i] = cursor;
			cursor += strlen(cursor) + 1;
		}
	}

	return result;
}

/*
 * Get column paths from array.
 *
 * The paths are only checked and stored to the context.
 * Initialization of the 'column scans' is deferred to the point where
 * the base scan really finds something.
 */
static void
retrieveColumnPaths(XMLScanContext xScanCtx, ArrayType *pathsColArr, int columns)
{
	int			colIndex;
	Oid			arrTypeOid;
	int16		elTypLen,
				arrTypLen;
	bool		elByVal;
	char		elTypAlign;
	bool		isNull;
	Oid			elTypOid = ARR_ELEMTYPE(pathsColArr);

	/*
	 * Retrieve parameters for 'array_ref()'
	 */
	get_typlenbyvalalign(elTypOid, &elTypLen, &elByVal, &elTypAlign);
	arrTypeOid = get_array_type(elTypOid);
	arrTypLen = get_typlen(arrTypeOid);

	for (colIndex = 1; colIndex <= columns; colIndex++)
	{
		Datum		colPathD = array_ref(pathsColArr, 1, &colIndex, arrTypLen, elTypLen, elByVal,
										 elTypAlign, &isNull);
		xpath		colPathPtr;
		XPathHeader colPathHeader;

		if (isNull)
		{
			elog(ERROR, "column XPath must not be null");
		}
		colPathPtr = (xpath) DatumGetPointer(colPathD);
		colPathHeader = getXPathHeader(colPathPtr);

		if (colPathHeader->paramCount > 0)
		{
			elog(ERROR, "this function does not accept parameterized xpath expression");
		}
		xScanCtx->colHeaders[colIndex - 1] = colPathHeader;
		xScanCtx->colExpressions[colIndex - 1] = getXPathExpressionFromStorage(colPathHeader);
	}
}

/*
 * Returns array of nodes where i-th element is result of scan starting at
 * 'baseNodeOff', using 'ctx->colPaths[i]' as XPath expression.
 */
static ArrayType *
getResultArray(XMLScanContext ctx, XMLNodeOffset baseNodeOff)
{
	bool		notNull = false;
	unsigned short int i;
	ArrayType  *result = NULL;

	if (ctx->colResults == NULL)
	{
		ctx->colResults = (Datum *) palloc(ctx->columns * sizeof(Datum));
	}
	if (ctx->colResNulls == NULL)
	{
		ctx->colResNulls = (bool *) palloc(ctx->columns * sizeof(bool));
	}

	for (i = 0; i < ctx->columns; i++)
	{
		xpathval	colValue = NULL;
		xmldoc		doc = ctx->baseScan->document;
		XPathHeader xpHdr = ctx->colHeaders[i];
		XPathExpression expr = ctx->colExpressions[i];
		bool		colNotNull = false;
		XMLCompNodeHdr baseNode = (XMLCompNodeHdr) ((char *) VARDATA(doc) + baseNodeOff);
		XPathExprState exprState = prepareXPathExpression(expr, baseNode, doc, xpHdr, ctx->baseScan);
		XPathExprOperandValueData resData;

		evaluateXPathExpression(exprState, exprState->expr, 0, &resData);
		colValue = getXPathExprValue(exprState, doc, &colNotNull, &resData);
		freeExpressionState(exprState);

		ctx->colResults[i] = PointerGetDatum(colValue);
		ctx->colResNulls[i] = !colNotNull;
		notNull = (notNull || colNotNull);
	}

	if (notNull)
	{
		int			dims[1];
		int			lbs[1];

		if (ctx->outElmType == InvalidOid)
		{
			ctx->outElmType = get_element_type(ctx->outArrayType);
			Assert(ctx->outElmType != InvalidOid);
			get_typlenbyvalalign(ctx->outElmType, &ctx->outElmLen, &ctx->outElmByVal, &ctx->outElmalign);
		}
		dims[0] = ctx->columns;
		lbs[0] = 1;
		result = construct_md_array(ctx->colResults, ctx->colResNulls, 1, dims, lbs, ctx->outElmType, ctx->outElmLen,
									ctx->outElmByVal, ctx->outElmalign);
	}
	return result;
}

static xpathval
getXPathExprValue(XPathExprState exprState, xmldoc document, bool *notNull, XPathExprOperandValue res)
{
	xpathval	retValue = NULL;
	XPathValue	xpval = NULL;
	unsigned int resSize = 0;
	char	   *output = NULL;

	*notNull = false;

	if (res->isNull)
	{
		return retValue;
	}

	if (res->type == XPATH_VAL_NODESET)
	{
		if (res->v.nodeSet.isDocument)
		{
			/*
			 * Convert document to a node fragment. Make sure that XML
			 * declaration is removed.
			 */
			XMLNodeHdr	node;
			XMLNodeOffset rootOffNew;
			XMLNodeHdr	rootOrig;
			unsigned int sizeOrig,
						sizeNewEst;
			char	   *output,
					   *outTmp,
					   *after;

			rootOrig = XNODE_ROOT(document);
			sizeOrig = VARSIZE(document);
			sizeNewEst = (rootOrig->flags & XNODE_DOC_XMLDECL) ? sizeOrig - sizeof(XMLDeclData) : sizeOrig;

			/* The new root node may need padding. */
			sizeNewEst += MAX_PADDING(XNODE_ALIGNOF_XPATHVAL) + sizeof(XPathValueData) +
				MAX_PADDING(XNODE_ALIGNOF_COMPNODE);

			output = getStorageForXPathValue(sizeNewEst, &xpval, &outTmp);
			copyXMLNode(rootOrig, outTmp, false, &rootOffNew);
			node = (XMLNodeHdr) (outTmp + rootOffNew);
			node->kind = XMLNODE_DOC_FRAGMENT;
			node->flags = 0;
			xpval->type = XPATH_VAL_NODESET;
			xpval->v.nodeSetRoot = (char *) node - (char *) xpval;

			after = (char *) node + getXMLNodeSize(node, false);
			SET_VARSIZE(output, after - output);
			retValue = (xpathval) output;
			*notNull = true;
		}
		else
		{
			unsigned int j = res->v.nodeSet.count;
			XMLNodeHdr	firstNode = NULL;

			/*
			 * Only create the output value for non-empty set. Otherwise
			 * '*notNull' will remain 'false' and the caller won't expect any
			 * output.
			 */
			if (j > 0)
			{
				char	   *outTmp,
						   *after;

				*notNull = true;

				if (j == 1)
				{
					unsigned int nodeSize;
					XMLNodeOffset root;
					char	   *nodeCopyPtr;

					firstNode = (XMLNodeHdr) getXPathOperandValue(exprState,
						 res->v.nodeSet.nodes.nodeId, XPATH_VAR_NODE_SINGLE);
					nodeSize = getXMLNodeSize(firstNode, true);
					resSize = VARHDRSZ + 1 + MAX_PADDING(XNODE_ALIGNOF_XPATHVAL) +
						sizeof(XPathValueData) + MAX_PADDING(XNODE_ALIGNOF_COMPNODE) +
						nodeSize;
					output = getStorageForXPathValue(resSize, &xpval, &outTmp);
					copyXMLNode(firstNode, outTmp, false, &root);
					nodeCopyPtr = outTmp + root;
					xpval->type = XPATH_VAL_NODESET;
					xpval->v.nodeSetRoot = nodeCopyPtr - (char *) xpval;
					after = nodeCopyPtr + getXMLNodeSize((XMLNodeHdr) nodeCopyPtr, false);
				}
				else
				{
					/*
					 * Construct a document fragment from the list of nodes
					 */
					unsigned int k;
					unsigned int nodeSizeTotal = 0;
					char		bwidth;
					char	   *refTarget;
					XMLCompNodeHdr fragmentHdr;
					XMLNodeHdr *nodeArray;
					XMLNodeOffset *offsAbs,
								offAbsFrag;


					nodeArray = (XMLNodeHdr *) getXPathOperandValue(exprState,
						 res->v.nodeSet.nodes.arrayId, XPATH_VAR_NODE_ARRAY);

					for (k = 0; k < j; k++)
					{
						XMLNodeHdr	node;

						if (k == XMLNODE_MAX_CHILDREN)
						{
							elog(ERROR, "Maximum number of %u children exceeded for node document fragment.",
								 XMLNODE_MAX_CHILDREN);
						}
						node = nodeArray[k];
						nodeSizeTotal += getXMLNodeSize(node, true) +
							MAX_PADDING(XNODE_ALIGNOF_COMPNODE);
					}

					/*
					 * 'nodeSizeTotal' now equals to the maximum possible
					 * distance between parent (doc fragment) and its child.
					 */
					bwidth = getXMLNodeOffsetByteWidth(nodeSizeTotal);
					resSize = VARHDRSZ + 1 + MAX_PADDING(XNODE_ALIGNOF_XPATHVAL) +
						sizeof(XPathValueData) + nodeSizeTotal +
						MAX_PADDING(XNODE_ALIGNOF_COMPNODE) +
						sizeof(XMLCompNodeHdrData) + bwidth * j;
					output = getStorageForXPathValue(resSize, &xpval, &outTmp);
					offsAbs = (XMLNodeOffset *) palloc(j * sizeof(XMLNodeOffset));
					nodeArray = (XMLNodeHdr *) getXPathOperandValue(exprState,
						 res->v.nodeSet.nodes.arrayId, XPATH_VAR_NODE_ARRAY);

					/*
					 * Copy the nodes and remember how far each is from the
					 * beginning.
					 */
					for (k = 0; k < j; k++)
					{
						XMLNodeHdr	node;
						char	   *nodeCopyPtr;
						XMLNodeOffset root;

						node = nodeArray[k];
						copyXMLNode(node, outTmp, false, &root);
						nodeCopyPtr = outTmp + root;
						offsAbs[k] = nodeCopyPtr - output;
						outTmp = nodeCopyPtr + getXMLNodeSize((XMLNodeHdr) nodeCopyPtr, false);
					}

					fragmentHdr = (XMLCompNodeHdr) (TYPEALIGN(XNODE_ALIGNOF_COMPNODE, outTmp));
					fragmentHdr->common.kind = XMLNODE_DOC_FRAGMENT;
					fragmentHdr->common.flags = 0;
					XNODE_SET_REF_BWIDTH(fragmentHdr, bwidth);
					fragmentHdr->children = j;
					refTarget = XNODE_FIRST_REF(fragmentHdr);
					offAbsFrag = (char *) fragmentHdr - output;

					for (k = 0; k < j; k++)
					{
						XMLNodeOffset offRel;

						offRel = offAbsFrag - offsAbs[k];
						writeXMLNodeOffset(offRel, &refTarget, bwidth, true);
					}
					pfree(offsAbs);
					after = refTarget;

					xpval->type = XPATH_VAL_NODESET;
					xpval->v.nodeSetRoot = (char *) fragmentHdr - (char *) xpval;
				}
				SET_VARSIZE(output, after - output);
				retValue = (xpathval) output;
			}
		}
	}
	else
	{
		if (res->type == XPATH_VAL_NUMBER || res->type == XPATH_VAL_BOOLEAN)
		{
			resSize = VARHDRSZ + 1 + MAX_PADDING(XNODE_ALIGNOF_XPATHVAL) +
				sizeof(XPathValueData);
			output = getStorageForXPathValue(resSize, &xpval, NULL);

			xpval->type = res->type;

			if (res->type == XPATH_VAL_NUMBER)
			{
				xpval->v.numVal = res->v.num;
				if (res->negative)
				{
					xpval->v.numVal *= -1.0f;
				}
			}
			else
			{
				xpval->v.booVal = res->v.boolean;
			}
			*notNull = true;
		}
		else if (res->type == XPATH_VAL_STRING)
		{
			char	   *resStr = (char *) getXPathOperandValue(exprState, res->v.stringId, XPATH_VAR_STRING);
			unsigned int len = strlen(resStr);

			resSize = VARHDRSZ + 1 + MAX_PADDING(XNODE_ALIGNOF_XPATHVAL) +
				sizeof(XPathValueData) + len;
			output = getStorageForXPathValue(resSize, &xpval, NULL);

			strcpy(xpval->v.strVal, resStr);
			xpval->type = res->type;
			*notNull = true;
		}
		else
		{
			elog(ERROR, "unknown value type: %u", res->type);
		}
		SET_VARSIZE(output, resSize);
		retValue = (xpathval) output;
	}
	return retValue;
}

static XPathValue
getXPathValue(xpathval raw)
{
	char	   *inData;

	inData = VARDATA(raw);
	inData = (char *) TYPEALIGN(XNODE_ALIGNOF_XPATHVAL, inData);
	return (XPathValue) inData;
}

static char *
getBoolValueString(bool value)
{
	StringInfoData out;

	/* lenth of 6 chars: 'false' + '\0' */
	xnodeInitStringInfo(&out, 6);
	if (value)
	{
		appendStringInfo(&out, "%s", "true");
	}
	else
	{
		appendStringInfo(&out, "%s", "false");
	}
	return out.data;
}


PG_FUNCTION_INFO_V1(xpathval_in);

Datum
xpathval_in(PG_FUNCTION_ARGS)
{
	elog(ERROR, "not implemented");
}

PG_FUNCTION_INFO_V1(xpathval_out);

Datum
xpathval_out(PG_FUNCTION_ARGS)
{
	XPathValue	xpv = getXPathValue(PG_GETARG_VARLENA_P(0));
	char	   *result = castXPathValToStr(xpv);

	PG_RETURN_CSTRING(result);
}


PG_FUNCTION_INFO_V1(xpathval_to_bool);

Datum
xpathval_to_bool(PG_FUNCTION_ARGS)
{
	XPathValue	xpv = getXPathValue(PG_GETARG_VARLENA_P(0));
	bool		result = castXPathValToBool(xpv);

	PG_RETURN_BOOL(result);
}


PG_FUNCTION_INFO_V1(xpathval_to_float8);

Datum
xpathval_to_float8(PG_FUNCTION_ARGS)
{
	XPathValue	xpv = getXPathValue(PG_GETARG_VARLENA_P(0));
	float8		result = castXPathValToNum(xpv);

	PG_RETURN_FLOAT8(result);
}


PG_FUNCTION_INFO_V1(xpathval_to_numeric);

Datum
xpathval_to_numeric(PG_FUNCTION_ARGS)
{
	XPathValue	xpv = getXPathValue(PG_GETARG_VARLENA_P(0));
	float8		resFloat = castXPathValToNum(xpv);
	Datum		resDatum = DirectFunctionCall1Coll(float8_numeric, InvalidOid, Float8GetDatum(resFloat));

	PG_RETURN_DATUM(resDatum);
}


PG_FUNCTION_INFO_V1(xpathval_to_int4);

Datum
xpathval_to_int4(PG_FUNCTION_ARGS)
{
	XPathValue	xpv = getXPathValue(PG_GETARG_VARLENA_P(0));
	float8		resFloat = castXPathValToNum(xpv);
	Datum		resDatum = DirectFunctionCall1Coll(dtoi4, InvalidOid, Float8GetDatum(resFloat));

	PG_RETURN_DATUM(resDatum);
}


PG_FUNCTION_INFO_V1(xpathval_to_xmlnode);

Datum
xpathval_to_xmlnode(PG_FUNCTION_ARGS)
{
	xpathval	xpval = PG_GETARG_VARLENA_P(0);
	XPathValue	xpv = getXPathValue(xpval);

	if (xpv->type == XPATH_VAL_NODESET)
	{
		XMLNodeHdr	rootNode;
		xmlnode		result;

		rootNode = (XMLNodeHdr) ((char *) xpv + xpv->v.nodeSetRoot);
		result = (xmlnode) copyXMLNode(rootNode, NULL, true, NULL);
		PG_RETURN_POINTER(result);
	}
	else
	{
		elog(ERROR, "%s can't be cast to node", xpathValueTypes[xpv->type]);
		PG_RETURN_NULL();
	}
}

/*
 * Returns block of memory initialized for 'pathval' storage.
 *
 * 'xpv' is set to point to the XPathValue header
 * 'cursor' is set to address where the actual value should be placed.
 */
static char *
getStorageForXPathValue(unsigned int totalSize, XPathValue *xpv, char **cursor)
{
	char	   *output,
			   *outTmp;

	output = (char *) palloc(totalSize);
	outTmp = VARDATA(output);
	outTmp = (char *) TYPEALIGN(XNODE_ALIGNOF_XPATHVAL, outTmp);
	*xpv = (XPathValue) outTmp;
	outTmp += sizeof(XPathValueData);

	if (cursor != NULL)
	{
		*cursor = outTmp;
	}
	return output;
}
