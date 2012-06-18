/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"

#include "xpath.h"

static char *getEmptyString(void);

/*
 * IMPORTANT
 *
 * The functions are ordered by argument count: first function having no arguments, then
 * those having non-zero argument count. This is especially important where function can
 * have zero or more arguments (e.g. name()). XPath parser does rely on such ordering in these cases.
 *
 * Where the number of arguments is equal, the functions are ordered by return type in this order:
 * boolean, number, string, node set.
 *
 * If some function
 */
XPathFunctionData xpathFunctions[] = {
	{
		XPATH_FUNC_TRUE,
		"true", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathTrue},
		XPATH_VAL_BOOLEAN, false
	},
	{
		XPATH_FUNC_FALSE,
		"false", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathFalse},
		XPATH_VAL_BOOLEAN, false
	},
	{
		XPATH_FUNC_POSITION,
		"position", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathPosition},
		XPATH_VAL_NUMBER, true
	},
	{
		XPATH_FUNC_LAST,
		"last", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathLast},
		XPATH_VAL_NUMBER, true
	},
	{
		XPATH_FUNC_NAME_NOARG,
		"name", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathNameNoArgs},
		XPATH_VAL_STRING, true
	},
	{
		XPATH_FUNC_CONTAINS,
		"contains", 2,
		{XPATH_VAL_STRING, XPATH_VAL_STRING, 0, 0}, false,
		{.args = xpathContains},
		XPATH_VAL_BOOLEAN, false
	},
	{
		XPATH_FUNC_BOOLEAN,
		"boolean", 1,
		{XPATH_VAL_OBJECT, 0, 0, 0}, false,
		{.args = xpathBoolean},
		XPATH_VAL_BOOLEAN, false
	},
	{
		XPATH_FUNC_NUMBER,
		"number", 1,
		{XPATH_VAL_OBJECT, 0, 0, 0}, false,
		{.args = xpathNumber},
		XPATH_VAL_NUMBER, false
	},
	{
		XPATH_FUNC_STRING,
		"string", 1,
		{XPATH_VAL_OBJECT, 0, 0, 0}, false,
		{.args = xpathString},
		XPATH_VAL_STRING, false
	},
	{
		XPATH_FUNC_NAME,
		"name", 1,
		{XPATH_VAL_NODESET, 0, 0, 0}, false,
		{.args = xpathName},
		XPATH_VAL_STRING, false
	},
	{
		XPATH_FUNC_STARTS_WITH,
		"starts-with", 2,
		{XPATH_VAL_STRING, XPATH_VAL_STRING, 0, 0}, false,
		{.args = xpathStartsWith},
		XPATH_VAL_BOOLEAN, false
	},
	{
		XPATH_FUNC_COUNT,
		"count", 1,
		{XPATH_VAL_NODESET, 0, 0, 0}, false,
		{.args = xpathCount},
		XPATH_VAL_NUMBER, false
	},
	{
		XPATH_FUNC_CONCAT,
		"concat", 2,
		{XPATH_VAL_STRING, XPATH_VAL_STRING, 0, 0}, true,
		{.args = xpathConcat},
		XPATH_VAL_STRING, false
	}
};

void
xpathTrue(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	result->type = XPATH_VAL_BOOLEAN;
	result->isNull = false;
	result->v.boolean = true;
}

void
xpathFalse(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	result->type = XPATH_VAL_BOOLEAN;
	result->isNull = false;
	result->v.boolean = false;
}

void
xpathPosition(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	XMLScanOneLevel scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);

	result->type = XPATH_VAL_NUMBER;
	result->isNull = false;
	result->v.num = scanLevel->contextPosition;
}

void
xpathLast(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	XMLScanOneLevel scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);

	if (!scanLevel->contextSizeKnown)
	{
		XMLCompNodeHdr parent = scanLevel->parent;
		char	   *refPtr = scanLevel->nodeRefPtr;
		unsigned short sblLeft = scanLevel->siblingsLeft;
		XPathElement xpEl = XPATH_CURRENT_LEVEL(xscan);
		unsigned short i = 0;


		scanLevel->contextSize = scanLevel->contextPosition;

		while (sblLeft > 0)
		{
			XMLNodeOffset nodeOff;
			XMLNodeHdr	node;

			nodeOff = readXMLNodeOffset(&refPtr, XNODE_GET_REF_BWIDTH(parent), true);

			/*
			 * The first node has to be skipped because its the context node
			 * for the function.
			 */
			if (i > 0)
			{
				node = (XMLNodeHdr) ((char *) parent - nodeOff);

				if (node->kind == XMLNODE_ELEMENT)
				{
					XMLCompNodeHdr ctxElement = (XMLCompNodeHdr) node;

					if (strcmp(xpEl->name, XNODE_ELEMENT_NAME(ctxElement)) == 0)
					{
						scanLevel->contextSize++;
					}
				}
			}
			sblLeft--;
			i++;
		}


		scanLevel->contextSizeKnown = true;
	}
	result->type = XPATH_VAL_NUMBER;
	result->isNull = false;
	result->v.num = scanLevel->contextSize;
}

void
xpathNameNoArgs(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	XMLScanOneLevel scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);
	XMLCompNodeHdr eh = scanLevel->parent;
	XMLCompNodeHdr contextNode = (XMLCompNodeHdr) ((char *) eh -
												   readXMLNodeOffset(&scanLevel->nodeRefPtr, XNODE_GET_REF_BWIDTH(eh), false));
	char	   *elName,
			   *elNameCopy;

	Assert(contextNode->common.kind == XMLNODE_ELEMENT);
	elName = XNODE_ELEMENT_NAME(contextNode);
	elNameCopy = (char *) palloc(strlen(elName) + 1);
	strcpy(elNameCopy, elName);
	result->type = XPATH_VAL_STRING;
	result->isNull = false;
	result->v.stringId = getXPathOperandId(exprState, elNameCopy, XPATH_VAR_STRING);
}

/*
 * Even though types are checked at XPath parse time, each argument needs to be cast to the target type.
 * The parser excludes casts that never work (specifically: non-node to nodeset), but it doesn't know
 * if for example location path will produce string or number.
 */

void
xpathBoolean(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	XPathExprOperandValue src = args;

	castXPathExprOperandToBool(exprState, src, result);
}

void
xpathNumber(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	XPathExprOperandValue src = args;

	castXPathExprOperandToNum(exprState, src, result, true);
}

void
xpathString(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	XPathExprOperandValue src = args;

	if (src->isNull)
	{
		char	   *emptyStr = getEmptyString();

		result->v.stringId = getXPathOperandId(exprState, emptyStr, XPATH_VAR_STRING);
		result->type = XPATH_VAL_STRING;
		result->isNull = false;
		return;
	}

	castXPathExprOperandToStr(exprState, src, result);
}

void
xpathName(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
		  XPathExprOperandValue result)
{

	XPathNodeSet nodeSet;
	XMLNodeHdr	node;
	char	   *resStr,
			   *resStrCopy;

	Assert(nargs == 1);
	result->type = XPATH_VAL_STRING;
	result->isNull = false;

	nodeSet = &args->v.nodeSet;
	if (nodeSet->isDocument || nodeSet->count == 0)
	{
		resStr = getEmptyString();
		result->v.stringId = getXPathOperandId(exprState, resStr, XPATH_VAR_STRING);
		return;
	}

	if (nodeSet->count == 1)
	{
		node = getXPathOperandValue(exprState, nodeSet->nodes.nodeId, XPATH_VAR_NODE_SINGLE);
	}
	else
	{
		XMLNodeHdr *nodes = getXPathOperandValue(exprState, nodeSet->nodes.arrayId, XPATH_VAR_NODE_ARRAY);

		node = nodes[0];
	}

	if (node->kind == XMLNODE_ELEMENT)
	{
		resStr = XNODE_ELEMENT_NAME((XMLCompNodeHdr) node);
	}
	else if (node->kind == XMLNODE_ATTRIBUTE)
	{
		resStr = XNODE_CONTENT(node);
	}
	else
	{
		resStr = getEmptyString();
		result->v.stringId = getXPathOperandId(exprState, resStr, XPATH_VAR_STRING);
		return;
	}

	resStrCopy = (char *) palloc(strlen(resStr) + 1);
	strcpy(resStrCopy, resStr);
	result->v.stringId = getXPathOperandId(exprState, resStrCopy, XPATH_VAR_STRING);
}

void
xpathStartsWith(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
				XPathExprOperandValue result)
{

	XPathExprOperandValueData containing;
	XPathExprOperandValueData start;
	char	   *containingStr,
			   *startStr;

	castXPathExprOperandToStr(exprState, args++, &containing);
	containingStr = getXPathOperandValue(exprState, containing.v.stringId, XPATH_VAR_STRING);
	castXPathExprOperandToStr(exprState, args, &start);
	startStr = getXPathOperandValue(exprState, start.v.stringId, XPATH_VAR_STRING);

	result->v.boolean = (strstr(containingStr, startStr) == containingStr);
	result->type = XPATH_VAL_BOOLEAN;
	result->isNull = false;
}

void
xpathCount(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	XPathNodeSet nodeSet = &args->v.nodeSet;

	result->type = XPATH_VAL_NUMBER;
	result->isNull = false;

	if (nodeSet->isDocument)
	{
		result->v.num = 1;
	}
	else
	{
		result->v.num = args->isNull ? 0 : nodeSet->count;
	}
}

void
xpathContains(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			  XPathExprOperandValue result)
{
	XPathExprOperandValueData argLeft,
				argRight;
	bool		leftEmpty,
				rightEmpty;
	char	   *argLeftStr = NULL,
			   *argRightStr = NULL;

	castXPathExprOperandToStr(exprState, args, &argLeft);
	if (!argLeft.isNull)
	{
		argLeftStr = (char *) getXPathOperandValue(exprState, argLeft.v.stringId, XPATH_VAR_STRING);
	}
	leftEmpty = (argLeft.isNull || strlen(argLeftStr) == 0);

	castXPathExprOperandToStr(exprState, args + 1, &argRight);
	if (!argRight.isNull)
	{
		argRightStr = (char *) getXPathOperandValue(exprState, argRight.v.stringId, XPATH_VAR_STRING);
	}
	rightEmpty = (argRight.isNull || strlen(argRightStr) == 0);

	result->type = XPATH_VAL_BOOLEAN;
	result->isNull = false;

	if (leftEmpty && rightEmpty)
	{
		result->v.boolean = true;
	}
	else if (leftEmpty || rightEmpty)
	{
		/*
		 * Right is the 'contained', left is 'containing'. Empty string always
		 * contained in non-empty.
		 */
		result->v.boolean = rightEmpty;
	}
	else
	{
		result->v.boolean = (strstr(argLeftStr, argRightStr) != NULL);
	}
}

void
xpathConcat(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			XPathExprOperandValue result)
{

	unsigned short i;
	StringInfoData out;
	XPathExprOperandValue currentArg = args;

	Assert(nargs > 1);

	out.maxlen = 32;
	initStringInfo(&out);

	for (i = 0; i < nargs; i++)
	{
		char	   *part;

		if (!currentArg->isNull)
		{
			XPathExprOperandValueData argStr;

			castXPathExprOperandToStr(exprState, currentArg, &argStr);
			part = (char *) getXPathOperandValue(exprState, argStr.v.stringId, XPATH_VAR_STRING);
			appendStringInfoString(&out, part);
		}
		currentArg++;
	}

	result->type = XPATH_VAL_STRING;
	result->isNull = false;
	result->v.stringId = getXPathOperandId(exprState, out.data, XPATH_VAR_STRING);
}

static char *
getEmptyString(void)
{
	char	   *result = (char *) palloc(sizeof(char));

	result[0] = '\0';
	return result;
}
