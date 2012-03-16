/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"

#include "xpath.h"

XPathFunctionData xpathFunctions[] = {
	{
		XPATH_FUNC_TRUE,
		"true", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathTrue},
		XPATH_VAL_BOOLEAN, true
	},
	{
		XPATH_FUNC_FALSE,
		"false", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathFalse},
		XPATH_VAL_BOOLEAN, true
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
xpathTrue(XMLScan xscan, XPathExprOperandValue result)
{
	result->type = XPATH_VAL_BOOLEAN;
	result->v.boolean = true;
}

void
xpathFalse(XMLScan xscan, XPathExprOperandValue result)
{
	result->type = XPATH_VAL_BOOLEAN;
	result->v.boolean = false;
}

void
xpathPosition(XMLScan xscan, XPathExprOperandValue result)
{
	XMLScanOneLevel scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);

	result->type = XPATH_VAL_NUMBER;
	result->v.num = scanLevel->contextPosition;
}

void
xpathLast(XMLScan xscan, XPathExprOperandValue result)
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
	result->v.num = scanLevel->contextSize;
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

	castXPathExprOperandToNum(exprState, src, result);
}

void
xpathString(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	XPathExprOperandValue src = args;

	if (src->isNull)
	{
		char	   *emptyStr = (char *) palloc(sizeof(char));

		emptyStr[0] = '\0';
		result->v.stringId = getXPathOperandId(exprState, emptyStr, XPATH_VAR_STRING);
		result->type = XPATH_VAL_STRING;
		result->isNull = false;
		return;
	}

	castXPathExprOperandToStr(exprState, src, result);
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
