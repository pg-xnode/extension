/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"

#include "xpath.h"

XPathFunctionData xpathFunctions[] = {
	{
		XPATH_FUNC_TRUE,
		"true", 0,
		{0, 0, 0, 0},
		{.noargs = xpathTrue},
		XPATH_VAL_BOOLEAN, true
	},
	{
		XPATH_FUNC_FALSE,
		"false", 0,
		{0, 0, 0, 0},
		{.noargs = xpathFalse},
		XPATH_VAL_BOOLEAN, true
	},
	{
		XPATH_FUNC_POSITION,
		"position", 0,
		{0, 0, 0, 0},
		{.noargs = xpathPosition},
		XPATH_VAL_NUMBER, true
	},
	{
		XPATH_FUNC_LAST,
		"last", 0,
		{0, 0, 0, 0},
		{.noargs = xpathLast},
		XPATH_VAL_NUMBER, true
	},
	{
		XPATH_FUNC_CONTAINS,
		"contains", 2,
		{XPATH_VAL_STRING, XPATH_VAL_STRING, 0, 0},
		{.args = xpathContains},
		XPATH_VAL_BOOLEAN, false
	},
	{
		XPATH_FUNC_COUNT,
		"count", 1,
		{XPATH_VAL_NODESET, 0, 0, 0},
		{.args = xpathCount},
		XPATH_VAL_NUMBER, false
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

extern void
xpathContains(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			  XPathExprOperandValue result)
{
	XPathExprOperandValueData argLeft,
				argRight;
	bool		leftEmpty,
				rightEmpty;
	char	   *argLeftStr = NULL,
			   *argRightStr = NULL;

	xpathValCastToStr(exprState, args, &argLeft);
	if (!argLeft.isNull)
	{
		argLeftStr = (char *) getXPathOperandValue(exprState, argLeft.v.stringId, XPATH_VAR_STRING);
	}
	leftEmpty = (argLeft.isNull || strlen(argLeftStr) == 0);

	xpathValCastToStr(exprState, args + 1, &argRight);
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
