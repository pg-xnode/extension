#include "postgres.h"

#include "xpath.h"

XPathFunctionData xpathFunctions[] = {
	{XPATH_FUNC_POSITION,
		"position", 0,
		{0, 0, 0, 0},
		NULL,
		XPATH_VAL_NUMBER, true
	},
	{XPATH_FUNC_CONTAINS,
		"contains", 2,
		{XPATH_VAL_STRING, XPATH_VAL_STRING, 0, 0},
		xpathContains,
		XPATH_VAL_BOOLEAN, false
	},
	{XPATH_FUNC_COUNT,
		"count", 1,
		{XPATH_VAL_NODESET, 0, 0, 0},
		xpathCount,
		XPATH_VAL_NUMBER, false
	}
};

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
