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
xpathCount(unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
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
xpathContains(unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	XPathExprOperandValueData argLeft,
				argRight;
	bool		leftEmpty,
				rightEmpty;

	xpathValCastToStr(args, &argLeft);
	xpathValCastToStr(args + 1, &argRight);
	leftEmpty = (argLeft.isNull || strlen(argLeft.v.string.str) == 0);
	rightEmpty = (argRight.isNull || strlen(argRight.v.string.str) == 0);

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
		result->v.boolean = (strstr(argLeft.v.string.str, argRight.v.string.str) != NULL);
	}
	xpathStrFree(&argLeft);
	xpathStrFree(&argRight);
}
