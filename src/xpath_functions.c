/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#include "postgres.h"

#include "xpath.h"
#include "xmlnode_util.h"

#define XFUNC_GET_ARG_STRING(exprState, args, n)\
	(getXPathOperandValue(exprState, args[n].v.stringId, XPATH_VAR_STRING))
#define XFUNC_GET_ARG_NODESET(args, n) (&args[n].v.nodeSet)


#define XFUNC_SET_RESULT_BOOL(res, value) ((res)->v.boolean = (value))
#define XFUNC_SET_RESULT_NUMBER(res, value) ((res)->v.num = (value))
#define XFUNC_SET_RESULT_STRING(res, exprState, value)\
	(result->v.stringId = getXPathOperandId(exprState, value, XPATH_VAR_STRING))
#define XFUNC_SET_RESULT_NULL(res) ((res)->isNull = true)

static char *getEmptyString(void);
static void nameNoArgs(XMLScan xscan, XPathExprState exprState, bool local, XPathExprOperandValue result);
static void name(XPathExprState exprState, XPathNodeSet nodeSet, bool local, XPathExprOperandValue result);

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
		XPATH_FUNC_LOCAL_NAME_NOARG,
		"local-name", 0,
		{0, 0, 0, 0}, false,
		{.noargs = xpathLocalNameNoArgs},
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
		XPATH_FUNC_LOCAL_NAME,
		"local-name", 1,
		{XPATH_VAL_NODESET, 0, 0, 0}, false,
		{.args = xpathLocalName},
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
		XPATH_FUNC_SUM,
		"sum", 1,
		{XPATH_VAL_NODESET}, false,
		{.args = xpathSum},
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
	XFUNC_SET_RESULT_BOOL(result, true);
}

void
xpathFalse(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	XFUNC_SET_RESULT_BOOL(result, false);
}

void
xpathPosition(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	XMLScanOneLevel scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);

	XFUNC_SET_RESULT_NUMBER(result, scanLevel->contextPosition);
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

	XFUNC_SET_RESULT_NUMBER(result, scanLevel->contextSize);
}

void
xpathNameNoArgs(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	nameNoArgs(xscan, exprState, false, result);
}

void
xpathLocalNameNoArgs(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result)
{
	nameNoArgs(xscan, exprState, true, result);
}

/*
 * Even though types are checked at XPath parse time, each argument needs to be cast to the target type.
 * The parser excludes casts that never work (specifically: non-node to nodeset), but it doesn't know
 * if for example location path will produce string or number.
 */

void
xpathBoolean(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	/*
	 * 'args' is pointer to first element of an array that should contain no
	 * other elements.
	 */
	castXPathExprOperandToBool(exprState, args, result);
}

void
xpathNumber(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	castXPathExprOperandToNum(exprState, args, result, true);
}

void
xpathString(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	if (args->isNull)
	{
		char	   *emptyStr = getEmptyString();

		XFUNC_SET_RESULT_STRING(result, exprState, emptyStr);
		return;
	}

	castXPathExprOperandToStr(exprState, args, result);
}

void
xpathName(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
		  XPathExprOperandValue result)
{
	Assert(nargs == 1);

	name(exprState, &args->v.nodeSet, false, result);
}

void
xpathLocalName(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			   XPathExprOperandValue result)
{
	Assert(nargs == 1);

	name(exprState, &args->v.nodeSet, true, result);
}

void
xpathStartsWith(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
				XPathExprOperandValue result)
{
	char	   *containingStr,
			   *startStr;

	containingStr = XFUNC_GET_ARG_STRING(exprState, args, 0);
	startStr = XFUNC_GET_ARG_STRING(exprState, args, 1);
	XFUNC_SET_RESULT_BOOL(result, strstr(containingStr, startStr) == containingStr);
}

void
xpathCount(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result)
{
	XPathNodeSet nodeSet = &args->v.nodeSet;

	if (nodeSet->isDocument)
		XFUNC_SET_RESULT_NUMBER(result, 1);
	else
		XFUNC_SET_RESULT_NUMBER(result, args->isNull ? 0 : nodeSet->count);
}

void
xpathContains(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			  XPathExprOperandValue result)
{
	XPathExprOperandValue argLeft,
				argRight;
	bool		leftEmpty,
				rightEmpty;
	char	   *argLeftStr = NULL,
			   *argRightStr = NULL;

	argLeft = &args[0];
	argRight = &args[1];

	if (!argLeft->isNull)
		argLeftStr = XFUNC_GET_ARG_STRING(exprState, args, 0);

	leftEmpty = (argLeft->isNull || strlen(argLeftStr) == 0);

	if (!argRight->isNull)
		argRightStr = XFUNC_GET_ARG_STRING(exprState, args, 1);

	rightEmpty = (argRight->isNull || strlen(argRightStr) == 0);

	if (leftEmpty && rightEmpty)
		XFUNC_SET_RESULT_BOOL(result, true);
	else if (leftEmpty || rightEmpty)

		/*
		 * The right is the 'contained', left is 'containing'. Empty string
		 * always contained in non-empty.
		 */
		XFUNC_SET_RESULT_BOOL(result, rightEmpty);
	else
		XFUNC_SET_RESULT_BOOL(result, strstr(argLeftStr, argRightStr) != NULL);
}

void
xpathConcat(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			XPathExprOperandValue result)
{
	unsigned short i;
	StringInfoData out;

	Assert(nargs > 1);

	xnodeInitStringInfo(&out, 32);

	for (i = 0; i < nargs; i++)
	{
		char	   *part;

		if (!args[i].isNull)
		{
			part = XFUNC_GET_ARG_STRING(exprState, args, i);
			appendStringInfoString(&out, part);
		}
	}

	XFUNC_SET_RESULT_STRING(result, exprState, out.data);
}

static char *
getEmptyString(void)
{
	char	   *result = (char *) palloc(sizeof(char));

	result[0] = '\0';
	return result;
}

static void
nameNoArgs(XMLScan xscan, XPathExprState exprState, bool local, XPathExprOperandValue result)
{
	if (xscan->state != NULL)
	{
		XMLScanOneLevel scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);
		XMLCompNodeHdr eh = scanLevel->parent;
		XMLNodeOffset ctxNdOffRel = readXMLNodeOffset(&scanLevel->nodeRefPtr,
											XNODE_GET_REF_BWIDTH(eh), false);
		XMLCompNodeHdr contextNode = (XMLCompNodeHdr) ((char *) eh - ctxNdOffRel);
		char	   *elName = XNODE_ELEMENT_NAME(contextNode);
		char	   *colon,
				   *elNameCopy;

		Assert(contextNode->common.kind == XMLNODE_ELEMENT);

		if (local && ((colon = strchr(elName, XNODE_CHAR_COLON)) != NULL))
		{
			elName = colon + 1;
		}

		elNameCopy = (char *) palloc(strlen(elName) + 1);
		strcpy(elNameCopy, elName);
		XFUNC_SET_RESULT_STRING(result, exprState, elNameCopy);
	}
	else
	{
		/*
		 * We should end up here if '/' is the base path. name() is not
		 * defined in such a contect.
		 */
		result->isNull = true;
	}
}

static void
name(XPathExprState exprState, XPathNodeSet nodeSet, bool local, XPathExprOperandValue result)
{
	XMLNodeHdr	node;
	char	   *nameStr,
			   *colon;


	if (nodeSet->isDocument || nodeSet->count == 0)
	{
		nameStr = NULL;
	}
	else
	{
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
			nameStr = XNODE_ELEMENT_NAME((XMLCompNodeHdr) node);
		}
		else if (node->kind == XMLNODE_ATTRIBUTE)
		{
			nameStr = XNODE_CONTENT(node);
		}
		else
		{
			nameStr = NULL;
		}
	}

	if (local && nameStr != NULL && ((colon = strchr(nameStr, XNODE_CHAR_COLON)) != NULL))
	{
		nameStr = colon + 1;
	}

	if (nameStr == NULL)
	{
		nameStr = getEmptyString();
		XFUNC_SET_RESULT_STRING(result, exprState, nameStr);
	}
	else
	{
		char	   *nameStrCopy;

		nameStrCopy = (char *) palloc(strlen(nameStr) + 1);
		strcpy(nameStrCopy, nameStr);
		XFUNC_SET_RESULT_STRING(result, exprState, nameStrCopy);
	}
}

extern void
xpathSum(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
		 XPathExprOperandValue result)
{
	XPathNodeSet nodeSet = XFUNC_GET_ARG_NODESET(args, 0);
	XMLNodeHdr *nodes = getArrayFromNodeSet(exprState, nodeSet);
	unsigned int i;
	float8		sum = 0.0f;

	for (i = 0; i < nodeSet->count; i++)
	{
		char	   *nodeStr = XNODE_GET_STRING(nodes[i]);
		bool		isNum = false;
		double		numValue = xnodeGetNumValue(nodeStr, false, &isNum);

		if (isNum)
			sum += numValue;
		else
		{
			result->isNull = true;
			XFUNC_SET_RESULT_NULL(result);
			return;
		}
	}

	XFUNC_SET_RESULT_NUMBER(result, sum);
}
