/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"
#include "mb/pg_wchar.h"

#include "xmlnode.h"
#include "xpath.h"

static void insertSubexpression(XPathExprOperand operand, XPathExprOperatorIdStore ** opIdPtr,
		XPathExpression exprTop, unsigned short blockSize, bool varsShiftAll,
					char *output, unsigned short *outPos);
static XPathExprOperand readExpressionOperand(XPathExpression exprTop,
	 XPathParserState state, char term, char *output, unsigned short *outPos,
					  XPath * paths, unsigned short *pathCnt, bool mainExpr);
static void nextOperandChar(char *value, XPathParserState state, unsigned short *ind,
				unsigned short indMax, bool endAllowed);
static void checkExprOperand(XPathExpression exprTop, XPathExprOperand operand, bool mainExpr);
static bool canBeNumber(char *str, double *numValue, char **end);
static XPathExprOperatorIdStore *readExpressionOperator(XPathParserState state, char *output,
					   unsigned short *outPos);
static void parseFunctionArgList(XPathParserState state, unsigned short nargs, char *output, unsigned short *outPos,
					 XPath * paths, unsigned short *pathCnt, bool mainExpr);
static void checkFunctionArgTypes(XPathExpression argList, XPathFunction function);
static void nextChar(XPathParserState state, bool endAllowed);
static void skipWhiteSpace(XPathParserState state, bool endAllowed);
static char *ensureSpace(unsigned int sizeNeeded, XPathParserState state);
static void checkExpressionBuffer(unsigned short maxPos);
static void utilizeSpaceForVars(char *output, unsigned short *outPos);

static void dumpXPathExpressionInternal(char **input, XPathHeader xpathHdr, StringInfo output, unsigned short level,
							bool main, bool debug);
static void dumpXPathExprOperand(char **input, XPathHeader xpathHdr, StringInfo output, unsigned short level,
					 bool debug);
static void dumpXPathExprOperator(char **input, StringInfo output, unsigned short level,
					  bool debug);

/*
 * If multiple operators start with the same char/substring, the longer
 * one(s) must precede the shorter one(s).
 */
XPathExprOperatorTextData xpathOperators[XPATH_EXPR_OPERATOR_KINDS] = {
	{{XPATH_EXPR_OPERATOR_LTE, 0, XPATH_VAL_BOOLEAN}, "<="},
	{{XPATH_EXPR_OPERATOR_LT, 0, XPATH_VAL_BOOLEAN}, "<"},
	{{XPATH_EXPR_OPERATOR_GTE, 0, XPATH_VAL_BOOLEAN}, ">="},
	{{XPATH_EXPR_OPERATOR_GT, 0, XPATH_VAL_BOOLEAN}, ">"},
	{{XPATH_EXPR_OPERATOR_EQ, 1, XPATH_VAL_BOOLEAN}, "="},
	{{XPATH_EXPR_OPERATOR_NEQ, 1, XPATH_VAL_BOOLEAN}, "!="},
	{{XPATH_EXPR_OPERATOR_AND, 2, XPATH_VAL_BOOLEAN}, "and"},
	{{XPATH_EXPR_OPERATOR_OR, 3, XPATH_VAL_BOOLEAN}, "or"}
};

/* Order of values must follow that of XPathNodeType enumeration */
static char nodeTypes[][XPATH_NODE_TYPE_MAX_LEN + 1] = {
	"comment()",
	"text()",
	"node()",
	"processing-instruction"
};

/*
 * Function to parse xpath expression.
 *
 * 'exprCurrent' - expression or subexpression that the function will process
 *
 * 'state' - state of the XPath parsing (i.e. not only that of the
 * expression).
 *
 * 'term' - terminating character. ']' for predicate, ')' for
 * (explicit) subexpression, '\0' for main expression.
 *
 * 'firstOpPtr' - first operator (or rather pointer to its id) of the subexpression
 * that the function will process. Sometimes we first read the operator and then realize
 * that it belongs to a subexpression (because it has higher precedence).
 * That's why we pass it to the function when calling it for the subexpression.
 *
 * 'output' - output byte array for the expression and subexpressions
 * (if xpath is contained as operand, only its id is stored there and the path itself
 * uses a separate buffer). XPATH_EXPR_BUFFER_SIZE is minimum size of this array.
 *
 * 'outPos' - position in the output array where the next data will be written. This is
 * both input and output variable.
 *
 * 'variables' - array indicating where variables are located in the expression
 * and its subexpressions.
 * Variable is an operand that needs to be substituted before the expression is
 * evaluated for particular node.
 *
 * 'isSubExpr' - 'true' if the function is to process a subexpression (explicit or implicit)
 *
 * 'paths' - if it appears that an expression operand is a location path,
 * it's processed separate and the result pointer is stored to 'paths' array.
 *
 * 'pathCnt' - the current count of processed paths contained in the 'top
 * expression' and all its subexpressions (and their subexpressions, etc.)
 *
 * 'mainExpr' - functions like xpath() receive 'main expression' as argument. The other
 * type is 'predicate expression', i.e. the one enclosed in [].
 *
 * Returns pointer to the first operator that doesn't pertain to the
 * expression - this typically happens when implicit sub-expression was
 * created to handle higher operator precedence and the next (returned)
 * operator has lower precedence than the first one in the sub-expression.
 *
 * NULL is returned if the whole expression or an explicit sub-expression has
 * ended.
 */

XPathExprOperatorIdStore *
parseXPathExpression(XPathExpression exprCurrent, XPathParserState state, char term,
XPathExprOperatorIdStore * firstOpIdPtr, char *output, unsigned short *outPos,
					 bool isSubExpr, bool argList, XPath * paths, unsigned short *pathCnt, bool mainExpr)
{

	XPathExpression exprTop = (XPathExpression) output;
	XPathExprOperand operand = NULL;
	XPathExprOperatorIdStore *opIdPtr = NULL;
	XPathExprOperator operator = NULL;
	unsigned char precedence = 0;
	bool		readOperator;
	bool		end = false;
	bool		firstIter = true;
	unsigned short firstMembOff = (char *) exprCurrent - (char *) output + sizeof(XPathExpressionData);

	/*
	 * If 'firstOperator is not NULL, then '*state->c' points to the 2nd
	 * operand and not to the 1st one.
	 */
	bool		subExprExpl = (*state->c == XNODE_CHAR_LBRKT_RND && firstOpIdPtr == NULL);
	XPathExprOperator firstOperator = XPATH_EXPR_OPERATOR(firstOpIdPtr);

	checkExpressionBuffer(*outPos);

	if (!isSubExpr)
	{
		exprCurrent->type = XPATH_OPERAND_EXPR_TOP;
		exprCurrent->variables = 0;

		exprCurrent->nlits = 0;
		exprCurrent->npaths = 0;
		exprCurrent->nfuncs = 0;
		firstMembOff += XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);
	}

	if (!isSubExpr || subExprExpl)
	{
		nextChar(state, false);
		skipWhiteSpace(state, false);
		operand = readExpressionOperand(exprTop, state, term, output, outPos, paths, pathCnt, mainExpr);

		/*
		 * In case the expression only has 1 member, its value type will be
		 * equal to that of the member.
		 */

		if (operand->type == XPATH_OPERAND_FUNC || operand->type == XPATH_OPERAND_FUNC_NOARG)
		{
			XPathFunctionId fid;
			XPathFunction func;

			if (operand->type == XPATH_OPERAND_FUNC_NOARG)
			{
				fid = operand->value.v.funcId;
			}
			else
			{
				XPathExpression argList = (XPathExpression) operand;

				fid = argList->funcId;
			}
			func = &xpathFunctions[fid];
			exprCurrent->valType = func->resType;
		}
		else
		{
			exprCurrent->valType = operand->value.type;
		}

		checkExprOperand(exprTop, operand, mainExpr);
		skipWhiteSpace(state, true);
		readOperator = true;
	}
	else
	{
		operator = firstOperator;
		opIdPtr = firstOpIdPtr;

		/*
		 * All operators of the same priority have the same result type.
		 * Therefore, in order to determine type of the whole expression, we
		 * need to check any operator at the current level. So we check the
		 * first one.
		 */
		exprCurrent->valType = firstOperator->resType;

		readOperator = false;
	}
	exprCurrent->members = 1;

	if (*state->c == term)
	{
		if (readOperator)
		{
			end = true;
		}
		else
		{
			elog(ERROR, "unexpected end of xpath expression");
		}
	}
	else
	{
		end = false;
	}

	while (!end)
	{
		if (readOperator)
		{
			opIdPtr = readExpressionOperator(state, output, outPos);
			operator = XPATH_EXPR_OPERATOR(opIdPtr);
			skipWhiteSpace(state, false);
		}
		if (firstIter)
		{
			if (!isSubExpr || subExprExpl)
			{
				precedence = operator->precedence;

				/*
				 * Now that we know the first operand is not alone, we need to
				 * adjust the value type according to the first operator.
				 */
				exprCurrent->valType = operator->resType;
			}
			else
			{
				precedence = firstOperator->precedence;
			}
			firstIter = false;
		}
		if (operator->precedence < precedence)
		{
			XPathExpression subExpr = (XPathExpression) operand;
			XPathExprOperatorIdStore *nextOpIdPtr;
			XPathExprOperator nextOperator;

			insertSubexpression(operand, &opIdPtr, exprTop, output + *outPos - (char *) operand, false,
								output, outPos);
			subExpr->members = 1;
			nextOpIdPtr = parseXPathExpression(subExpr, state, term, opIdPtr, output, outPos, true, false, paths,
											   pathCnt, mainExpr);
			operator = XPATH_EXPR_OPERATOR(opIdPtr);
			nextOperator = XPATH_EXPR_OPERATOR(nextOpIdPtr);

			readOperator = (nextOpIdPtr == NULL);

			if (nextOperator)
			{
				operator = nextOperator;
				opIdPtr = nextOpIdPtr;

				while (*state->c != term && operator->precedence < precedence)
				{
					XPathExprOperator nextOperator;
					XPathExprOperatorIdStore *nextOpIdPtr;

					subExpr = (XPathExpression) operand;
					insertSubexpression(operand, &opIdPtr, exprTop, output + *outPos - (char *) operand,
										false, output, outPos);
					subExpr->members = 1;
					nextOpIdPtr = parseXPathExpression(subExpr, state, term, opIdPtr, output, outPos, true, false,
												   paths, pathCnt, mainExpr);
					operator = XPATH_EXPR_OPERATOR(opIdPtr);
					nextOperator = XPATH_EXPR_OPERATOR(nextOpIdPtr);

					if (nextOperator)
					{
						operator = nextOperator;
						opIdPtr = nextOpIdPtr;
					}
					readOperator = (nextOpIdPtr == NULL);
				}
			}
		}
		else if (operator->precedence > precedence)
		{
			unsigned short shift = sizeof(XPathExpressionData);

			if (isSubExpr && !subExprExpl && !argList)
			{
				/*
				 * The last operator we've read doesn't belong to the
				 * (implicit) subexpression
				 */
				exprCurrent->size = *outPos - firstMembOff + shift - sizeof(XPathExprOperatorIdStore);
				return opIdPtr;
			}
			else
			{
				char	   *firstMember = (char *) exprTop + firstMembOff;
				XPathExpression subExpr = (XPathExpression) firstMember;
				unsigned short subExprSzNew = (char *) opIdPtr - (char *) subExpr + shift;

				precedence = operator->precedence;
				insertSubexpression((XPathExprOperand) subExpr, &opIdPtr, exprTop,
							  *outPos - firstMembOff, false, output, outPos);
				operator = XPATH_EXPR_OPERATOR(opIdPtr);
				subExpr->members = exprCurrent->members;
				subExpr->size = subExprSzNew;
				subExpr->valType = operator->resType;
				operand = readExpressionOperand(exprTop, state, term, output, outPos, paths, pathCnt, mainExpr);
				checkExprOperand(exprTop, operand, mainExpr);
				skipWhiteSpace(state, false);
				readOperator = true;
				exprCurrent->members = 2;
			}
		}
		else
		{
			operand = readExpressionOperand(exprTop, state, term, output, outPos, paths, pathCnt, mainExpr);
			checkExprOperand(exprTop, operand, mainExpr);
			skipWhiteSpace(state, true);
			readOperator = true;
			exprCurrent->members++;
		}

		if (*state->c == term)
		{
			if (readOperator)
			{
				end = true;
			}
			else
			{
				elog(ERROR, "unexpected end of xpath expression");
			}
		}
	}

	exprCurrent->size = *outPos - firstMembOff + sizeof(XPathExpressionData);
	if (!isSubExpr)
	{
		exprCurrent->size += XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);
	}

	/*
	 * This covers the main expression only. For predicate expressions the
	 * same is done by parseLocationPath() function.
	 */
	if (mainExpr && !isSubExpr && exprTop->variables < XPATH_EXPR_VAR_MAX)
	{
		utilizeSpaceForVars((char *) exprTop, outPos);
	}
	return NULL;
}

/*
 * Parse location path
 *
 * paths - if 'isSubPath' is true, then store pointer to the (binary) XPath
 * to this array, at position 'pathCount'
 *
 * 'isSubPath' - if true, a sub-path is to be parsed. Otherwise a top-level XPath.
 *
 * 'pathCount' - number of sub-paths of the top-level XPath that have been processed so far.
 *
 * 'xpathSrc' - pointer to the source text where parsing should continue when path has been
 * processe.
 *
 * 'pos' - position in the source text where the parsing should continue.
 * Unlike 'xpathSrc', this is increased by one for each character, whether it's single-byte or MB.
 *
 * Returns a pointer to the top-level (binary) XPath or NULL for a sub-path
 */
XPath
parseLocationPath(XPath * paths, bool isSubPath, unsigned short *pathCount, char **xpathSrc,
				  unsigned short *pos)
{
	XPath		xpath;
	XPathParserStateData state;
	char	   *xpathStr = *xpathSrc;
	bool		slashAllowed = true;
	bool		nonEmpty;

	if (*pathCount == XPATH_SET_MAX_PATHS)
	{
		elog(ERROR, "too many paths in an XPath expression");
	}
	state.outSize = XPATH_PARSER_OUTPUT_CHUNK;
	state.outChunks = 1;
	state.output = state.result = (char *) palloc(state.outSize);
	xpath = (XPath) ensureSpace(sizeof(XPathData), &state);
	xpath->depth = 0;
	xpath->descendants = 0;
	xpath->relative = (*xpathStr != XNODE_CHAR_SLASH);

	if (isSubPath)
	{
		state.pos = xpath->relative ? *pos : *pos + 1;
	}
	else
	{
		state.pos = xpath->relative ? 1 : 2;
	}
	state.c = xpath->relative ? xpathStr : xpathStr + 1;

	if (isSubPath)
	{
		nonEmpty = (*state.c != '\0' && (*state.c == XNODE_CHAR_AT || *state.c == XNODE_CHAR_SLASH ||
										 XNODE_VALID_NAME_START(state.c)));
	}
	else
	{
		nonEmpty = (!xpath->relative && strlen(xpathStr) > 1) || (xpath->relative && strlen(xpathStr) > 0);
	}

	if (nonEmpty)
	{
		bool		finished = false;

		/*
		 * Ensure space for 'XPathData.elements' array. The first element is
		 * inside the XPathData structure, that's why (XPATH_MAX_DEPTH - 1).
		 */
		ensureSpace((XPATH_MAX_DEPTH - 1) * sizeof(XPathOffset), &state);

		state.cWidth = pg_utf_mblen((unsigned char *) state.c);

		/*
		 * Each iteration processes a single path element (i.e. the part
		 * between 2 slashes).
		 */
		while (!finished)
		{
			unsigned short nameSrcPos = state.c - xpathStr;
			unsigned short nameLen = 0;
			char	   *exprOutput = NULL;
			XPathExpression expr = NULL;
			XPathElement xpel = (XPathElement) ensureSpace(sizeof(XPathElementData), &state);
			XPathOffset xpelOff = (char *) xpel - state.result;

			xpel->descendant = false;

			/*
			 * The 'xpath' pointer should be re-initialized each time, unless
			 * it's clear that no reallocation of the output array happened
			 * since the last use of the 'hdr' pointer.
			 */
			xpath = (XPath) state.result;
			if (xpath->depth >= XPATH_MAX_DEPTH)
			{
				elog(ERROR, "maximum xpath depth (%u elements) exceeded.", XPATH_MAX_DEPTH);
			}
			xpel->hasPredicate = false;
			state.elementPos = 0;

			while (*state.c != XNODE_CHAR_SLASH && *state.c != '\0')
			{
				if (state.elementPos == 0)
				{
					if (*state.c == XNODE_CHAR_AT)
					{
						xpath = (XPath) state.result;
						xpath->targNdKind = XMLNODE_ATTRIBUTE;
						xpath->allAttributes = false;
						nextChar(&state, false);
						nameSrcPos++;
					}
					else
					{
						unsigned char nr;
						unsigned char nodeType = 0;
						bool		ndTypeFound = false;

						for (nr = 0; nr < XPATH_NODE_TYPES_COUNT; nr++)
						{
							char	   *nodeTypeStr = nodeTypes[nr];
							unsigned short ntStrLen = strlen(nodeTypeStr);

							if (strncmp(state.c, nodeTypeStr, ntStrLen) == 0)
							{
								if (nr == XPATH_NODE_TYPE_PI && *(state.c + ntStrLen) != XNODE_CHAR_LBRKT_RND)
								{
									continue;
								}
								nodeType = nr;
								ndTypeFound = true;
								break;
							}
						}

						if (ndTypeFound)
						{
							unsigned short i;
							unsigned short ndTestLen = strlen(nodeTypes[nodeType]);

							if (nr == XPATH_NODE_TYPE_PI)
							{
								/*
								 * Skip the left bracket.
								 */
								ndTestLen++;
							}
							xpath = (XPath) state.result;
							switch (nodeType)
							{
								case XPATH_NODE_TYPE_COMMENT:
									xpath->targNdKind = XMLNODE_COMMENT;
									break;

								case XPATH_NODE_TYPE_TEXT:
									xpath->targNdKind = XMLNODE_TEXT;
									break;

								case XPATH_NODE_TYPE_NODE:
									xpath->targNdKind = XMLNODE_NODE;
									break;

								case XPATH_NODE_TYPE_PI:
									xpath->targNdKind = XMLNODE_PI;
									break;

								default:
									elog(ERROR, "unknown node type %u", nodeType);
									break;
							}

							for (i = 0; i < ndTestLen; i++)
							{
								nextChar(&state, true);
							}
							if (nodeType == XPATH_NODE_TYPE_PI)
							{
								char		qmark;
								char	   *piTarget = NULL;

								skipWhiteSpace(&state, false);
								if (*state.c == XNODE_CHAR_APOSTR || *state.c == XNODE_CHAR_QUOTMARK)
								{
									qmark = *state.c;
									nextChar(&state, false);
									nameSrcPos = state.c - xpathStr;
									piTarget = state.c;
									while (*state.c != qmark)
									{
										nextChar(&state, false);
									}
									nameLen = state.c - piTarget;
									nextChar(&state, false);
									skipWhiteSpace(&state, false);
									xpath->piTestValue = true;
								}
								else
								{
									xpath->piTestValue = false;
								}
								if (*state.c != XNODE_CHAR_RBRKT_RND)
								{
									elog(ERROR, "')' or string literal expected at position %u of xpath expression.", state.pos);
								}
								else
								{
									nextChar(&state, true);
								}
							}
							break;
						}
						else
						{
							if (XNODE_VALID_NAME_START(state.c))
							{
								xpath = (XPath) state.result;
								xpath->targNdKind = XMLNODE_ELEMENT;
								nameLen += state.cWidth;
								nextChar(&state, true);
							}
							else
							{
								if (XNODE_WHITESPACE(state.c))
								{
									elog(ERROR, "xpath must not end with '/'");
								}
								else
								{
									elog(ERROR, "unexpected character '%c' in element name, see xpath position %u",
										 *state.c, state.pos);
								}
							}
						}
					}
				}
				else
				{
					/* state.elementPos > 0 */
					xpath = (XPath) state.result;
					if (xpath->targNdKind == XMLNODE_ELEMENT)
					{
						if (*state.c == XNODE_CHAR_LBRACKET)
						{
							unsigned short outPos;

							xpel = (XPathElement) (state.result + xpelOff);
							xpel->hasPredicate = true;
							exprOutput = (char *) palloc(XPATH_EXPR_BUFFER_SIZE);
							expr = (XPathExpression) exprOutput;
							outPos = sizeof(XPathExpressionData) +
								XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);

							checkExpressionBuffer(outPos);
							parseXPathExpression(expr, &state, XNODE_CHAR_RBRACKET, NULL, exprOutput, &outPos,
									  false, false, paths, pathCount, false);

							/*
							 * Shift the operands/operators left if some
							 * variable offsets are unused
							 */
							if (expr->variables < XPATH_EXPR_VAR_MAX)
							{
								utilizeSpaceForVars(exprOutput, &outPos);
							}
							nextChar(&state, true);

							/*
							 * Now we're right after ']'
							 */
							break;
						}
						else if (XNODE_VALID_NAME_CHAR(state.c))
						{
							nameLen += state.cWidth;
							nextChar(&state, true);
						}
						else
						{
							break;
						}
					}
					else if (xpath->targNdKind == XMLNODE_ATTRIBUTE)
					{
						if (*state.c == XNODE_CHAR_ASTERISK)
						{
							xpath->allAttributes = true;
							nextChar(&state, true);
							break;
						}
						if (nameLen == 0)
						{
							/*
							 * Starting to read attribute name after '@'
							 */
							if (!XNODE_VALID_NAME_START(state.c))
							{
								if (XNODE_WHITESPACE(state.c))
								{
									elog(ERROR, "xpath must not end with '@'");
								}
								else
								{
									elog(ERROR, "unexpected character '%c' in attribute name, see position %u of xpath expression.",
										 *state.c, state.pos);
								}
							}
						}
						else
						{
							if (!XNODE_VALID_NAME_CHAR(state.c))
							{
								break;
							}
						}
						nameLen += state.cWidth;
						if (nameLen >= XPATH_ELEMENT_MAX_LEN)
						{
							elog(ERROR, "XPath element too long");
						}
						nextChar(&state, true);
					}
				}
			}

			/*
			 * We're at the first character after the xpath element. That can
			 * be '/', white space, operator, right bracket...
			 */
			if (state.elementPos == 0)
			{
				if (!slashAllowed)
				{
					elog(ERROR, "invalid xpath expression");
				}
				slashAllowed = false;
				state.output -= sizeof(XPathElementData);
				nextChar(&state, false);
				continue;
			}
			else
			{
				/* xpath element after '//' ? */
				if (!slashAllowed)
				{
					xpel->descendant = true;
				}
			}

			if (!isSubPath)
			{
				if (*state.c == XNODE_CHAR_SLASH)
				{
					if (*(state.c + 1) == '\0')
					{
						elog(ERROR, "xpath must not end with '/'");
					}
					else
					{
						nextChar(&state, true);
					}
				}
				else if (*state.c == '\0')
				{
					finished = true;
				}
				else
				{
					elog(ERROR, "unexpected character '%c' at position %u of xpath expression",
						 *state.c, state.pos);
				}
			}
			else
			{
				if (*state.c != XNODE_CHAR_SLASH)
				{
					finished = true;
				}
				else
				{
					/*
					 * At '/' now, so move to the next element.
					 */
					nextChar(&state, false);
				}
			}

			xpath = (XPath) state.result;

			/*
			 * Save the xpath element
			 */
			xpath->elements[xpath->depth] = xpelOff;
			if (xpath->targNdKind == XMLNODE_ELEMENT || xpath->targNdKind == XMLNODE_PI ||
			(xpath->targNdKind == XMLNODE_ATTRIBUTE && !xpath->allAttributes))
			{
				ensureSpace(nameLen, &state);

				/*
				 * Potential reallocation of the output array has to be taken
				 * into account:
				 */
				xpel = (XPathElement) (state.result + xpelOff);
				memcpy(xpel->name, xpathStr + nameSrcPos, nameLen);
				*(state.output - 1) = '\0';
			}
			else
			{
				xpel = (XPathElement) (state.result + xpelOff);
				xpel->name[0] = '\0';
			}

			if (xpel->hasPredicate)
			{
				char	   *target = ensureSpace(expr->size, &state);

				memcpy(target, exprOutput, expr->size);
				pfree(exprOutput);
			}
			if (xpel->descendant)
			{
				xpath->descendants++;
			}
			slashAllowed = true;
			xpath = (XPath) state.result;
			xpath->depth++;
		}

		/*
		 * If there's unused space in the array of element offsets, shift all
		 * elements left
		 */
		xpath = (XPath) state.result;
		if (xpath->depth < XPATH_MAX_DEPTH)
		{
			unsigned short shift = (XPATH_MAX_DEPTH - xpath->depth) * sizeof(XPathOffset);
			char	   *firstEl = state.result + xpath->elements[0];
			unsigned short len = state.output - firstEl;
			unsigned short i;

			memmove(firstEl - shift, firstEl, len);
			state.output -= shift;

			for (i = 0; i < xpath->depth; i++)
			{
				xpath->elements[i] -= shift;
			}
		}
	}
	else
	{
		if (xpath->relative)
		{
			elog(ERROR, "empty xpath expression");
		}
		xpath->depth = 0;
		xpath->targNdKind = XMLNODE_DOC;
	}
	xpath = (XPath) state.result;
	xpath->size = state.output - state.result;
	if (isSubPath)
	{
		*xpathSrc = state.c;
		*pos = state.pos;
		paths[*pathCount] = xpath;
		return NULL;
	}
	else
	{
		return xpath;
	}
}

/*
 * Make 'operand' the first member of a subexpression.
 *
 * Operator following this operand is also shifted (this operator's precedence is typically the reason
 * for the new subexpression). Therefore 'opIdPtr' (pointer to the operator's id in the storage) is passed.
 *
 * It's also necessary to adjust variables already pointing to the operands that we shift.
 */
static void
insertSubexpression(XPathExprOperand operand, XPathExprOperatorIdStore ** opIdPtr,
		XPathExpression exprTop, unsigned short blockSize, bool varsShiftAll,
					char *output, unsigned short *outPos)
{
	unsigned short i;
	unsigned short subExprSz = sizeof(XPathExpressionData);
	XPathOffset *varOffPtr;
	XPathExpression subExpr = (XPathExpression) operand;

	checkExpressionBuffer(*outPos + subExprSz);
	memmove((char *) operand + subExprSz, operand, blockSize);
	if (opIdPtr != NULL)
	{
		*opIdPtr = (XPathExprOperatorIdStore *) ((char *) *opIdPtr + subExprSz);
	}
	if (exprTop != NULL)
	{
		XPathOffset opndOff = (char *) operand - (char *) exprTop;

		/*
		 * Adjust the variable offsets affected
		 */
		varOffPtr = (XPathOffset *) ((char *) exprTop + sizeof(XPathExpressionData));
		for (i = 0; i < exprTop->variables; i++)
		{
			if (varsShiftAll || *varOffPtr >= opndOff)
			{
				*varOffPtr += subExprSz;
			}
			varOffPtr++;
		}
	}
	subExpr->type = XPATH_OPERAND_EXPR_SUB;
	subExpr->flags = 0;
	*outPos += subExprSz;
}

static XPathExprOperand
readExpressionOperand(XPathExpression exprTop,
	 XPathParserState state, char term, char *output, unsigned short *outPos,
					  XPath * paths, unsigned short *pathCnt, bool mainExpr)
{

	XPathExprOperand op = (XPathExprOperand) ((char *) output + *outPos);
	unsigned short ind = 0;
	unsigned short indMax;
	unsigned short outPosInit = *outPos;
	bool		setSize = true;
	XPathOffset *variables = (XPathOffset *) (output + sizeof(XPathExpressionData));

	checkExpressionBuffer(*outPos + sizeof(XPathExprOperandData));
	if (*state->c != XNODE_CHAR_LBRKT_RND)
	{
		*outPos += sizeof(XPathExprOperandData);
	}
	/* '-1' stands for terminating '\0' */
	indMax = XPATH_EXPR_BUFFER_SIZE - *outPos - 1;

	op->value.castToNumber = false;
	if (*state->c == term)
	{
		elog(ERROR, "unexpected end of xpath expression or subexpression at position %u", state->pos);
	}
	if (*state->c == XNODE_CHAR_LBRKT_RND)
	{
		XPathExpression subExpr = (XPathExpression) op;

		insertSubexpression(op, NULL, NULL, output + *outPos - (char *) op, false,
							output, outPos);
		checkExpressionBuffer(*outPos);
		parseXPathExpression(subExpr, state, XNODE_CHAR_RBRKT_RND, NULL, output, outPos, true, false, paths,
							 pathCnt, mainExpr);

		subExpr->type = XPATH_OPERAND_EXPR_SUB;
		subExpr->flags = XPATH_SUBEXPRESSION_EXPLICIT;
		nextChar(state, true);

		/*
		 * Because of the subexpression insertion, the 'op' variable no longer
		 * points to the operand. Moreover, subexpression record has fixed
		 * size.
		 */
		setSize = false;
	}
	else if (*state->c == XNODE_CHAR_AT || XNODE_VALID_NAME_START(state->c) || *state->c == XNODE_CHAR_SLASH)
	{
		XPathFunction func = NULL;

		if (exprTop->variables == XPATH_EXPR_VAR_MAX)
		{
			elog(ERROR, "xpath expression contains too many variables");
		}
		if (*state->c == XNODE_CHAR_AT)
		{
			op->type = XPATH_OPERAND_ATTRIBUTE;

			/*
			 * Set the type even if 'substituteAttributes()' function does it
			 * too. If there's no appropriate attribute in the current
			 * element, then no substitution takes place.
			 * 'evaluateBinaryOperator()' could recognize 'isNull' attribute
			 * of the operand, but its logic to evaluate operand types and
			 * choose the correct casts is already complex enough.
			 */
			op->value.type = XPATH_VAL_STRING;
			nextChar(state, false);

			if (!XNODE_VALID_NAME_START(state->c))
			{
				if (*state->c == term)
				{
					elog(ERROR, "unexpected end of xpath expression at position %u", state->pos);
				}
				else
				{
					elog(ERROR, "unexpected character at position %u of xpath expression", state->pos);
				}
			}
		}
		else
		{
			unsigned char id;
			bool		found = false;

			/*
			 * Is it a function?
			 */
			for (id = 0; id < XPATH_FUNCTIONS; id++)
			{
				unsigned short len;

				func = &xpathFunctions[id];
				len = strlen(func->name);

				if (strncmp(state->c, func->name, len) == 0)
				{
					char	   *c = state->c + len;

					found = true;
					while (XNODE_WHITESPACE(c))
					{
						c++;
					}
					if (*c == XNODE_CHAR_LBRKT_RND)
					{
						c++;
						/* Stop right after '(' */
						while (state->c < c)
						{
							nextChar(state, false);
						}

						if (func->nargs == 0)
						{
							op->type = XPATH_OPERAND_FUNC_NOARG;
							op->value.v.funcId = func->id;
							op->value.type = func->resType;
							while (XNODE_WHITESPACE(c))
							{
								c++;
							}
							if (*c != XNODE_CHAR_RBRKT_RND)
							{
								elog(ERROR, "no arguments expected for function %s()", func->name);
							}
							else
							{
								/*
								 * Stop right after ')'.
								 */
								while (state->c <= c)
								{
									nextChar(state, true);
								}
							}
						}
						else
						{
							long int	diff = sizeof(XPathExpressionData) - sizeof(XPathExprOperandData);
							XPathExpression argList = (XPathExpression) op;

							if (diff > 0)
							{
								checkExpressionBuffer(*outPos + diff);
							}
							*outPos += diff;

							parseFunctionArgList(state, func->nargs, output, outPos, paths, pathCnt, mainExpr);
							argList->type = XPATH_OPERAND_FUNC;
							argList->funcId = func->id;
							argList->valType = func->resType;
							argList->members = func->nargs;

							checkFunctionArgTypes(argList, func);
						}
					}
					else
					{
						/* Not a function */
						found = false;
						func = NULL;
					}
					break;
				}
			}
			if (!found)
			{
				op->type = XPATH_OPERAND_PATH;
				op->value.type = XPATH_VAL_NODESET;
			}
		}

		if (op->type != XPATH_OPERAND_FUNC)
		{
			op->value.isNull = true;
			op->substituted = false;
		}
		if (op->type == XPATH_OPERAND_ATTRIBUTE || op->type == XPATH_OPERAND_FUNC_NOARG ||
			op->type == XPATH_OPERAND_PATH)
		{
			XPathOffset varOff = (XPathOffset) ((char *) op - (char *) exprTop);
			XPathOffset *varOffPtr = variables + exprTop->variables;

			*varOffPtr = varOff;
			exprTop->variables++;
		}
		if (op->type == XPATH_OPERAND_ATTRIBUTE)
		{
			/* Finish reading of the attribute name. */
			char	   *valueStorage = output + *outPos;

			do
			{
				nextOperandChar(valueStorage, state, &ind, indMax, true);
			} while (XNODE_VALID_NAME_CHAR(state->c));
			valueStorage[ind] = '\0';
			*outPos += ind + 1;
		}
		else if (op->type == XPATH_OPERAND_PATH)
		{
			parseLocationPath(paths, true, pathCnt, &(state->c), &(state->pos));
			op->value.v.path = *pathCnt;
			(*pathCnt)++;
		}
	}
	else if (*state->c == XNODE_CHAR_QUOTMARK || *state->c == XNODE_CHAR_APOSTR)
	{
		char		qMark = *state->c;
		char	   *valueStorage = output + *outPos;

		op->type = XPATH_OPERAND_LITERAL;
		op->value.type = XPATH_VAL_STRING;
		op->value.isNull = false;

		nextChar(state, false);
		while (*state->c != qMark)
		{
			nextOperandChar(valueStorage, state, &ind, indMax, false);
		}
		valueStorage[ind] = '\0';
		*outPos += ind + 1;

		/* skip the quotation mark */
		nextChar(state, true);
	}
	else if (isdigit(*state->c) || *state->c == XNODE_CHAR_DOT)
	{
		char	   *numEnd;

		op->type = XPATH_OPERAND_LITERAL;
		op->value.type = XPATH_VAL_NUMBER;
		op->value.castToNumber = true;
		op->value.isNull = false;

		if (!canBeNumber(state->c, &(op->value.v.num), &numEnd))
		{
			elog(ERROR, "invalid numeric value, see position %u of the xpath expression", state->pos);
		}

		/*
		 * Move to a character immediately following the number.
		 */
		while (state->c < numEnd)
		{
			nextChar(state, true);
		}
	}
	else
	{
		elog(ERROR, "(sub)expression or function operand expected at position %u of xpath expression, found '%c' instead",
			 state->pos, *state->c);
	}
	if (setSize)
	{
		op->size = *outPos - outPosInit;
	}
	return op;
}

static void
nextOperandChar(char *value, XPathParserState state, unsigned short *ind,
				unsigned short indMax, bool endAllowed)
{

	if ((*ind + state->cWidth) > indMax)
	{
		elog(ERROR, "xpath expression operand is too long or too complex");
	}
	if (state->cWidth == 1)
	{
		value[*ind] = *state->c;
	}
	else
	{
		memcpy(value + *ind, state->c, state->cWidth);
	}
	*ind += state->cWidth;
	nextChar(state, endAllowed);
}

static void
checkExprOperand(XPathExpression exprTop, XPathExprOperand operand, bool mainExpr)
{
	if (operand->type == XPATH_OPERAND_PATH)
	{
		exprTop->npaths++;
	}
	else if (operand->type == XPATH_OPERAND_FUNC_NOARG || operand->type == XPATH_OPERAND_FUNC)
	{
		if (mainExpr)
		{
			XPathFunctionId fid;
			XPathFunction func;

			if (operand->type == XPATH_OPERAND_FUNC_NOARG)
			{
				fid = operand->value.v.funcId;
			}
			else
			{
				XPathExpression argList = (XPathExpression) operand;

				fid = argList->funcId;
			}
			func = &xpathFunctions[fid];
			if (func->predicateOnly)
			{
				elog(ERROR, "%s() function can't be used in main expression", func->name);
			}
		}
		exprTop->nfuncs++;
	}
	else if (operand->type == XPATH_OPERAND_LITERAL)
	{
		exprTop->nlits++;
	}
}

/*
 * Test if a valid number starts at 'str'.
 * If it does, then '*end' is set to the first character after the number.
 */
static bool
canBeNumber(char *str, double *numValue, char **end)
{
	bool		result;

	*numValue = strtod(str, end);
	result = (*end != str);
	return result;
}

static XPathExprOperatorIdStore *
readExpressionOperator(XPathParserState state, char *output, unsigned short *outPos)
{

	XPathExprOperatorIdStore *opIdPtr = (XPathExprOperatorIdStore *) ((char *) output + *outPos);
	unsigned char i;

	checkExpressionBuffer(*outPos + sizeof(XPathExprOperatorIdStore));
	*outPos += sizeof(XPathExprOperatorIdStore);
	for (i = 0; i < XPATH_EXPR_OPERATOR_KINDS; i++)
	{
		XPathExprOperatorText ot = xpathOperators + i;
		char	   *opText = ot->text;

		if (strncmp(state->c, opText, strlen(opText)) == 0)
		{
			unsigned int j;

			for (j = 0; j < strlen(opText); j++)
			{
				nextChar(state, false);
			}
			if ((i == XPATH_EXPR_OPERATOR_AND || i == XPATH_EXPR_OPERATOR_OR) &&
				!XNODE_WHITESPACE(state->c))
			{
				elog(ERROR, "white space expected at position %u", state->pos);
			}
			*opIdPtr = ot->op.id;
			return opIdPtr;
		}
	}

	if (*state->c == XNODE_CHAR_RBRKT_RND)
	{
		elog(ERROR, "unexpected ')' at position %u of xpath expression",
			 state->pos);
	}
	else
	{
		elog(ERROR, "xpath expression (binary) operator or end of expression expected at position %u of xpath expression",
			 state->pos);
	}
	return NULL;
}

/*
 * Parse function argument list where 'nargs' is the expected number of arguments.
 * Any XPath expression operand, including location path or a subexpression can be the argument.
 *
 * Reading stops right after ')'.
 */
static void
parseFunctionArgList(XPathParserState state, unsigned short nargs, char *output, unsigned short *outPos,
					 XPath * paths, unsigned short *pathCnt, bool mainExpr)
{

	XPathExpression exprTop = (XPathExpression) output;
	unsigned short i;

	Assert(nargs > 0);

	skipWhiteSpace(state, false);
	for (i = 1; i <= nargs; i++)
	{
		char		sep = (i < nargs) ? XNODE_CHAR_COMMA : XNODE_CHAR_RBRKT_RND;
		XPathExprOperand opnd = readExpressionOperand(exprTop, state, sep, output, outPos,
												   paths, pathCnt, mainExpr);

		checkExprOperand(exprTop, opnd, mainExpr);
		skipWhiteSpace(state, false);
		if (*state->c != sep)
		{
			XPathExprOperatorIdStore *opIdPtr = readExpressionOperator(state, output, outPos);
			XPathExprOperator operator = XPATH_EXPR_OPERATOR(opIdPtr);

			skipWhiteSpace(state, false);
			if (operator != NULL)
			{
				XPathExpression subExpr = (XPathExpression) opnd;

				insertSubexpression(opnd, &opIdPtr, exprTop, output + *outPos - (char *) opnd, false, output, outPos);
				parseXPathExpression(subExpr, state, sep, opIdPtr, output, outPos, true, true, paths,
									 pathCnt, mainExpr);
				operator = XPATH_EXPR_OPERATOR(opIdPtr);
				subExpr->valType = operator->resType;
			}
			else
			{
				*outPos -= sizeof(XPathExprOperatorIdStore);
			}

			if (*state->c != sep)
			{
				elog(ERROR, "'%c' expected at position %u of the xpath expression", sep, state->pos);
			}
		}
		nextChar(state, true);
		skipWhiteSpace(state, true);
	}
}

/*
 * Types of function arguments are checked at parse time.
 * It would make no sense to let the XPath processor perform (repeated) evaluation of invalid function call.
 */
static void
checkFunctionArgTypes(XPathExpression argList, XPathFunction function)
{
	char	   *c = (char *) argList;
	unsigned short i;
	XPathValueType *typesRequired = function->argTypes;

	c += sizeof(XPathExpressionData);
	for (i = 0; i < argList->members; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) c;
		XPathValueType argType;
		XPathValueType typeRequired = typesRequired[i];

		if (opnd->type == XPATH_OPERAND_EXPR_TOP || opnd->type == XPATH_OPERAND_EXPR_SUB)
		{
			XPathExpression subExpr = (XPathExpression) opnd;

			argType = subExpr->valType;
			c += subExpr->size;
		}
		else
		{
			argType = opnd->value.type;
			c += opnd->size;
		}

		if (argType != typeRequired)
		{
			if (!xpathCasts[argType][typeRequired])
			{
				elog(ERROR, "%s cannot be cast to %s. check argument %u of %s() function",
					 xpathValueTypes[argType], xpathValueTypes[typeRequired], i + 1, function->name);
			}
		}
	}
}

static void
nextChar(XPathParserState state, bool endAllowed)
{
	state->pos++;
	state->c += state->cWidth;
	state->elementPos++;
	if (*state->c == '\0' && !endAllowed)
	{
		elog(ERROR, "unexpected end of xpath expression");
	}
	state->cWidth = pg_utf_mblen((unsigned char *) state->c);
}

static void
skipWhiteSpace(XPathParserState state, bool endAllowed)
{
	while (*state->c != '\0' && XNODE_WHITESPACE(state->c))
	{
		nextChar(state, endAllowed);
	}
}

/*
 * Ensures that 'sizeNeeded' bytes can be written to the output, starting at
 * 'state->output'. The function returns correct target pointer for the new
 * byte(s), regardless reallocation took place or not.
 */
static char *
ensureSpace(unsigned int sizeNeeded, XPathParserState state)
{
	unsigned short chunksNew = 0;
	unsigned short currentSize = state->output - state->result;
	char	   *target = state->output;

	while (currentSize + sizeNeeded > state->outSize)
	{
		state->outSize += XPATH_PARSER_OUTPUT_CHUNK;
		chunksNew++;
	}
	if (chunksNew > 0)
	{
		state->outChunks += chunksNew;
		if (state->outChunks > XPATH_PARSER_OUTPUT_CHUNKS)
		{
			elog(ERROR, "XPath parser: maximum size of output exceeded");
		}
		state->result = (char *) repalloc(state->result, state->outSize);
		target = state->output = state->result + currentSize;
		elog(DEBUG1, "XPath output buffer reallocated");
	}
	state->output += sizeNeeded;
	return target;
}

/*
 * It might not be worthwhile to add reallocation ability to this buffer. In
 * the future it'd make more sense to replace XPATH_EXPR_BUFFER_SIZE constant
 * with a configurable parameter.
 */
static void
checkExpressionBuffer(unsigned short maxPos)
{
	if (maxPos >= XPATH_EXPR_BUFFER_SIZE)
	{
		elog(ERROR, "insufficient buffer for XPath expression (expression too long or too complex)");
	}
}

/*
 * If the space for offsets of variables hasn't been used up, the whole expression is shifted
 * so that no space is wasted.
 * The existing 'variable offsets' are decreased in such a case, so they keep pointing to the
 * variables.
 */
static void
utilizeSpaceForVars(char *output, unsigned short *outPos)
{
	XPathExpression expr = (XPathExpression) output;
	XPathOffset firstMembOff = sizeof(XPathExpressionData) + XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);
	char	   *exprStart = output + firstMembOff;
	XPathOffset *varOffPtr;
	unsigned short i;
	unsigned short shift = (XPATH_EXPR_VAR_MAX - expr->variables) * sizeof(XPathOffset);

	memmove(exprStart - shift, exprStart, *outPos - firstMembOff);
	*outPos -= shift;

	varOffPtr = (XPathOffset *) (output + sizeof(XPathExpressionData));
	for (i = 0; i < expr->variables; i++)
	{
		*varOffPtr -= shift;

		varOffPtr++;
	}
	expr->size -= shift;
}



void
dumpXPathExpression(XPathExpression expr, XPathHeader xpathHdr, StringInfo output, bool main,
					bool debug)
{

	char	   *input = (char *) expr;

	if (debug)
	{
		if (main)
		{
			appendStringInfo(output, "main expr.:");
		}
		else
		{
			appendStringInfo(output, "\n  predicate expr.:");
		}
		appendStringInfo(output, " (paths / funcs: %u / %u)", expr->npaths, expr->nfuncs);
	}
	dumpXPathExpressionInternal(&input, xpathHdr, output, 0, main, debug);
	if (expr->variables > 0 && debug)
	{
		XPathOffset *varOffPtr = (XPathOffset *) ((char *) expr + sizeof(XPathExpressionData));
		unsigned short i;

		appendStringInfoString(output, "\n  variables:");

		for (i = 0; i < expr->variables; i++)
		{
			XPathExprOperand opnd = (XPathExprOperand) ((char *) expr + *varOffPtr);

			switch (opnd->type)
			{
				case XPATH_OPERAND_ATTRIBUTE:
					appendStringInfo(output, "\n    attribute: %s", XPATH_STRING_LITERAL(&opnd->value));
					break;

				case XPATH_OPERAND_PATH:
					appendStringInfo(output, "\n    path: %u", opnd->value.v.path);
					break;

				case XPATH_OPERAND_FUNC_NOARG:
					appendStringInfo(output, "\n    function: %s()", xpathFunctions[opnd->value.v.funcId].name);
					break;

				default:
					elog(ERROR, "unknown type of variable: %u", opnd->type);
					break;
			}
			varOffPtr++;
		}
	}
}

/*
 * Functions to dump XPath expressions.
 * If 'debug' is true, then structure of the expression is displayed in detail, showing subexpressions,
 * precedence of operators, etc.
 */

void
dumpLocationPath(XPathHeader xpathHdr, StringInfo output, bool debug, unsigned short pathNr)
{
	unsigned short i,
				last;
	XPathOffset o;
	XPathElement el;
	XPath		xpath = (XPath) ((char *) xpathHdr + xpathHdr->paths[pathNr]);

	if (debug)
	{
		appendStringInfo(output, "<path %u>\n\n", pathNr);
		if (xpath->relative)
		{
			appendStringInfoString(output, "relative ");
		}
		else
		{
			appendStringInfoString(output, "absolute ");
		}
		appendStringInfoString(output, "xpath\n");
	}
	else
	{
		if (!xpath->relative)
		{
			appendStringInfoChar(output, XNODE_CHAR_SLASH);
		}
	}
	if (xpath->depth == 0)
	{
		return;
	}
	last = (xpath->targNdKind == XMLNODE_ELEMENT) ? xpath->depth : xpath->depth - 1;

	for (i = 1; i <= last; i++)
	{
		unsigned short nameLen;

		o = xpath->elements[i - 1];
		el = (XPathElement) ((char *) xpath + o);
		nameLen = strlen(el->name);

		if (debug)
		{
			appendStringInfoString(output, "\nnode test:\t");
		}
		if (!debug && el->descendant)
		{
			appendStringInfoChar(output, XNODE_CHAR_SLASH);
		}
		appendStringInfoString(output, el->name);
		if (debug && el->descendant)
		{
			appendStringInfo(output, " (desc.)");
		}
		if (el->hasPredicate)
		{
			XPathExpression pexpr = (XPathExpression) ((char *) el +
										 sizeof(XPathElementData) + nameLen);

			dumpXPathExpression(pexpr, xpathHdr, output, false, debug);
		}
		if (!debug && ((i < last) || (xpath->targNdKind != XMLNODE_ELEMENT)))
		{
			appendStringInfoChar(output, XNODE_CHAR_SLASH);
		}
	}

	if (xpath->targNdKind != XMLNODE_ELEMENT)
	{
		if (debug)
		{
			if (xpath->targNdKind == XMLNODE_ATTRIBUTE)
			{
				appendStringInfoString(output, "\nattr. test:\t");
			}
			else
			{
				appendStringInfoString(output, "\nnode test:\t");
			}
		}

		o = xpath->elements[i - 1];
		el = (XPathElement) ((char *) xpath + o);

		if (!debug && el->descendant)
		{
			appendStringInfoChar(output, XNODE_CHAR_SLASH);
		}

		switch (xpath->targNdKind)
		{
			case XMLNODE_COMMENT:
				appendStringInfoString(output, nodeTypes[XPATH_NODE_TYPE_COMMENT]);
				break;

			case XMLNODE_TEXT:
				appendStringInfoString(output, nodeTypes[XPATH_NODE_TYPE_TEXT]);
				break;

			case XMLNODE_NODE:
				appendStringInfoString(output, nodeTypes[XPATH_NODE_TYPE_NODE]);
				break;

			case XMLNODE_PI:
				appendStringInfo(output, "%s%c", nodeTypes[XPATH_NODE_TYPE_PI], XNODE_CHAR_LBRKT_RND);
				el = (XPathElement) ((char *) xpath + xpath->elements[xpath->depth - 1]);
				if (strlen(el->name) > 0)
				{
					appendStringInfo(output, "\"%s\"", el->name);
				}
				appendStringInfoChar(output, XNODE_CHAR_RBRKT_RND);
				break;

			case XMLNODE_ATTRIBUTE:
				if (xpath->allAttributes)
				{
					appendStringInfoString(output, "@*");
				}
				else
				{
					appendStringInfo(output, "@%s", el->name);
				}
				break;

			default:
				elog(ERROR, "invalid node kind: %u", xpath->targNdKind);
				break;
		}

		if (debug && el->descendant)
		{
			appendStringInfoString(output, " (desc.)");
		}
	}
}

/*
 * 'level' - explicit or implicit sub-expression has level > 0. 'main' - main
 * expression, i.e. not a node test predicate (e.g. not the expression in '[
 * ]')
 */
static void
dumpXPathExpressionInternal(char **input, XPathHeader xpathHdr, StringInfo output, unsigned short level,
							bool main, bool debug)
{

	unsigned short i;
	XPathExpression expr = (XPathExpression) * input;

	*input += sizeof(XPathExpressionData);
	if (level == 0)
	{
		*input += expr->variables * sizeof(XPathOffset);
	}
	if (!debug && !main && level == 0)
	{
		appendStringInfoChar(output, XNODE_CHAR_LBRACKET);
	}
	dumpXPathExprOperand(input, xpathHdr, output, level, debug);
	for (i = 1; i < expr->members; i++)
	{
		dumpXPathExprOperator(input, output, level, debug);
		dumpXPathExprOperand(input, xpathHdr, output, level, debug);
	}
	if (!debug && !main && level == 0)
	{
		appendStringInfoChar(output, XNODE_CHAR_RBRACKET);
	}
}

static void
dumpXPathExprOperand(char **input, XPathHeader xpathHdr, StringInfo output, unsigned short level,
					 bool debug)
{

	unsigned short shortValue;
	XPathExprOperand operand = (XPathExprOperand) * input;
	XPathExpression subExpr;
	XPathFunction func;
	unsigned short size;

	if (debug)
	{
		appendStringInfoChar(output, '\n');
		appendStringInfoSpaces(output, 2 * (level + 2));
	}
	switch (operand->type)
	{
		case XPATH_OPERAND_LITERAL:
			if (operand->value.type == XPATH_VAL_STRING)
			{
				appendStringInfo(output, "\"%s\"", XPATH_STRING_LITERAL(&operand->value));
			}
			else if (operand->value.type == XPATH_VAL_NUMBER)
			{
				/*
				 * TODO fine tune the formatting
				 */
				appendStringInfo(output, "%.2f", operand->value.v.num);
			}
			else
			{
				elog(ERROR, "invalid literal");
			}
			*input += operand->size;
			break;

		case XPATH_OPERAND_ATTRIBUTE:
			size = operand->size;
			appendStringInfo(output, "%c%s", XNODE_CHAR_AT, XPATH_STRING_LITERAL(&operand->value));
			*input += size;
			break;

		case XPATH_OPERAND_PATH:
			shortValue = operand->value.v.path;
			if (debug)
			{
				appendStringInfo(output, "<path %u>", shortValue);
			}
			else
			{
				dumpLocationPath(xpathHdr, output, debug, shortValue);
			}
			size = operand->size;
			*input += size;
			break;

		case XPATH_OPERAND_EXPR_SUB:
			subExpr = (XPathExpression) * input;
			if (debug)
			{
				appendStringInfo(output, "subexpr. ");
				if ((subExpr->flags & XPATH_SUBEXPRESSION_EXPLICIT) == 1)
				{
					appendStringInfo(output, "explicit: ");
				}
				else
				{
					appendStringInfo(output, "implicit: ");
				}
				appendStringInfo(output, "(val. type: %u)", subExpr->valType);
			}
			if (!debug && (subExpr->flags & XPATH_SUBEXPRESSION_EXPLICIT) == 1)
			{
				appendStringInfoChar(output, XNODE_CHAR_LBRKT_RND);
			}
			dumpXPathExpressionInternal(input, xpathHdr, output, level + 1, false, debug);
			if (!debug && (subExpr->flags & XPATH_SUBEXPRESSION_EXPLICIT) == 1)
			{
				appendStringInfoChar(output, XNODE_CHAR_RBRKT_RND);
			}
			break;

		case XPATH_OPERAND_FUNC:
			subExpr = (XPathExpression) * input;
			func = &xpathFunctions[subExpr->funcId];
			appendStringInfo(output, "%s(", func->name);
			{
				unsigned short i;

				*input += sizeof(XPathExpressionData);
				for (i = 1; i <= subExpr->members; i++)
				{
					dumpXPathExprOperand(input, xpathHdr, output, level + 1, debug);
					if (i < subExpr->members)
					{
						if (debug)
						{
							appendStringInfoChar(output, '\n');
							appendStringInfoSpaces(output, 2 * (level + 3));
						}
						appendStringInfoChar(output, ',');
					}
				}
			}
			if (debug)
			{
				appendStringInfoChar(output, '\n');
				appendStringInfoSpaces(output, 2 * (level + 2));
			}
			appendStringInfoChar(output, ')');
			break;

		case XPATH_OPERAND_FUNC_NOARG:
			func = &xpathFunctions[operand->value.v.funcId];
			appendStringInfo(output, "%s()", func->name);
			size = operand->size;
			*input += size;
			break;

		default:
			elog(ERROR, "unknown xpath expression operand type: %u", operand->type);
			break;
	}
}

static void
dumpXPathExprOperator(char **input, StringInfo output, unsigned short level,
					  bool debug)
{
	XPathExprOperator operator = XPATH_EXPR_OPERATOR(*input);

	if (debug)
	{
		appendStringInfoChar(output, '\n');
		appendStringInfoSpaces(output, 2 * (level + 2));
	}
	if (!debug && (operator->id == XPATH_EXPR_OPERATOR_AND || operator->id == XPATH_EXPR_OPERATOR_OR))
	{
		appendStringInfoSpaces(output, 1);
	}
	appendStringInfoString(output, xpathOperators[operator->id].text);
	if (!debug && (operator->id == XPATH_EXPR_OPERATOR_AND || operator->id == XPATH_EXPR_OPERATOR_OR))
	{
		appendStringInfoSpaces(output, 1);
	}
	*input += sizeof(XPathExprOperatorIdStore);
}
