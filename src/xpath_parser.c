/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"
#include "mb/pg_wchar.h"

#include "xmlnode.h"
#include "xpath.h"
#include "xmlnode_util.h"

static void insertSubexpression(XPathExprOperand operand, XPathExprOperatorStorage * operatorStorage,
		XPathExpression exprTop, unsigned short blockSize, bool varsShiftAll,
					char *output, unsigned short *outPos);
static XPathExprOperand readExpressionOperand(XPathExpression exprTop, XPathParserState state, unsigned char termFlags,
 char *output, unsigned short *outPos, XPath *paths, unsigned short *pathCnt,
					  XMLNodeContainer paramNames);
static unsigned short getParameterId(XMLNodeContainer params, char *parNameNew);
static void nextOperandChar(char *value, XPathParserState state, unsigned short *ind,
				unsigned short indMax, bool endAllowed);
static void reflectOperandType(XPathExpression exprTop, XPathExprOperand operand, XPath *paths);
static void checkOperandValueType(XPathExprOperand operand, XPathValueType valType);
static XPathValueType getFunctionResultType(XPathExprOperand funcOperand);
static XPathExprOperatorStorage readExpressionOperator(XPathParserState state, char *output,
					   unsigned short *outPos);
static int parseFunctionArgList(XPathParserState state, XPathFunction, char *output, unsigned short *outPos,
		 XPath *paths, unsigned short *pathCnt, XMLNodeContainer paramNames);
static void checkFunctionArgTypes(XPathExpression argList, XPathFunction function);
static void nextChar(XPathParserState state, bool endAllowed);
static void skipWhiteSpace(XPathParserState state, bool endAllowed);
static char *ensureSpace(unsigned int sizeNeeded, unsigned char alignment, LocationPathOutput *output);
static void checkExpressionBuffer(unsigned short maxPos);

static void dumpXPathExpressionInternal(char **input, XPathHeader xpathHdr, StringInfo output, unsigned short level,
							bool main, char **paramNames, bool debug);
static void dumpXPathExprOperand(char **input, XPathHeader xpathHdr, StringInfo output, unsigned short level,
					 char **paramNames, bool debug);
static void dumpXPathExprOperator(char **input, StringInfo output, unsigned short level,
					  bool debug);
static void separateOperand(bool debug, StringInfo output, XPathExprOperatorId id);

/*
 * If multiple operators start with the same char/substring, the longer
 * one(s) must precede the shorter one(s).
 */
XPathExprOperatorTextData xpathOperators[XPATH_EXPR_OPERATOR_KINDS] = {
	{{XPATH_EXPR_OPERATOR_UNION, 0, XPATH_VAL_NODESET}, "|"},
	{{XPATH_EXPR_OPERATOR_MULTIPLY, 1, XPATH_VAL_NUMBER}, "*"},
	{{XPATH_EXPR_OPERATOR_DIV, 1, XPATH_VAL_NUMBER}, "div"},
	{{XPATH_EXPR_OPERATOR_MOD, 1, XPATH_VAL_NUMBER}, "mod"},
	{{XPATH_EXPR_OPERATOR_PLUS, 2, XPATH_VAL_NUMBER}, "+"},
	{{XPATH_EXPR_OPERATOR_MINUS, 2, XPATH_VAL_NUMBER}, "-"},
	{{XPATH_EXPR_OPERATOR_LTE, 3, XPATH_VAL_BOOLEAN}, "<="},
	{{XPATH_EXPR_OPERATOR_LT, 3, XPATH_VAL_BOOLEAN}, "<"},
	{{XPATH_EXPR_OPERATOR_GTE, 3, XPATH_VAL_BOOLEAN}, ">="},
	{{XPATH_EXPR_OPERATOR_GT, 3, XPATH_VAL_BOOLEAN}, ">"},
	{{XPATH_EXPR_OPERATOR_EQ, 4, XPATH_VAL_BOOLEAN}, "="},
	{{XPATH_EXPR_OPERATOR_NEQ, 4, XPATH_VAL_BOOLEAN}, "!="},
	{{XPATH_EXPR_OPERATOR_AND, 5, XPATH_VAL_BOOLEAN}, "and"},
	{{XPATH_EXPR_OPERATOR_OR, 6, XPATH_VAL_BOOLEAN}, "or"}
};

/* Order of values must follow that of XPathNodeType enumeration */
static char nodeTypes[][XPATH_NODE_TYPE_MAX_LEN + 1] = {
	"comment()",
	"text()",
	"node()",
	"processing-instruction"
};

bool
validXPathTermChar(char c, unsigned char flags)
{
	unsigned short flag;

	switch (c)
	{
		case '\0':
			flag = XPATH_TERM_NULL;
			break;

		case XNODE_CHAR_RBRACKET:
			flag = XPATH_TERM_RBRKT;
			break;

		case XNODE_CHAR_RBRKT_CUR:
			flag = XPATH_TERM_RBRKT_CRL;
			break;

		case XNODE_CHAR_RBRKT_RND:
			flag = XPATH_TERM_RBRKT_RND;
			break;

		case XNODE_CHAR_COMMA:
			flag = XPATH_TERM_COMMA;
			break;

		default:
			flag = 0;
			break;
	}

	return ((flag & flags) != 0);
}

/*
 * Function to parse xpath expression.
 *
 * 'exprCurrent' - expression or subexpression that the function will process
 *
 * 'state' - state of the XPath parsing (i.e. not only that of the
 * expression).
 *
 * 'term' - terminating character. ']' for predicate, ')' for
 * (explicit) subexpression, '\0' for main expression. The flag is passed in a form
 * of flag(s), e.g. XPATH_TERM_RBRKT
 *
 * 'firstOperatorStorage' - first operator of the subexpression that
 * the function will process. Sometimes we first read the operator and then realize
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
 * 'paramNames' - a container where unique names of parameters are collected.
 *
 * Returns pointer to the first operator that doesn't pertain to the
 * expression - this typically happens when implicit sub-expression was
 * created to handle higher operator precedence and the next (returned)
 * operator has lower precedence than the first one in the sub-expression.
 *
 * NULL is returned if the whole expression or an explicit sub-expression has
 * ended.
 */

XPathExprOperatorStorage
parseXPathExpression(XPathExpression exprCurrent, XPathParserState state, unsigned char termFlags,
					 XPathExprOperatorStorage firstOperatorStorage, char *output, unsigned short *outPos,
		 bool isSubExpr, bool argList, XPath *paths, unsigned short *pathCnt,
					 XMLNodeContainer paramNames)
{

	XPathExpression exprTop = (XPathExpression) output;
	XPathExprOperand operand = NULL;
	XPathExprOperatorStorage operatorStorage = NULL;
	XPathExprOperator operator = NULL;
	unsigned char precedence = 0;
	bool		readOperator;
	bool		end = false;
	bool		firstIter = true;
	char	   *firstOperandPtr;
	unsigned short firstOperandPos;

	/*
	 * Not any operand type can be cast to nodeset. Therefore, if some
	 * operator requires nodesets, additional check has to be applied to
	 * operands.
	 */
	bool		nodeSetsOnly = false;

	/*
	 * If 'firstOperator is not NULL, then '*state->c' points to the 2nd
	 * operand and not to the 1st one.
	 */
	bool		subExprExpl = (*state->c == XNODE_CHAR_LBRKT_RND && firstOperatorStorage == NULL);
	XPathExprOperator firstOperator = XPATH_EXPR_OPERATOR(firstOperatorStorage);

	firstOperandPtr = (char *) exprCurrent + sizeof(XPathExpressionData);

	if (!isSubExpr)
	{
		exprCurrent->common.type = XPATH_OPERAND_EXPR_TOP;
		exprCurrent->variables = 0;
		exprCurrent->negative = false;

		exprCurrent->nlits = 0;
		exprCurrent->npaths = 0;
		exprCurrent->nfuncs = 0;
		firstOperandPtr = (char *) TYPEALIGN(XPATH_ALIGNOF_OFFSET, firstOperandPtr);
		firstOperandPtr += XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);
	}

	firstOperandPtr = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, firstOperandPtr);
	firstOperandPos = firstOperandPtr - output;

	if (!isSubExpr)
	{
		*outPos = firstOperandPos;
	}

	/*
	 * No need to check the output buffer size so far, nothing has been
	 * written yet. The operand / operator functions will do.
	 */

	if (!isSubExpr || subExprExpl)
	{
		nextChar(state, false);
		skipWhiteSpace(state, false);
		operand = readExpressionOperand(exprTop, state, termFlags, output, outPos, paths, pathCnt, paramNames);
		Assert(operand->common.type != XPATH_OPERAND_EXPR_TOP);

		/*
		 * In case the expression only has 1 member, its value type will be
		 * equal to that of the member.
		 */

		if (operand->common.type == XPATH_OPERAND_FUNC || operand->common.type == XPATH_OPERAND_FUNC_NOARG)
		{
			exprCurrent->valType = getFunctionResultType(operand);
		}
		else if (operand->common.type == XPATH_OPERAND_EXPR_SUB)
		{
			XPathExpression operandExpr = (XPathExpression) operand;

			exprCurrent->valType = operandExpr->valType;
		}
		else
		{
			exprCurrent->valType = operand->value.type;
		}

		reflectOperandType(exprTop, operand, paths);
		skipWhiteSpace(state, true);
		readOperator = true;
	}
	else
	{
		operator = firstOperator;
		operatorStorage = firstOperatorStorage;

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

	if (validXPathTermChar(*state->c, termFlags))
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
			operatorStorage = readExpressionOperator(state, output, outPos);
			operator = XPATH_EXPR_OPERATOR(operatorStorage);
			skipWhiteSpace(state, false);
		}

		if (exprCurrent->members == 1)
		{
			if (operator->id == XPATH_EXPR_OPERATOR_UNION)
			{
				XPathExprOperand opndFirst = (XPathExprOperand) ((char *) exprTop + firstOperandPos);

				checkOperandValueType(opndFirst, XPATH_VAL_NODESET);
				nodeSetsOnly = true;
			}
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
			XPathExprOperatorStorage nextOperatorStorage;
			XPathExprOperator nextOperator;

			insertSubexpression(operand, &operatorStorage, exprTop,
				 output + *outPos - (char *) operand, false, output, outPos);
			subExpr->members = 1;
			nextOperatorStorage = parseXPathExpression(subExpr, state,
					 termFlags, operatorStorage, output, outPos, true, false,
												 paths, pathCnt, paramNames);

			operator = XPATH_EXPR_OPERATOR(operatorStorage);
			nextOperator = XPATH_EXPR_OPERATOR(nextOperatorStorage);
			readOperator = (nextOperatorStorage == NULL);

			if (nextOperator)
			{
				operator = nextOperator;
				operatorStorage = nextOperatorStorage;

				while (!validXPathTermChar(*state->c, termFlags) && operator->precedence < precedence)
				{
					XPathExprOperator nextOperator;
					XPathExprOperatorStorage nextOperatorStorage;

					subExpr = (XPathExpression) operand;
					insertSubexpression(operand, &operatorStorage, exprTop, output + *outPos - (char *) operand,
										false, output, outPos);
					subExpr->members = 1;
					nextOperatorStorage = parseXPathExpression(subExpr, state,
							termFlags, operatorStorage, output, outPos, true,
										  false, paths, pathCnt, paramNames);
					operator = XPATH_EXPR_OPERATOR(operatorStorage);
					nextOperator = XPATH_EXPR_OPERATOR(nextOperatorStorage);

					if (nextOperator)
					{
						operator = nextOperator;
						operatorStorage = nextOperatorStorage;
					}
					readOperator = (nextOperatorStorage == NULL);
				}
			}
		}
		else if (operator->precedence > precedence)
		{
			if (isSubExpr && !subExprExpl && !argList)
			{
				unsigned int size;

				size = (char *) operatorStorage - (char *) exprCurrent;
				exprCurrent->common.size = size;
				return operatorStorage;
			}
			else
			{
				/*
				 * If the current operator is at lower position in terms of
				 * precedence, everything we've read so far will be enclosed
				 * in an implicit subexpression. This is not necessary as long
				 * as expression is evaluated from left to right. However it
				 * brings advantage in some cases to have only one precedence
				 * in each (sub)expression.
				 */
				char	   *firstMember = (char *) exprTop + firstOperandPos;
				XPathExpression subExpr = (XPathExpression) firstMember;
				unsigned int subExprSize;

				precedence = operator->precedence;
				insertSubexpression((XPathExprOperand) subExpr, &operatorStorage, exprTop,
						   *outPos - firstOperandPos, false, output, outPos);
				operator = XPATH_EXPR_OPERATOR(operatorStorage);
				subExpr->members = exprCurrent->members;
				subExprSize = (char *) operatorStorage - (char *) subExpr;
				subExpr->common.size = subExprSize;
				subExpr->valType = exprCurrent->valType;

				exprCurrent->valType = operator->resType;
				operand = readExpressionOperand(exprTop, state, termFlags, output, outPos, paths, pathCnt,
												paramNames);
				if (nodeSetsOnly)
				{
					checkOperandValueType(operand, XPATH_VAL_NODESET);
				}
				reflectOperandType(exprTop, operand, paths);
				skipWhiteSpace(state, false);
				readOperator = true;
				exprCurrent->members = 2;
			}
		}
		else
		{
			/* The operator has the same precedence as the previous did. */

			/*
			 * Union has unique precedence, therefore parser should not mix it
			 * with any other operator.
			 */
			Assert((nodeSetsOnly && operator->id == XPATH_EXPR_OPERATOR_UNION) || !nodeSetsOnly);

			operand = readExpressionOperand(exprTop, state, termFlags, output, outPos, paths, pathCnt,
											paramNames);
			if (nodeSetsOnly)
			{
				checkOperandValueType(operand, XPATH_VAL_NODESET);
			}
			reflectOperandType(exprTop, operand, paths);
			skipWhiteSpace(state, true);
			readOperator = true;
			exprCurrent->members++;
		}

		if (validXPathTermChar(*state->c, termFlags))
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

	exprCurrent->common.size = (output + *outPos) - (char *) exprCurrent;
	return NULL;
}

/*
 * Parse location path
 *
 * paths - the parsed path pointer is stored into this array, at position 'pathCount'.
 * If the location path has predicates containing other paths, these are stored too.
 * NULL value can be used to indicate that location path is not expected.
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
 * 'paramNames' - container to collect (unique) parameter names.
 */
void
parseLocationPath(XPath *paths, bool isSubPath, unsigned short *pathCount, char **xpathSrc,
				  unsigned short *pos, XMLNodeContainer paramNames)
{
	XPath		locPath;
	XPathParserStateData state;
	char	   *xpathStr = *xpathSrc;
	bool		slashAllowed = true;
	bool		nonEmpty;
	LocationPathOutput output;

	if (paths == NULL)
	{
		elog(ERROR, "location path not expected here");
	}

	if (*pathCount == XPATH_SET_MAX_PATHS)
	{
		elog(ERROR, "too many paths in an XPath expression");
	}

	/*
	 * Separate instance of the state is used here because we need a separate
	 * block of memory for each location path (pointer to which will be added
	 * to 'paths' output array).
	 */
	output.size = XPATH_PARSER_OUTPUT_CHUNK;
	output.chunks = 1;
	output.cursor = output.data = (char *) palloc(output.size);
	/* No specific alignment required now, the block is MAXALIGNed. */
	locPath = (XPath) ensureSpace(sizeof(XPathData), 0, &output);
	locPath->depth = 0;
	locPath->descendants = 0;
	locPath->relative = (*xpathStr != XNODE_CHAR_SLASH);

	if (isSubPath)
	{
		state.pos = locPath->relative ? *pos : *pos + 1;
	}
	else
	{
		state.pos = locPath->relative ? 1 : 2;
	}
	state.c = locPath->relative ? xpathStr : xpathStr + 1;

	if (isSubPath)
	{
		nonEmpty = (*state.c != '\0' && (*state.c == XNODE_CHAR_AT || *state.c == XNODE_CHAR_SLASH ||
										 XNODE_VALID_NAME_START(state.c)));
	}
	else
	{
		nonEmpty = (!locPath->relative && strlen(xpathStr) > 1) || (locPath->relative && strlen(xpathStr) > 0);
	}

	if (nonEmpty)
	{
		bool		finished = false;

		/*
		 * Ensure space for 'XPathData.elements' array. The first element is
		 * inside the XPathData structure, that's why (XPATH_MAX_DEPTH - 1).
		 * For the same reason we don't require any alignment here: the other
		 * offsets immediately follow the XPathData structure.
		 */
		ensureSpace((XPATH_MAX_DEPTH - 1) * sizeof(XPathOffset), 0, &output);

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
			XPathElement xpel;
			XPathOffset xpelOff;

			xpel = (XPathElement) ensureSpace(sizeof(XPathElementData), XPATH_ALIGNOF_LOC_STEP, &output);
			xpel->descendant = false;

			/*
			 * The 'xpath' pointer should be re-initialized each time, unless
			 * it's clear that no reallocation of the output array happened
			 * since the last use of the output.
			 */
			locPath = (XPath) output.data;

			xpelOff = (char *) xpel - output.data;

			if (locPath->depth >= XPATH_MAX_DEPTH)
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
						locPath = (XPath) output.data;
						locPath->targNdKind = XMLNODE_ATTRIBUTE;
						locPath->allAttributes = false;

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
							locPath = (XPath) output.data;

							switch (nodeType)
							{
								case XPATH_NODE_TYPE_COMMENT:
									locPath->targNdKind = XMLNODE_COMMENT;

									break;

								case XPATH_NODE_TYPE_TEXT:
									locPath->targNdKind = XMLNODE_TEXT;

									break;

								case XPATH_NODE_TYPE_NODE:
									locPath->targNdKind = XMLNODE_NODE;

									break;

								case XPATH_NODE_TYPE_PI:
									locPath->targNdKind = XMLNODE_PI;

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
									locPath->piTestValue = true;
								}
								else
								{
									locPath->piTestValue = false;
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
								locPath = (XPath) output.data;
								locPath->targNdKind = XMLNODE_ELEMENT;

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
					locPath = (XPath) output.data;

					if (locPath->targNdKind == XMLNODE_ELEMENT)
					{
						/* Predicate expression? */
						if (*state.c == XNODE_CHAR_LBRACKET)
						{
							unsigned short outPos;

							xpel = (XPathElement) ((char *) locPath + xpelOff);
							xpel->hasPredicate = true;
							exprOutput = (char *) palloc(XPATH_EXPR_BUFFER_SIZE);
							expr = (XPathExpression) exprOutput;
							expr->needsContext = false;

							/*
							 * 'isSubExpr=false' will be passed in which case
							 * the initial value of 'outPos' will be set by
							 * the parser itself.
							 */
							outPos = 0;
							checkExpressionBuffer(outPos);
							parseXPathExpression(expr, &state, XPATH_TERM_RBRKT, NULL, exprOutput, &outPos,
								 false, false, paths, pathCount, paramNames);

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
					else if (locPath->targNdKind == XMLNODE_ATTRIBUTE)
					{
						if (*state.c == XNODE_CHAR_ASTERISK)
						{
							if (state.elementPos == 1)
							{
								locPath->allAttributes = true;
								nextChar(&state, true);
							}

							/*
							 * If (state.elementPos > 1) then we're reading
							 * multiply operator: don't move on.
							 */
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

			if (*state.c == XNODE_CHAR_SLASH && locPath->targNdKind == XMLNODE_ATTRIBUTE)
			{
				elog(ERROR, "if location path contains attribute test, it must be the last location step");
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
				output.cursor -= sizeof(XPathElementData);
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

			locPath = (XPath) output.data;

			/*
			 * Save the xpath element
			 */
			locPath->elements[locPath->depth] = xpelOff;


			if (locPath->targNdKind == XMLNODE_ELEMENT || locPath->targNdKind == XMLNODE_PI ||
				(locPath->targNdKind == XMLNODE_ATTRIBUTE && !locPath->allAttributes))
			{
				ensureSpace(nameLen, 0, &output);

				/*
				 * Potential reallocation of the output array has to be taken
				 * into account:
				 */
				xpel = (XPathElement) ((char *) locPath + xpelOff);
				memcpy(xpel->name, xpathStr + nameSrcPos, nameLen);
				*(output.cursor - 1) = '\0';
			}
			else
			{
				xpel = (XPathElement) ((char *) locPath + xpelOff);
				xpel->name[0] = '\0';
			}

			if (xpel->hasPredicate)
			{
				char	   *target = ensureSpace(expr->common.size, XPATH_ALIGNOF_EXPR, &output);

				memcpy(target, exprOutput, expr->common.size);
				pfree(exprOutput);
			}
			if (xpel->descendant)
			{
				locPath->descendants++;
			}
			slashAllowed = true;
			locPath = (XPath) output.data;
			locPath->depth++;
		}
	}
	else
	{
		if (locPath->relative)
		{
			elog(ERROR, "empty xpath expression");
		}
		locPath->depth = 0;
		locPath->targNdKind = XMLNODE_DOC;
	}
	locPath = (XPath) output.data;
	locPath->size = output.cursor - output.data;

	*xpathSrc = state.c;
	*pos = state.pos;
	paths[*pathCount] = locPath;
}

/*
 * Make 'operand' the first member of a new subexpression.
 *
 * Operator following this operand is also shifted (this operator's precedence
 * is typically the reason to create the subexpression). That's why
 * '&operatorStorage' is passed too.
 *
 * It's also necessary to adjust variables already pointing to the operands
 * that we shift.
 */
static void
insertSubexpression(XPathExprOperand operand, XPathExprOperatorStorage * operatorStorage,
		XPathExpression exprTop, unsigned short blockSize, bool varsShiftAll,
					char *output, unsigned short *outPos)
{
	unsigned short i;
	unsigned short moveBy;
	XPathOffset *varOffPtr;
	XPathExpression subExpr = (XPathExpression) operand;

	moveBy = sizeof(XPathExpressionData);
	/* No following operad's alignment may get broken. */
	moveBy = TYPEALIGN(XPATH_ALIGNOF_EXPR, moveBy);

	checkExpressionBuffer(*outPos + moveBy + blockSize);
	memmove((char *) operand + moveBy, operand, blockSize);
	if (operatorStorage != NULL)
	{
		*operatorStorage = (XPathExprOperatorStorage) ((char *) *operatorStorage + moveBy);
	}
	if (exprTop != NULL)
	{
		XPathOffset opndOff = (char *) operand - (char *) exprTop;

		/*
		 * Adjust the variable offsets affected
		 */
		varOffPtr = (XPathOffset *) ((char *) exprTop + sizeof(XPathExpressionData));
		varOffPtr = (XPathOffset *) TYPEALIGN(XPATH_ALIGNOF_OFFSET, varOffPtr);
		for (i = 0; i < exprTop->variables; i++)
		{
			if (varsShiftAll || *varOffPtr >= opndOff)
			{
				*varOffPtr += moveBy;
			}
			varOffPtr++;
		}
	}
	subExpr->common.type = XPATH_OPERAND_EXPR_SUB;
	subExpr->flags = 0;
	subExpr->negative = false;
	*outPos += moveBy;
}

/*
 * Read expression operand, starting at 'state->c'.
 *
 * 'termFlags' represents set of possible terminating character of the expression.
 *	The parameter exists just to make error messages more precise.
 *
 * 'checkTerm' should be set to 'false' when the caller can't determine a single terminating character.
 *
 * If a valid operator is found, reading stops right after the operand. (If that happens to be a white space,
 * then caller is responsible for skipping it.)
 */
static XPathExprOperand
readExpressionOperand(XPathExpression exprTop, XPathParserState state, unsigned char termFlags,
 char *output, unsigned short *outPos, XPath *paths, unsigned short *pathCnt,
					  XMLNodeContainer paramNames)
{
	char	   *outUnaligned;
	XPathExprOperand op;
	unsigned short outPosInit;
	bool		setSize = true;
	bool		negative = false;
	XPathOffset *variables;

	outUnaligned = output + *outPos;
	op = (XPathExprOperand) TYPEALIGN(XPATH_ALIGNOF_OPERAND, outUnaligned);
	*outPos += (char *) op - outUnaligned;
	checkExpressionBuffer(*outPos + sizeof(XPathExprOperandData));
	outPosInit = *outPos;

	variables = (XPathOffset *) (output + sizeof(XPathExpressionData));

	/*
	 * 2-aligned size just added to 8-aligned address, so 2 byte alignment
	 * shouldn't be broken.
	 */
	Assert(PointerIsAligned(variables, XPATH_ALIGNOF_OFFSET));

	op->value.castToNumber = false;

	/*
	 * If the terminating character is known, check it now. Otherwise we must
	 * test all operand types and (possibly) fail when no match is found.
	 */
	if (validXPathTermChar(*state->c, termFlags))
	{
		elog(ERROR, "unexpected end of xpath expression or subexpression at position %u", state->pos);
	}

	if (*state->c == XNODE_CHAR_DASH)
	{
		nextChar(state, false);
		skipWhiteSpace(state, false);
		negative = true;
	}

	/*
	 * Ensure that we don't overwrite the operand header. If subexpression
	 * starts here, do nothing. The *outPos will be increased accordingly
	 * during insertion of the subexpression header.
	 */
	if (*state->c != XNODE_CHAR_LBRKT_RND)
	{
		*outPos += sizeof(XPathExprOperandData);
	}

	if (*state->c == XNODE_CHAR_LBRKT_RND)
	{
		XPathExpression subExpr = (XPathExpression) op;

		insertSubexpression(op, NULL, NULL, output + *outPos - (char *) op, false,
							output, outPos);
		checkExpressionBuffer(*outPos);
		parseXPathExpression(subExpr, state, XPATH_TERM_RBRKT_RND, NULL, output, outPos, true, false, paths,
							 pathCnt, paramNames);

		subExpr->common.type = XPATH_OPERAND_EXPR_SUB;
		subExpr->flags = XPATH_SUBEXPRESSION_EXPLICIT;
		subExpr->negative = negative;
		nextChar(state, true);

		/*
		 * Because of the subexpression insertion, the 'op' variable no longer
		 * points to the operand. Moreover, subexpression record has fixed
		 * size.
		 */
		setSize = false;
	}
	else if (*state->c == XNODE_CHAR_AT || XNODE_VALID_NAME_START(state->c) || *state->c == XNODE_CHAR_SLASH ||
			 *state->c == XNODE_CHAR_DOLLAR)
	{
		XPathFunction func = NULL;

		/*
		 * One of the following is expected: attribute (including '@*'), path
		 * (either absolute or relative) or function.
		 */
		if (exprTop->variables == XPATH_EXPR_VAR_MAX)
		{
			elog(ERROR, "xpath expression contains too many variables");
		}

		if (*state->c == XNODE_CHAR_AT || *state->c == XNODE_CHAR_DOLLAR)
		{
			if (*state->c == XNODE_CHAR_AT)
			{
				op->common.type = XPATH_OPERAND_ATTRIBUTE;

				/*
				 * Set the type even if 'substituteAttributes()' function does
				 * it too. If there's no appropriate attribute in the current
				 * element, then no substitution takes place.
				 *
				 * In such a case 'evaluateBinaryOperator()' could recognize
				 * 'isNull' attribute of the operand and consequently ignore
				 * the value type, but the logic to evaluate operand types and
				 * choose the correct casts is already complex enough.
				 */
				op->value.type = XPATH_VAL_NODESET;
				op->value.v.nodeSet.count = 0;
				op->value.v.nodeSet.isDocument = false;
			}
			else
			{
				/* No other information is available at the moment. */
				op->common.type = XPATH_OPERAND_PARAMETER;
			}

			nextChar(state, false);

			if (!(XNODE_VALID_NAME_START(state->c) ||
				  (op->common.type == XPATH_OPERAND_ATTRIBUTE && *state->c == XNODE_CHAR_ASTERISK)))
			{
				if (validXPathTermChar(*state->c, termFlags))
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

						if (func->nargs == 0)
						{
							op->common.type = XPATH_OPERAND_FUNC_NOARG;
							op->value.v.funcId = func->id;
							op->value.type = func->resType;

							while (XNODE_WHITESPACE(c))
							{
								c++;
							}
							if (*c != XNODE_CHAR_RBRKT_RND)
							{
								bool		found = false;
								char	   *funcName = func->name;

								/*
								 * The same function may exist having non-zero
								 * argument count.
								 */
								id++;
								for (; id < XPATH_FUNCTIONS; id++)
								{
									func = &xpathFunctions[id];
									len = strlen(func->name);

									if (strncmp(state->c, func->name, len) == 0)
									{
										found = true;
										break;
									}
								}
								if (!found)
								{
									elog(ERROR, "no arguments expected for function %s()", funcName);
								}
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

						if (func->nargs > 0)
						{
							long int	diff = sizeof(XPathExpressionData) - sizeof(XPathExprOperandData);
							XPathExpression argList = (XPathExpression) op;

							/*
							 * If this is the 2nd attempt to find the function
							 * (i.e. after having failed for 'argumentless'
							 * version), then 'c' is at the first
							 * non-whitespace after '('.
							 *
							 * If there was no 'argumentless' function having
							 * the same name before, we're at the first
							 * character after '(' (whether it's white space
							 * or not).
							 */
							while (state->c < c)
							{
								nextChar(state, false);
							}

							if (diff > 0)
							{
								checkExpressionBuffer(*outPos + diff);
							}
							*outPos += diff;

							argList->members = parseFunctionArgList(state, func, output, outPos, paths, pathCnt,
																 paramNames);
							argList->common.type = XPATH_OPERAND_FUNC;
							argList->negative = negative;
							argList->funcId = func->id;
							argList->valType = func->resType;
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
				op->common.type = XPATH_OPERAND_PATH;
				op->value.type = XPATH_VAL_NODESET;
			}
		}

		if (op->common.type != XPATH_OPERAND_FUNC)
		{
			op->value.isNull = true;
			op->value.negative = negative;
			op->substituted = false;
		}

		if (op->common.type == XPATH_OPERAND_ATTRIBUTE || op->common.type == XPATH_OPERAND_FUNC_NOARG ||
			op->common.type == XPATH_OPERAND_PATH || op->common.type == XPATH_OPERAND_PARAMETER)
		{
			XPathOffset varOff = (XPathOffset) ((char *) op - (char *) exprTop);
			XPathOffset *varOffPtr = variables + exprTop->variables;

			*varOffPtr = varOff;
			exprTop->variables++;
		}
		if (op->common.type == XPATH_OPERAND_ATTRIBUTE || op->common.type == XPATH_OPERAND_PARAMETER)
		{
			/* Finish reading of the name. */
			char	   *valueStorage;
			bool		oneCharOnly = (op->common.type == XPATH_OPERAND_ATTRIBUTE && *state->c == XNODE_CHAR_ASTERISK);
			unsigned short ind = 0;

			/* '-1' stands for terminating '\0' */
			unsigned int indMax;

			if (op->common.type == XPATH_OPERAND_ATTRIBUTE)
			{
				valueStorage = output + *outPos;
				indMax = XPATH_EXPR_BUFFER_SIZE - *outPos - 1;
			}
			else
			{
				valueStorage = (char *) palloc(XPATH_PARAM_NAME_MAX_LEN + 1);
				indMax = XPATH_PARAM_NAME_MAX_LEN - 1;
			}

			do
			{
				nextOperandChar(valueStorage, state, &ind, indMax, true);
			} while (XNODE_VALID_NAME_CHAR(state->c) && !oneCharOnly);
			valueStorage[ind] = '\0';

			if (op->common.type == XPATH_OPERAND_ATTRIBUTE)
			{
				*outPos += ind + 1;
			}

			if (op->common.type == XPATH_OPERAND_PARAMETER)
			{
				op->value.v.paramId = getParameterId(paramNames, valueStorage);
				pfree(valueStorage);
			}
		}
		else if (op->common.type == XPATH_OPERAND_PATH)
		{
			parseLocationPath(paths, true, pathCnt, &(state->c), &(state->pos), paramNames);
			op->value.v.path = *pathCnt;
			(*pathCnt)++;
		}
	}
	else if (*state->c == XNODE_CHAR_QUOTMARK || *state->c == XNODE_CHAR_APOSTR)
	{
		char		qMark = *state->c;
		char	   *valueStorage = output + *outPos;
		unsigned short ind = 0;
		unsigned int indMax = XPATH_EXPR_BUFFER_SIZE - *outPos - 1;

		op->common.type = XPATH_OPERAND_LITERAL;
		op->value.type = XPATH_VAL_STRING;
		op->value.negative = negative;
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

		op->common.type = XPATH_OPERAND_LITERAL;
		op->value.type = XPATH_VAL_NUMBER;
		op->value.negative = negative;
		op->value.castToNumber = true;
		op->value.isNull = false;

		if (!xmlStringIsNumber(state->c, &(op->value.v.num), &numEnd, false))
		{
			elog(ERROR, "invalid numeric value, see position %u of the xpath expression", state->pos);
		}

		/*
		 * Move to a character immediately following the number. 'nextChar()'
		 * must be used so that all positions are adjusted correctly. That's
		 * why 'xmlStringIsNumber()' couldn't receive 'skipWhitespace=true'.
		 */
		while (state->c < numEnd)
		{
			nextChar(state, true);
		}
	}
	else
	{
		elog(ERROR, "expression or function operand expected at position %u of xpath expression.", state->pos);
	}

	if (setSize)
	{
		Assert(op->common.type != XPATH_OPERAND_EXPR_SUB && op->common.type != XPATH_OPERAND_EXPR_TOP);
		op->common.size = *outPos - outPosInit;
	}
	return op;
}

/*
 * Return id of a parameter name, whether existing or just added.
 */
static unsigned short
getParameterId(XMLNodeContainer params, char *parNameNew)
{
	XNodeListItem *item = params->content;
	unsigned short i;

	for (i = 0; i < params->position; i++)
	{
		char	   *parName = item->value.singlePtr;

		if (strcmp(parName, parNameNew) == 0)
		{
			return i;
		}
		item++;
	}

	if (i == params->position)
	{
		if (i < XPATH_PARAM_MAX_COUNT)
		{
			char	   *copy = (char *) palloc(strlen(parNameNew) + 1);

			strcpy(copy, parNameNew);
			xmlnodePushSinglePtr(params, copy);
		}
		else
		{
			elog(ERROR, "the maximum number of %u xpath parameters exceeded", XPATH_PARAM_MAX_COUNT);
		}
	}
	return i;
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

/*
 * Depending on the operand type, the owning (top-level) expression may need
 * to reflect some specifics of the operand.
 */
static void
reflectOperandType(XPathExpression exprTop, XPathExprOperand operand, XPath *paths)
{
	if (operand->common.type == XPATH_OPERAND_LITERAL)
	{
		exprTop->nlits++;
	}
	else if (operand->common.type == XPATH_OPERAND_ATTRIBUTE)
	{
		exprTop->needsContext = true;
	}
	else if (operand->common.type == XPATH_OPERAND_PATH)
	{
		XPath		locPath = paths[operand->value.v.path];

		if (locPath->relative)
		{
			exprTop->needsContext = true;
		}

		exprTop->npaths++;
	}
	else if (operand->common.type == XPATH_OPERAND_FUNC_NOARG || operand->common.type == XPATH_OPERAND_FUNC)
	{
		XPathFunctionId fid;
		XPathFunction func;

		if (operand->common.type == XPATH_OPERAND_FUNC_NOARG)
		{
			fid = operand->value.v.funcId;
		}
		else
		{
			XPathExpression argList = (XPathExpression) operand;

			fid = argList->funcId;
		}

		func = &xpathFunctions[fid];
		if (func->needsContext)
		{
			exprTop->needsContext = true;
		}

		exprTop->nfuncs++;
	}
}

static void
checkOperandValueType(XPathExprOperand operand, XPathValueType valType)
{
	bool		match = false;

	if (valType == XPATH_VAL_NODESET)
	{
		switch (operand->common.type)
		{
			case XPATH_OPERAND_ATTRIBUTE:
			case XPATH_OPERAND_PATH:

				/*
				 * Parameter does not cause error but needs to be checked at
				 * runtime again.
				 */
			case XPATH_OPERAND_PARAMETER:
				match = true;
				break;

			case XPATH_OPERAND_EXPR_TOP:
			case XPATH_OPERAND_EXPR_SUB:
				{
					XPathExpression expr = (XPathExpression) operand;

					match = (expr->valType == valType);
				}
				break;

			case XPATH_OPERAND_FUNC:
			case XPATH_OPERAND_FUNC_NOARG:
				match = (getFunctionResultType(operand) == valType);
				break;
		}
	}
	else
	{
		elog(ERROR, "check of operand value type %u not implemented", valType);
	}

	if (!match)
	{
		elog(ERROR, "xpath expression operand can't be used as %s", xpathValueTypes[valType]);
	}
}

static XPathValueType
getFunctionResultType(XPathExprOperand funcOperand)
{
	XPathFunctionId fid = 0;
	XPathFunction func;

	if (funcOperand->common.type == XPATH_OPERAND_FUNC_NOARG)
	{
		fid = funcOperand->value.v.funcId;
	}
	else if (funcOperand->common.type == XPATH_OPERAND_FUNC)
	{
		XPathExpression argList = (XPathExpression) funcOperand;

		fid = argList->funcId;
	}
	else
	{
		elog(ERROR, "function expected, received operand type %u instead", funcOperand->common.type);
	}
	func = &xpathFunctions[fid];
	return func->resType;
}

static XPathExprOperatorStorage
readExpressionOperator(XPathParserState state, char *output, unsigned short *outPos)
{

	XPathExprOperatorStorage opStorage = (XPathExprOperatorStorage) ((char *) output + *outPos);
	unsigned char i;
	unsigned int incr = sizeof(XPathExprOperatorStorageData);

	checkExpressionBuffer(*outPos + incr);
	*outPos += incr;

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
			if ((i == XPATH_EXPR_OPERATOR_AND || i == XPATH_EXPR_OPERATOR_OR || i == XPATH_EXPR_OPERATOR_DIV ||
				 i == XPATH_EXPR_OPERATOR_MOD) &&
				!(XNODE_WHITESPACE(state->c) || *state->c == XNODE_CHAR_AT || *state->c == XNODE_CHAR_SLASH ||
				  *state->c == XNODE_CHAR_LBRKT_RND || *state->c == XNODE_CHAR_QUOTMARK ||
				  *state->c == XNODE_CHAR_APOSTR))
			{
				elog(ERROR, "white space or xpath operand expected at position %u (%u)", state->pos, i);
			}
			opStorage->id = ot->op.id;
			return opStorage;
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
 * Parse function argument list where 'func' contains information about arguments.
 * Any XPath expression operand, including location path or a subexpression can be the argument.
 *
 * Reading stops right after ')'.
 *
 * Number of arguments is returned.
 * This may be higher than the 'nargs' field of XPathFunctionData, as long as 'nargsSoftLimit' is true.
 */
static int
parseFunctionArgList(XPathParserState state, XPathFunction func, char *output, unsigned short *outPos,
		  XPath *paths, unsigned short *pathCnt, XMLNodeContainer paramNames)
{

	XPathExpression exprTop = (XPathExpression) output;
	bool		done = false;
	unsigned short argsMax;
	unsigned short i = 0;

	Assert(func->nargs > 0);
	argsMax = func->nargs + func->nargsSoftLimit;

	do
	{
		XPathExprOperand opnd;

		if (i == XPATH_FUNC_MAX_ARGS)
		{
			elog(ERROR, "the maximum number of XPath function arguments is %u", XPATH_FUNC_MAX_ARGS);
		}
		skipWhiteSpace(state, false);
		opnd = readExpressionOperand(exprTop, state, XPATH_TERM_NULL, output, outPos,
									 paths, pathCnt, paramNames);
		i++;
		reflectOperandType(exprTop, opnd, paths);
		skipWhiteSpace(state, false);

		if (*state->c != XNODE_CHAR_RBRKT_RND && *state->c != XNODE_CHAR_COMMA)
		{
			/* The current argument looks like a subexpression. */
			XPathExprOperatorStorage operatorStorage = readExpressionOperator(state, output, outPos);
			XPathExprOperator operator = XPATH_EXPR_OPERATOR(operatorStorage);

			skipWhiteSpace(state, false);

			if (operator != NULL)
			{
				XPathExpression subExpr = (XPathExpression) opnd;
				unsigned char termFlags = XPATH_TERM_COMMA | XPATH_TERM_RBRKT_RND;

				insertSubexpression(opnd, &operatorStorage, exprTop,
					output + *outPos - (char *) opnd, false, output, outPos);
				parseXPathExpression(subExpr, state, termFlags, operatorStorage,
								  output, outPos, true, true, paths, pathCnt,
									 paramNames);
				operator = XPATH_EXPR_OPERATOR(operatorStorage);
				subExpr->valType = operator->resType;
			}
			else
			{
				elog(ERROR, "unrecognized character '%c' found at position %u of the xpath expression",
					 *state->c, state->pos);
			}
		}

		if (*state->c == XNODE_CHAR_RBRKT_RND)
		{
			done = true;
		}
		Assert(*state->c == XNODE_CHAR_RBRKT_RND || *state->c == XNODE_CHAR_COMMA);
		nextChar(state, done);
	} while (!done);

	if (!func->nargsSoftLimit)
	{
		if (i != func->nargs)
		{
			elog(ERROR, "%u argument(s) expected when calling function %s()", func->nargs, func->name);
		}
	}
	else
	{
		if (i < func->nargs)
		{
			elog(ERROR, "at least %u argument(s) expected when calling function %s()", func->nargs, func->name);
		}
	}
	return i;
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

	for (i = 1; i <= argList->members; i++)
	{
		XPathExprOperand opnd;
		XPathValueType argType;
		XPathValueType typeRequired;

		c = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, c);
		opnd = (XPathExprOperand) c;

		if (opnd->common.type == XPATH_OPERAND_PARAMETER)
		{
			/* Parameter value type is not known at parse time. */
			continue;
		}

		if (i <= function->nargs)
		{
			/* Types of the regular arguments are taken from the definition. */
			typeRequired = typesRequired[i - 1];
		}
		else
		{
			/*
			 * The additional parameters have type of the last regular
			 * argument.
			 */
			typeRequired = typesRequired[function->nargs - 1];
		}

		if (opnd->common.type == XPATH_OPERAND_EXPR_TOP || opnd->common.type == XPATH_OPERAND_EXPR_SUB ||
			opnd->common.type == XPATH_OPERAND_FUNC)
		{
			XPathExpression subExpr = (XPathExpression) opnd;

			argType = subExpr->valType;
			c += subExpr->common.size;
		}
		else
		{
			argType = opnd->value.type;
			c += opnd->common.size;
		}

		if (argType != typeRequired)
		{
			if (typeRequired == XPATH_VAL_NODESET && argType != XPATH_VAL_NODESET)
			{
				elog(ERROR, "%s type cannot be cast to nodeset. check argument %u of %s() function",
					 xpathValueTypes[argType], i, function->name);
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
 * 'state->output'. The function returns (aligned) target pointer for the new
 * byte(s), regardless reallocation took place or not.
 */
static char *
ensureSpace(unsigned int sizeNeeded, unsigned char alignment, LocationPathOutput *output)
{
	unsigned short chunksNew = 0;
	unsigned short currentSize = output->cursor - output->data;
	char	   *target;
	unsigned char padding;

	if (alignment > 0)
	{
		target = (char *) TYPEALIGN(alignment, output->cursor);
	}
	else
	{
		target = output->cursor;
	}
	padding = target - output->cursor;

	sizeNeeded += padding;

	while (currentSize + sizeNeeded > output->size)
	{
		output->size += XPATH_PARSER_OUTPUT_CHUNK;
		chunksNew++;
	}
	if (chunksNew > 0)
	{
		output->chunks += chunksNew;
		if (output->chunks > XPATH_PARSER_OUTPUT_CHUNKS)
		{
			elog(ERROR, "XPath parser: maximum size of output exceeded");
		}
		output->data = (char *) repalloc(output->data, output->size);

		/* Make sure the pointers point to the same data in the new block. */
		output->cursor = output->data + currentSize;
		target = output->cursor + padding;

		elog(DEBUG1, "XPath output buffer reallocated");
	}
	output->cursor += sizeNeeded;
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

void
dumpXPathExpression(XPathExpression expr, XPathHeader xpathHdr, StringInfo output, bool main,
					char **paramNames, bool debug)
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
		appendStringInfo(output, " (paths / funcs: %u / %u, val. type: %u)", expr->npaths, expr->nfuncs, expr->valType);
	}
	dumpXPathExpressionInternal(&input, xpathHdr, output, 0, main, paramNames, debug);

	if (expr->variables > 0 && debug)
	{
		XPathOffset *varOffPtr = (XPathOffset *) ((char *) expr + sizeof(XPathExpressionData));
		unsigned short i;

		appendStringInfoString(output, "\n  variables:");

		for (i = 0; i < expr->variables; i++)
		{
			XPathExprOperand opnd = (XPathExprOperand) ((char *) expr + *varOffPtr);

			switch (opnd->common.type)
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

				case XPATH_OPERAND_PARAMETER:
					appendStringInfo(output, "\n    parameter: %s", paramNames[opnd->value.v.paramId]);
					break;

				default:
					elog(ERROR, "unknown type of variable: %u", opnd->common.type);
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
dumpLocationPath(XPathHeader xpathHdr, unsigned short pathNr, StringInfo output, char **paramNames,
				 bool debug)
{
	unsigned short i,
				last;
	XPathOffset o;
	XPathElement el;
	XPath		locPath = (XPath) ((char *) xpathHdr + xpathHdr->paths[pathNr]);

	if (debug)
	{
		appendStringInfo(output, "<path %u>\n\n", pathNr);
		if (locPath->relative)
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
		if (!locPath->relative)
		{
			appendStringInfoChar(output, XNODE_CHAR_SLASH);
		}
	}
	if (locPath->depth == 0)
	{
		return;
	}
	last = (locPath->targNdKind == XMLNODE_ELEMENT) ? locPath->depth : locPath->depth - 1;

	for (i = 1; i <= last; i++)
	{
		unsigned short nameLen;

		o = locPath->elements[i - 1];

		el = (XPathElement) ((char *) locPath + o);
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
			char	   *pExprUnaligned;
			XPathExpression pexpr;

			pExprUnaligned = (char *) el + sizeof(XPathElementData) + nameLen;
			pexpr = (XPathExpression) TYPEALIGN(XPATH_ALIGNOF_EXPR, pExprUnaligned);
			dumpXPathExpression(pexpr, xpathHdr, output, false, paramNames, debug);
		}
		if (!debug && ((i < last) || (locPath->targNdKind != XMLNODE_ELEMENT)))
		{
			appendStringInfoChar(output, XNODE_CHAR_SLASH);
		}
	}

	if (locPath->targNdKind != XMLNODE_ELEMENT)
	{
		if (debug)
		{
			if (locPath->targNdKind == XMLNODE_ATTRIBUTE)
			{
				appendStringInfoString(output, "\nattr. test:\t");
			}
			else
			{
				appendStringInfoString(output, "\nnode test:\t");
			}
		}

		o = locPath->elements[i - 1];

		el = (XPathElement) ((char *) locPath + o);

		if (!debug && el->descendant)
		{
			appendStringInfoChar(output, XNODE_CHAR_SLASH);
		}

		switch (locPath->targNdKind)
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
				el = (XPathElement) ((char *) locPath + locPath->elements[locPath->depth - 1]);
				if (strlen(el->name) > 0)
				{
					appendStringInfo(output, "\"%s\"", el->name);
				}
				appendStringInfoChar(output, XNODE_CHAR_RBRKT_RND);
				break;

			case XMLNODE_ATTRIBUTE:
				if (locPath->allAttributes)
				{
					appendStringInfoString(output, "@*");
				}
				else
				{
					appendStringInfo(output, "@%s", el->name);
				}
				break;

			default:
				elog(ERROR, "invalid node kind: %u", locPath->targNdKind);
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
							bool main, char **paramNames, bool debug)
{

	unsigned short i;
	XPathExpression expr = (XPathExpression) *input;

	*input += sizeof(XPathExpressionData);
	if (level == 0)
	{
		*input += XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);
	}
	if (!debug && !main && level == 0)
	{
		appendStringInfoChar(output, XNODE_CHAR_LBRACKET);
	}

	*input = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, *input);
	dumpXPathExprOperand(input, xpathHdr, output, level, paramNames, debug);

	for (i = 1; i < expr->members; i++)
	{
		dumpXPathExprOperator(input, output, level, debug);
		*input = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, *input);
		dumpXPathExprOperand(input, xpathHdr, output, level, paramNames, debug);
	}
	if (!debug && !main && level == 0)
	{
		appendStringInfoChar(output, XNODE_CHAR_RBRACKET);
	}
}

static void
dumpXPathExprOperand(char **input, XPathHeader xpathHdr, StringInfo output, unsigned short level,
					 char **paramNames, bool debug)
{

	unsigned short shortValue;
	XPathExprOperand operand = (XPathExprOperand) *input;
	XPathExpression subExpr;
	XPathFunction func;

	if (debug)
	{
		appendStringInfoChar(output, '\n');
		appendStringInfoSpaces(output, 2 * (level + 2));
	}

	switch (operand->common.type)
	{
		case XPATH_OPERAND_LITERAL:
		case XPATH_OPERAND_ATTRIBUTE:
		case XPATH_OPERAND_PATH:
		case XPATH_OPERAND_FUNC_NOARG:
		case XPATH_OPERAND_PARAMETER:
			if (operand->value.negative)
			{
				appendStringInfoChar(output, XNODE_CHAR_DASH);
			}
			break;

		case XPATH_OPERAND_FUNC:
		case XPATH_OPERAND_EXPR_SUB:
			{
				XPathExpression expr = (XPathExpression) operand;

				if (expr->negative)
				{
					appendStringInfo(output, "%c ", XNODE_CHAR_DASH);
				}
			}
			break;

		default:
			elog(ERROR, "unrecognized xpath operand type %u", operand->common.type);
			break;
	}

	switch (operand->common.type)
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
			*input += operand->common.size;
			break;

		case XPATH_OPERAND_ATTRIBUTE:
			appendStringInfo(output, "%c%s", XNODE_CHAR_AT, XPATH_STRING_LITERAL(&operand->value));
			*input += operand->common.size;
			break;

		case XPATH_OPERAND_PARAMETER:
			appendStringInfo(output, "%c%s", XNODE_CHAR_DOLLAR, paramNames[operand->value.v.paramId]);
			*input += operand->common.size;
			break;

		case XPATH_OPERAND_PATH:
			shortValue = operand->value.v.path;
			if (debug)
			{
				appendStringInfo(output, "<path %u>", shortValue);
			}
			else
			{
				dumpLocationPath(xpathHdr, shortValue, output, paramNames, debug);
			}
			*input += operand->common.size;
			break;

		case XPATH_OPERAND_EXPR_SUB:
			subExpr = (XPathExpression) *input;
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
			dumpXPathExpressionInternal(input, xpathHdr, output, level + 1, false, paramNames, debug);
			if (!debug && (subExpr->flags & XPATH_SUBEXPRESSION_EXPLICIT) == 1)
			{
				appendStringInfoChar(output, XNODE_CHAR_RBRKT_RND);
			}
			break;

		case XPATH_OPERAND_FUNC:
			subExpr = (XPathExpression) *input;
			func = &xpathFunctions[subExpr->funcId];
			appendStringInfo(output, "%s(", func->name);
			{
				unsigned short i;

				*input += sizeof(XPathExpressionData);
				for (i = 1; i <= subExpr->members; i++)
				{
					*input = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, *input);
					dumpXPathExprOperand(input, xpathHdr, output, level + 1, paramNames, debug);
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
			*input += operand->common.size;
			break;

		default:
			elog(ERROR, "unknown xpath expression operand type: %u", operand->common.type);
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
	separateOperand(debug, output, operator->id);
	appendStringInfoString(output, xpathOperators[operator->id].text);
	separateOperand(debug, output, operator->id);
	*input += sizeof(XPathExprOperatorStorageData);
}

/*
 * Operators consisting of valid XML name characters must be separated by the space.
 * Otherwise the operator becomes part of the neighbour operand.
 */
static void
separateOperand(bool debug, StringInfo output, XPathExprOperatorId id)
{
	if (!debug && (id == XPATH_EXPR_OPERATOR_AND || id == XPATH_EXPR_OPERATOR_OR ||
			 id == XPATH_EXPR_OPERATOR_DIV || id == XPATH_EXPR_OPERATOR_MOD))
	{
		appendStringInfoSpaces(output, 1);
	}
}
