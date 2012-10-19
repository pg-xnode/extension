/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"
#include "utils/builtins.h"

#include "xpath.h"
#include "xmlnode_util.h"

static void evaluateXPathOperand(XPathExprState exprState, XPathExprOperand operand,
				unsigned short recursionLevel, XPathExprOperandValue result);
static void evaluateXPathFunction(XPathExprState exprState, XPathExpression funcExpr,
				unsigned short recursionLevel, XPathExprOperandValue result);
static void prepareLiteral(XPathExprState exprState, XPathExprOperand operand);
static void evaluateBinaryOperator(XPathExprState exprState, XPathExprOperandValue valueLeft,
					   XPathExprOperandValue valueRight, XPathExprOperator operator, XPathExprOperandValue result);

static bool considerSubScan(XPathElement xpEl, XMLNodeHdr node, XMLScan xscan, bool subScanJustDone);
static void addNodeToIgnoreList(XMLNodeHdr node, XMLScan scan);

static void substituteAttributes(XPathExprState exprState, XMLCompNodeHdr element);
static void substitutePaths(XPathExpression expression, XPathExprState exprState, XMLCompNodeHdr element,
				xmldoc document, XPathHeader xpHdr);
static void substituteFunctions(XPathExpression expression, XPathExprState exprState, XMLScan xscan);

static void compareNumValues(XPathExprState exprState, XPathExprOperandValue valueLeft,
				 XPathExprOperandValue valueRight, XPathExprOperator operator, XPathExprOperandValue result);
static bool xmlNumberToDouble(XPathExprState exprState, XPathExprOperandValue numOperand, double *simple);
static void compareNumbers(double numLeft, double numRight, XPathExprOperator operator,
			   XPathExprOperandValue result);
static bool compareNodeSets(XPathExprState exprState, XPathNodeSet ns1, XPathNodeSet ns2, XPathExprOperator operator);
static bool compareElements(XMLCompNodeHdr elLeft, XMLCompNodeHdr elRight);
static bool compareValueToNode(XPathExprState exprState, XPathExprOperandValue value, XMLNodeHdr node,
				   XPathExprOperator operator);
static void compareNumToStr(double num, char *numStr, XPathExprOperator operator,
				XPathExprOperandValue result);
static bool compareValueToNodeSet(XPathExprState exprState, XPathExprOperandValue value, XPathNodeSet ns,
					  XPathExprOperator operator);
static bool isOnIgnoreList(XMLNodeHdr node, XMLScan scan);
static void getUnion(XPathExprState exprState, XPathNodeSet setLeft, XPathNodeSet setRight, XPathNodeSet setResult);
static void copyNodeSet(XPathExprState exprState, XPathNodeSet nodeSet, XMLNodeHdr *output, unsigned int *position);
static int	nodePtrComparator(const void *arg1, const void *arg2);
static void flipOperandValue(XPathExprOperandValue value, XPathExprState exprState);

/*
 * 'xscan' - the scan to be initialized 'xpath' - location path to be used
 * for this scan
 *
 * 'xpHdr' - header of the 'path set' the 'xpath' is contained in.
 * We may need it if one or more predicates in 'xpath' contain other paths (subpaths) as operands.
 *
 * 'scanRoot' - node where the scan starts. This node won't be tested itself, the scan starts
 * one level lower. Typically, this is document node.
 */
void
initXMLScan(XMLScan xscan, XMLScan parent, XPath xpath, XPathHeader xpHdr, XMLCompNodeHdr scanRoot,
			xmldoc document, bool checkUniqueness)
{
	XMLScanOneLevel firstLevel;

	xscan->done = false;

	xscan->xpath = xpath;

	xscan->xpathHeader = xpHdr;
	xscan->xpathRoot = 0;
	if (xscan->xpath->depth > 0)
	{
		firstLevel = (XMLScanOneLevel) palloc(xscan->xpath->depth * sizeof(XMLScanOneLevelData));
		firstLevel->parent = scanRoot;
		firstLevel->nodeRefPtr = XNODE_FIRST_REF(scanRoot);
		firstLevel->siblingsLeft = scanRoot->children;
		firstLevel->contextPosition = 0;
		firstLevel->contextSizeKnown = false;
		firstLevel->up = (parent == NULL) ? NULL : XMLSCAN_CURRENT_LEVEL(parent);

		xscan->state = firstLevel;
	}
	else
	{
		xscan->state = NULL;
	}

	xscan->depth = 0;
	xscan->skip = false;
	xscan->document = document;

	xscan->parent = parent;

	if (checkUniqueness)
	{
		/*
		 * Only top-level scan can have the container, to share it with
		 * sub-scans.
		 */
		if (xscan->parent == NULL)
		{
			xscan->ignoreList = (XMLNodeContainer) palloc(sizeof(XMLNodeContainerData));
			xmlnodeContainerInit(xscan->ignoreList);
		}
		else
		{
			xscan->ignoreList = xscan->parent->ignoreList;
		}
	}
	else
	{
		xscan->ignoreList = NULL;
	}


	xscan->subScan = NULL;
}

void
finalizeXMLScan(XMLScan xscan)
{
	if (xscan->state != NULL)
	{
		pfree(xscan->state);
		xscan->state = NULL;
	}
	if (xscan->ignoreList != NULL && xscan->parent == NULL)
	{
		xmlnodeContainerFree(xscan->ignoreList);
		pfree(xscan->ignoreList);
	}
}

/*
 * Find the next matching node. The XML scan itself.
 *
 * 'xscan' - scan status. It remembers where the last scan ended. If used for consequent call, the function
 * will continue right after that position.
 * If caller changes the document between calls to this function, he is responsible for adjusting the
 * scan status and - if some exist - state of any sub-scan. In addition, 'xscan->document' of the scan and
 * sub-scans has to point to the new (modified) document.
 *
 *
 * Returns a pointer to the matching node. This points to inside 'xscan->document' and therefore
 * must not be pfree'd.
 */
XMLNodeHdr
getNextXMLNode(XMLScan xscan)
{
	if (xscan->state == NULL)
	{
		elog(ERROR, "XML scan state is not well initialized");
	}

	while (true)
	{
		XMLScanOneLevel scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);

		while (scanLevel->siblingsLeft > 0)
		{
			XPathElement xpEl;
			XMLCompNodeHdr eh = scanLevel->parent;
			XMLNodeHdr	currentNode = NULL;
			XMLNodeKind cKind;

			/*
			 * Indicates later in the loop whether sub-scan has finished in
			 * the current iteration.
			 */
			bool		subScanDone = false;

			if (scanLevel->nodeRefPtr == NULL)
			{

				if (scanLevel->siblingsLeft != 1)
				{
					elog(ERROR, "undefined state of xml scan");
				}

				/* Done for the current level. */
				scanLevel->siblingsLeft--;
				break;
			}

			xpEl = XPATH_CURRENT_LEVEL(xscan);

			if (xscan->subScan != NULL)
			{
				XMLNodeHdr	subNode = getNextXMLNode(xscan->subScan);

				/*
				 * isOnIgnoreList() is not used here on return because the
				 * check has been performed when returning from the
				 * getNextXMLNode() above.
				 *
				 * addToIgnoreList() is not used bellow for the same reason.
				 */
				if (subNode != NULL)
				{
					return subNode;
				}
				else
				{
					finalizeXMLScan(xscan->subScan);
					pfree(xscan->subScan);
					xscan->subScan = NULL;
					xscan->skip = true;
					subScanDone = true;
				}
			}

			currentNode = (XMLNodeHdr) ((char *) eh -
										readXMLNodeOffset(&scanLevel->nodeRefPtr, XNODE_GET_REF_BWIDTH(eh), false));

			/* CDATA should be treated as text node. */
			cKind = currentNode->kind == XMLNODE_CDATA ? XMLNODE_TEXT : currentNode->kind;

			if (xscan->skip)
			{
				/*
				 * The current node is already processed, but the possible
				 * descendant axe hasn't been considered yet.
				 *
				 * considerSubScan() is used twice in the loop It would be
				 * possible to call it just once, at the beginning. But thus
				 * the descendants would be returned before the element
				 * itself.
				 */
				if (considerSubScan(xpEl, currentNode, xscan, subScanDone))
				{
					continue;
				}

				xscan->skip = false;

				/*
				 * The current node is processed and no subscan is needed, so
				 * just move ahead.
				 */
				scanLevel->nodeRefPtr = XNODE_NEXT_REF(scanLevel->nodeRefPtr, eh);
				scanLevel->siblingsLeft--;
				continue;
			}

			/*
			 * Evaluate the node according to its type
			 */
			if (cKind == XMLNODE_ELEMENT &&
				!(XPATH_LAST_LEVEL(xscan) && xscan->xpath->targNdKind != XMLNODE_ELEMENT))
			{
				XMLCompNodeHdr currentElement = (XMLCompNodeHdr) currentNode;
				char	   *childFirst = XNODE_FIRST_REF(currentElement);
				char	   *name = XNODE_ELEMENT_NAME(currentElement);
				char	   *nameTest = xpEl->name;

				if (XPATH_LAST_LEVEL(xscan) && xscan->xpath->targNdKind == XMLNODE_NODE &&
					!isOnIgnoreList(currentNode, xscan))
				{
					xscan->skip = true;
					addNodeToIgnoreList(currentNode, xscan);
					return currentNode;
				}
				if (strcmp(name, nameTest) == 0)
				{
					bool		passed = true;

					scanLevel->contextPosition++;
					if (xpEl->hasPredicate)
					{
						XPathExprOperandValueData result;
						char	   *exprUnaligned;
						XPathExpression exprOrig;
						XPathExprState exprState;

						exprUnaligned = (char *) xpEl + sizeof(XPathElementData) +
							strlen(nameTest);
						exprOrig = (XPathExpression) TYPEALIGN(XPATH_ALIGNOF_EXPR, exprUnaligned);
						exprState = prepareXPathExpression(exprOrig, currentElement,
								 xscan->document, xscan->xpathHeader, xscan);
						evaluateXPathExpression(exprState, exprState->expr, 0, &result);

						if (result.isNull)
						{
							passed = false;
						}
						else
						{
							if (result.type == XPATH_VAL_NUMBER)
							{
								XPathExprOperandValueData resSigned;

								/*
								 * The cast is used to get rid of the
								 * 'negative' flag.
								 */
								castXPathExprOperandToNum(exprState, &result, &resSigned, false);
								if (resSigned.v.num <= 0.0f || resSigned.v.num > XMLNODE_MAX_CHILDREN)
								{
									passed = false;
								}
								else
								{
									int2		posInt2 = DatumGetInt16(DirectFunctionCall1Coll(dtoi2, InvalidOid, Float8GetDatum(resSigned.v.num)));

									passed = (scanLevel->contextPosition == posInt2);
								}
							}
							else
							{
								XPathExprOperandValueData resultCast;

								castXPathExprOperandToBool(exprState, &result, &resultCast);
								passed = resultCast.v.boolean;
							}
						}
						freeExpressionState(exprState);
					}

					if (!passed)
					{
						XMLCompNodeHdr eh = scanLevel->parent;

						scanLevel->nodeRefPtr = XNODE_NEXT_REF(scanLevel->nodeRefPtr, eh);
						scanLevel->siblingsLeft--;
						continue;
					}
					if (!XPATH_LAST_LEVEL(xscan) && XNODE_HAS_CHILDREN(currentElement))
					{
						/*
						 * Avoid descent to lower levels if all nodes at the
						 * next level are attributes and attribute is not
						 * target of the xpath
						 */
						if ((currentElement->common.flags & XNODE_EMPTY) &&
							xscan->xpath->targNdKind != XMLNODE_ATTRIBUTE)
						{

							XMLCompNodeHdr eh = scanLevel->parent;

							scanLevel->nodeRefPtr = XNODE_NEXT_REF(scanLevel->nodeRefPtr, eh);
							scanLevel->siblingsLeft--;
							continue;
						}
						else
						{
							XMLScanOneLevel nextLevel;

							/*
							 * Initialize the scan state for the next (deeper)
							 * level
							 */
							xscan->depth++;
							nextLevel = XMLSCAN_CURRENT_LEVEL(xscan);
							nextLevel->parent = currentElement;
							nextLevel->nodeRefPtr = childFirst;
							nextLevel->siblingsLeft = currentElement->children;
							nextLevel->contextPosition = 0;
							nextLevel->contextSizeKnown = false;
							nextLevel->up = scanLevel;
							break;
						}
					}
					else if (XPATH_LAST_LEVEL(xscan) && !isOnIgnoreList(currentNode, xscan))
					{
						/*
						 * We're at the end of the xpath.
						 */
						xscan->skip = true;
						/* Return the matching node. */
						addNodeToIgnoreList(currentNode, xscan);
						return currentNode;
					}
				}
			}
			else if (XPATH_LAST_LEVEL(xscan) && !isOnIgnoreList(currentNode, xscan))
			{
				/*
				 * If the 'targNdKind' is XMLNODE_ELEMENT, it means that the
				 * kind did match with path step above, but the node name did
				 * not. In such a case we're not interested in the current
				 * node anymore.
				 */

				if (cKind == xscan->xpath->targNdKind && xscan->xpath->targNdKind != XMLNODE_ELEMENT)
				{
					if (cKind == XMLNODE_TEXT || cKind == XMLNODE_COMMENT)
					{
						xscan->skip = true;
						addNodeToIgnoreList(currentNode, xscan);
						return currentNode;
					}
					else if (cKind == XMLNODE_PI)
					{
						char	   *piTarget = (char *) (currentNode + 1);
						char	   *piTargTest = xpEl->name;

						if (xscan->xpath->piTestValue)
						{
							if (strcmp(piTarget, piTargTest) == 0)
							{
								xscan->skip = true;
								addNodeToIgnoreList(currentNode, xscan);
								return currentNode;
							}
						}
						else
						{
							xscan->skip = true;
							addNodeToIgnoreList(currentNode, xscan);
							return currentNode;
						}

					}
					else if (cKind == XMLNODE_ATTRIBUTE)
					{
						if (xscan->xpath->allAttributes)
						{
							xscan->skip = true;
							addNodeToIgnoreList(currentNode, xscan);
							return currentNode;
						}
						else
						{
							char	   *attrName = (char *) currentNode + sizeof(XMLNodeHdrData);
							char	   *attrNameTest = xpEl->name;

							if (strcmp(attrNameTest, attrName) == 0)
							{
								xscan->skip = true;
								addNodeToIgnoreList(currentNode, xscan);
								return currentNode;
							}
						}
					}
				}
				else if (xscan->xpath->targNdKind == XMLNODE_NODE && currentNode->kind != XMLNODE_ATTRIBUTE)
				{
					xscan->skip = true;
					addNodeToIgnoreList(currentNode, xscan);
					return currentNode;
				}
			}

			/*
			 * No match found. The current path element however might be
			 * interested in descendants.
			 */
			if (considerSubScan(xpEl, currentNode, xscan, subScanDone))
			{
				continue;
			}

			/*
			 * Whether descendants found or not, go to the next element on the
			 * current level
			 */
			scanLevel->nodeRefPtr = XNODE_NEXT_REF(scanLevel->nodeRefPtr, scanLevel->parent);
			scanLevel->siblingsLeft--;
		}

		/*
		 * If descent to next level has just been prepared (see the 'break'
		 * statement after initialization of next level), we're not yet at the
		 * end and the following condition can't be met. Otherwise we're done
		 * with the current level.
		 */
		if (scanLevel->siblingsLeft == 0)
		{
			if (xscan->depth == 0)
			{
				xscan->done = true;

				return NULL;
			}
			else
			{
				xscan->skip = true;
				xscan->depth--;
				continue;
			}
		}
	}
	/* keep the compiler silent */
	return NULL;
}


/*
 * Returns expression state, containing a (mutable) copy of an expression as well as
 * values of operands that had to be substituted.
 *
 * The function does process all sub-expressions (implicit / explicit) and
 * (sub)paths. When it tries to substitute node-sets for sub-paths, it has to
 * process predicate expressions of those sub-paths (which can again contain
 * sub-paths, etc.)
 */
XPathExprState
prepareXPathExpression(XPathExpression exprOrig, XMLCompNodeHdr ctxElem,
					   xmldoc document, XPathHeader xpHdr, XMLScan xscan)
{
	XPathExpression expr;
	XPathExprState state = createXPathVarCache(XPATH_VAR_CACHE_DEF_SIZE);

	expr = (XPathExpression) palloc(exprOrig->common.size);
	memcpy(expr, exprOrig, exprOrig->common.size);
	state->expr = expr;

	/* Replace attribute names with the values found in the current node.  */
	if (ctxElem != NULL)
	{
		substituteAttributes(state, ctxElem);
	}

	if (expr->npaths > 0)
	{
		/* Replace paths with matching node-sets. */
		substitutePaths(expr, state, ctxElem, document, xpHdr);
	}

	if (expr->nfuncs)
	{
		/*
		 * Functions having no arguments can also be replaced by value before
		 * evaluation starts.
		 */
		substituteFunctions(expr, state, xscan);
	}
	return state;
}


/*
 * Size is estimated for the initial allocation so that reallocation may not be needed.
 */
void
allocXPathExpressionVarCache(XPathExprState state, XPathExprVar varKind, unsigned int increment, bool init)
{
	unsigned short unitSize;
	unsigned int maxOrig,
				sizeOrig,
				size;
	void	   *chunkNew,
			   *chunkOrig = NULL;

	if (increment == 0)
	{
		elog(ERROR, "increment of variable cache size must be greater than zero");
	}

	if (init)
	{
		state->count[varKind] = 0;
		state->countMax[varKind] = 0;
	}

	switch (varKind)
	{
		case XPATH_VAR_STRING:
			if (init)
			{
				state->strings = NULL;
			}
			chunkOrig = state->strings;
			unitSize = sizeof(char *);
			break;

		case XPATH_VAR_NODE_SINGLE:
			if (init)
			{
				state->nodes = NULL;
			}
			chunkOrig = state->nodes;
			unitSize = sizeof(XMLNodeHdr);
			break;

		case XPATH_VAR_NODE_ARRAY:
			if (init)
			{
				state->nodeSets = NULL;
			}
			chunkOrig = state->nodeSets;
			unitSize = sizeof(XMLNodeHdr *);
			break;

		default:
			elog(ERROR, "unrecognized variable type %u", varKind);
			break;
	}


	maxOrig = state->countMax[varKind];
	state->countMax[varKind] += increment;


	size = state->countMax[varKind] * unitSize;
	sizeOrig = maxOrig * unitSize;

	if (chunkOrig == NULL)
	{
		chunkNew = palloc(size);
		memset(chunkNew, 0, size);
	}
	else
	{
		chunkNew = repalloc(chunkOrig, size);
		memset((char *) chunkNew + sizeOrig, 0, size - sizeOrig);
	}

	switch (varKind)
	{
		case XPATH_VAR_STRING:
			state->strings = (char **) chunkNew;
			break;

		case XPATH_VAR_NODE_SINGLE:
			state->nodes = (XMLNodeHdr *) chunkNew;
			break;

		case XPATH_VAR_NODE_ARRAY:
			state->nodeSets = (XMLNodeHdr **) chunkNew;
			break;

		default:
			elog(ERROR, "unrecognized variable type %u", varKind);
			break;
	}
}

XPathExprState
createXPathVarCache(unsigned int size)
{
	XPathExprState state = (XPathExprState) palloc(sizeof(XPathExprStateData));

	allocXPathExpressionVarCache(state, XPATH_VAR_STRING, size, true);
	allocXPathExpressionVarCache(state, XPATH_VAR_NODE_SINGLE, size, true);
	allocXPathExpressionVarCache(state, XPATH_VAR_NODE_ARRAY, size, true);
	state->expr = NULL;
	return state;
}

/*
 * 'element' is the 'owning element' of the predicate that we're just
 * evaluating. In general - a context node.
 */
void
evaluateXPathExpression(XPathExprState exprState, XPathExpression expr, unsigned short recursionLevel,
						XPathExprOperandValue result)
{
	unsigned short i;
	char	   *currentPtr = (char *) expr;
	XPathExprOperand currentOpnd;
	XPathExprOperator operator = NULL;

	currentPtr += sizeof(XPathExpressionData);
	Assert(PointerIsAligned(currentPtr, XPATH_ALIGNOF_OFFSET));

	if (recursionLevel == 0)
	{
		unsigned short varSize = XPATH_EXPR_VAR_MAX * sizeof(XPathOffset);

		currentPtr += varSize;
	}

	for (i = 0; i < expr->members; i++)
	{
		XPathExprOperandValueData resTmp,
					currentVal;

		if (i > 0)
		{
			operator = XPATH_EXPR_OPERATOR(currentPtr);
			currentPtr += sizeof(XPathExprOperatorStorageData);
		}

		currentPtr = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, currentPtr);
		currentOpnd = (XPathExprOperand) currentPtr;
		currentPtr += currentOpnd->common.size;

		prepareLiteral(exprState, currentOpnd);
		evaluateXPathOperand(exprState, currentOpnd, recursionLevel, &currentVal);

		if (i == 0)
		{
			memcpy(result, &currentVal, sizeof(XPathExprOperandValueData));
		}

		if (expr->members == 1)
		{
			break;
		}

		if (i == 0)
			continue;

		evaluateBinaryOperator(exprState, result, &currentVal, operator, &resTmp);
		memcpy(result, &resTmp, sizeof(XPathExprOperandValueData));

		/*
		 * Short evaluation.
		 *
		 * If the operator is OR, then all operators on the same level must be
		 * OR too. The same applies to AND. These operators are unique in
		 * terms of precedence.
		 */
		if (operator->id == XPATH_EXPR_OPERATOR_OR)
		{
			Assert(result->type == XPATH_VAL_BOOLEAN);
			if (!result->isNull && result->v.boolean)
			{
				break;
			}
		}
		else if (operator->id == XPATH_EXPR_OPERATOR_AND)
		{
			Assert(result->type == XPATH_VAL_BOOLEAN);
			if (result->isNull || (!result->isNull && !result->v.boolean))
			{
				break;;
			}
		}
	}

	/*
	 * Both evaluateXPathOperand() and evaluateBinaryOperator() must ensure
	 * that the actual value (result->v.num) is negative by now if the
	 * 'result->negative' was originally 'true'.
	 */
	Assert(!result->negative);

	result->negative = expr->negative;

	if (result->negative)
	{
		flipOperandValue(result, exprState);
	}
}

void
freeExpressionState(XPathExprState state)
{
	if (state->strings)
	{
		pfree(state->strings);
	}

	if (state->nodes)
	{
		pfree(state->nodes);
	}

	if (state->nodeSets)
	{
		unsigned short i;

		for (i = 0; i < state->countMax[XPATH_VAR_NODE_ARRAY]; i++)
		{
			XMLNodeHdr *ns = state->nodeSets[i];

			if (ns == NULL)
			{
				break;
			}
			pfree(ns);
		}
		pfree(state->nodeSets);
	}
	pfree(state);
}

static void
evaluateXPathOperand(XPathExprState exprState, XPathExprOperand operand, unsigned short recursionLevel,
					 XPathExprOperandValue result)
{
	XPathExpression expr = (XPathExpression) operand;

	if (operand->common.type == XPATH_OPERAND_EXPR_SUB)
		evaluateXPathExpression(exprState, expr, recursionLevel + 1, result);
	else if (operand->common.type == XPATH_OPERAND_FUNC)
		evaluateXPathFunction(exprState, expr, recursionLevel + 1, result);
	else
	{
		if (operand->value.negative)
		{
			castXPathExprOperandToNum(exprState, &operand->value, result, false);
		}
		else
		{
			memcpy(result, &operand->value, sizeof(XPathExprOperandValueData));
		}
	}
}


/*
 * Evaluate XPath function having non-empty argument list.
 *
 * If a function has no arguments, its value only depends on context
 * and as such has already been evaluated by substituteFunctions().
 */
static void
evaluateXPathFunction(XPathExprState exprState, XPathExpression funcExpr, unsigned short recursionLevel,
					  XPathExprOperandValue result)
{
	char	   *c;
	unsigned short i;
	XPathExprOperandValue args,
				argCursor;
	XPathFunctionId funcId;
	XPathFunction function;
	XpathFuncImpl funcImpl;
	XPathValueType *argTypes;

	argCursor = args = (XPathExprOperandValue) palloc(funcExpr->members * sizeof(XPathExprOperandValueData));

	c = (char *) funcExpr + sizeof(XPathExpressionData);

	funcId = funcExpr->funcId;
	function = &xpathFunctions[funcId];
	argTypes = function->argTypes;

	/*
	 * Prepare each argument: evaluate it if it's expression or function or
	 * just copy if it's a single operand.
	 *
	 * 'args' will contain the arguments to pass..
	 *
	 */
	for (i = 0; i < funcExpr->members; i++)
	{
		XPathExprOperand opnd;
		XPathExprOperandValueData argValue;
		XPathValueType targetType;

		c = (char *) TYPEALIGN(XPATH_ALIGNOF_OPERAND, c);
		opnd = (XPathExprOperand) c;

		if (opnd->common.type == XPATH_OPERAND_EXPR_TOP || opnd->common.type == XPATH_OPERAND_EXPR_SUB ||
			opnd->common.type == XPATH_OPERAND_FUNC)
		{
			XPathExpression subExpr = (XPathExpression) opnd;

			if (opnd->common.type == XPATH_OPERAND_FUNC)
			{
				/* In this case, 'subExpr' means 'argument list'. */
				evaluateXPathFunction(exprState, subExpr, recursionLevel, &argValue);
			}
			else
			{
				evaluateXPathExpression(exprState, subExpr, recursionLevel, &argValue);
			}
		}
		else
		{
			prepareLiteral(exprState, opnd);
			evaluateXPathOperand(exprState, opnd, recursionLevel, &argValue);
		}

		/*
		 * Cast the argument now so that implementation function can assume
		 * having received the desired type(s).
		 */

		if (i < function->nargs)
		{
			targetType = argTypes[i];
		}
		else if (function->variadic)
		{
			targetType = argTypes[function->nargs - 1];
		}
		else
		{
			/* Parser should not allow this if working as supposed to. */
			targetType = XPATH_VAL_OBJECT;		/* Keep the compiler silent. */
			elog(ERROR, "too many arguments passed to a function");
		}

		switch (targetType)
		{
			case XPATH_VAL_BOOLEAN:
				castXPathExprOperandToBool(exprState, &argValue, argCursor);
				break;

			case XPATH_VAL_NUMBER:
				castXPathExprOperandToNum(exprState, &argValue, argCursor, false);
				break;

			case XPATH_VAL_STRING:
				castXPathExprOperandToStr(exprState, &argValue, argCursor);
				break;

			case XPATH_VAL_NODESET:
				if (argValue.type == XPATH_VAL_NODESET)
				{
					memcpy(argCursor, &argValue, sizeof(XPathExprOperandValueData));
				}
				else
					elog(ERROR, "%s type cannot be cast to nodeset. check argument %u of %s() function",
						 xpathValueTypes[argValue.type], i, function->name);
				break;

			case XPATH_VAL_OBJECT:
				/* Function accepts anything, so just copy it. */
				memcpy(argCursor, &argValue, sizeof(XPathExprOperandValueData));
				break;

			default:
				elog(ERROR, "unrecognized type to cast to: %u", targetType);
				break;

		}

		c += opnd->common.size;
		argCursor++;
	}

	funcImpl = function->impl.args;
	result->negative = false;
	/* The implementation function may set this to 'true'. */
	result->isNull = false;
	/* Call the function. */
	funcImpl(exprState, funcExpr->members, args, result);
	pfree(args);

	result->type = function->resType;

	if (result->negative)
	{
		elog(ERROR, "function implementation is not expected to set 'negative' flag");
	}
	result->negative = funcExpr->negative;
	if (result->negative)
	{
		flipOperandValue(result, exprState);
	}
}

/*
 * Find out whether the literal is in buffer that the appropriate function received from FMGR
 * or is it a copy.
 */
static void
prepareLiteral(XPathExprState exprState, XPathExprOperand operand)
{
	if (operand->common.type == XPATH_OPERAND_LITERAL && operand->value.type == XPATH_VAL_STRING)
	{
		operand->value.v.stringId = getXPathOperandId(exprState, XPATH_STRING_LITERAL(&operand->value),
													  XPATH_VAR_STRING);
	}
}

static void
evaluateBinaryOperator(XPathExprState exprState, XPathExprOperandValue valueLeft,
					   XPathExprOperandValue valueRight, XPathExprOperator operator, XPathExprOperandValue result)
{


	/* If numeric value is the output, the value will reflect the sign itself */
	result->negative = false;

	if (operator->id == XPATH_EXPR_OPERATOR_OR || operator->id == XPATH_EXPR_OPERATOR_AND)
	{
		bool		left,
					right;
		XPathExprOperandValueData valueTmp;

		result->isNull = false;
		if (valueLeft->type == XPATH_VAL_NODESET && valueRight->type == XPATH_VAL_NODESET &&
		 valueLeft->v.nodeSet.isDocument && valueRight->v.nodeSet.isDocument)
		{
			elog(ERROR, "invalid xpath expression");
		}

		/*
		 * Subexpression is currently the only type of operand of boolean type
		 */
		castXPathExprOperandToBool(exprState, valueLeft, &valueTmp);
		left = valueTmp.v.boolean;
		castXPathExprOperandToBool(exprState, valueRight, &valueTmp);
		right = valueTmp.v.boolean;

		result->type = XPATH_VAL_BOOLEAN;
		result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_OR) ? left || right : left && right;
	}
	else if (operator->id == XPATH_EXPR_OPERATOR_EQ || operator->id == XPATH_EXPR_OPERATOR_NEQ)
	{
		bool		equal;
		XPathValueType typeLeft = valueLeft->type;
		XPathValueType typeRight = valueRight->type;
		XPathExprOperandValueData valueTmp;

		result->isNull = false;
		result->type = XPATH_VAL_BOOLEAN;
		if (typeLeft == XPATH_VAL_BOOLEAN || typeRight == XPATH_VAL_BOOLEAN)
		{
			bool		boolLeft,
						boolRight;

			/*
			 * As we don't know which one is boolean and which one is
			 * something else, cast both values to boolean
			 */
			castXPathExprOperandToBool(exprState, valueLeft, &valueTmp);
			boolLeft = valueTmp.v.boolean;
			castXPathExprOperandToBool(exprState, valueRight, &valueTmp);
			boolRight = valueTmp.v.boolean;
			;
			equal = boolLeft == boolRight;
			result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_EQ) ? equal : !equal;
			return;
		}
		else if (typeLeft == XPATH_VAL_NUMBER || typeRight == XPATH_VAL_NUMBER)
		{
			compareNumValues(exprState, valueLeft, valueRight, operator, result);
			return;
		}
		else
		{
			/*
			 * compare 2 strings, 2 node-sets or a combination of both.
			 */
			if (typeLeft == XPATH_VAL_NODESET && typeRight == XPATH_VAL_NODESET)
			{
				XPathNodeSet nsLeft = &valueLeft->v.nodeSet;
				XPathNodeSet nsRight = &valueRight->v.nodeSet;

				/*
				 * Document is not a typical node-set, let's exclude it first.
				 */
				if (nsLeft->isDocument && nsRight->isDocument)
				{
					result->v.boolean = operator->id == XPATH_EXPR_OPERATOR_EQ;
					return;
				}
				else if (nsLeft->isDocument || nsRight->isDocument)
				{
					XPathNodeSet setDoc,
								setNonDoc;

					if (nsLeft->isDocument)
					{
						setDoc = nsLeft;
						setNonDoc = nsRight;
					}
					else
					{
						setDoc = nsRight;
						setNonDoc = nsLeft;
					}

					if (setNonDoc->count > 0)
					{
						result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_EQ);
					}
					else
					{
						result->v.boolean = false;
					}

					return;
				}

				if (nsLeft->count == 0 || nsRight->count == 0)
				{
					result->v.boolean = false;
					return;
				}
				result->v.boolean = compareNodeSets(exprState, &valueLeft->v.nodeSet, &valueRight->v.nodeSet,
													operator);
				return;
			}
			else if (typeLeft == XPATH_VAL_NODESET || typeRight == XPATH_VAL_NODESET)
			{
				/*
				 * One operand is a simple (string) value, the other is a node
				 * set
				 */
				XPathExprOperandValue setValue,
							nonSetValue;
				XPathNodeSet nodeSet;

				if (typeLeft == XPATH_VAL_NODESET)
				{
					setValue = valueLeft;
					nonSetValue = valueRight;
				}
				else
				{
					setValue = valueRight;
					nonSetValue = valueLeft;
				}

				if (setValue->v.nodeSet.isDocument)
				{
					result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_NEQ);
					return;
				}
				nodeSet = &setValue->v.nodeSet;

				/*
				 * The previous conditions should have caught 'number to
				 * node-set' and 'boolean to node-set' comparisons.
				 */
				Assert(nonSetValue->type == XPATH_VAL_STRING);

				if (nonSetValue->isNull || nodeSet->count == 0)
				{
					/*
					 * If either operand is null, comparison makes no sense.
					 * (No match even if XPATH_EXPR_OPERATOR_EQ is the
					 * operator).
					 */
					result->v.boolean = false;
					return;
				}
				result->v.boolean = compareValueToNodeSet(exprState, nonSetValue, nodeSet, operator);
				return;
			}

			/*
			 * Compare 2 'simple strings', i.e. none is contained in a
			 * node-set.
			 */
			if (!valueLeft->isNull && !valueRight->isNull)
			{
				char	   *strLeft,
						   *strRight;

				strLeft = getXPathOperandValue(exprState, valueLeft->v.stringId, XPATH_VAR_STRING);
				strRight = getXPathOperandValue(exprState, valueRight->v.stringId, XPATH_VAR_STRING);

				equal = strcmp(strLeft, strRight) == 0;
				result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_EQ) ? equal : !equal;
				return;
			}
			else
			{
				/*
				 * One or both sides are null. It makes no sense to say
				 * whether the operands are equal or non-equal.
				 */
				result->v.boolean = false;
				return;
			}
		}
	}
	else if (operator->id == XPATH_EXPR_OPERATOR_LT || operator->id == XPATH_EXPR_OPERATOR_GT ||
			 operator->id == XPATH_EXPR_OPERATOR_LTE || operator->id == XPATH_EXPR_OPERATOR_GTE)
	{
		compareNumValues(exprState, valueLeft, valueRight, operator, result);
	}
	else if (operator->id == XPATH_EXPR_OPERATOR_PLUS || operator->id == XPATH_EXPR_OPERATOR_MINUS ||
			 operator->id == XPATH_EXPR_OPERATOR_MULTIPLY || operator->id == XPATH_EXPR_OPERATOR_DIV ||
			 operator->id == XPATH_EXPR_OPERATOR_MOD)
	{
		XPathExprOperandValueData numLeft,
					numRight;

		result->isNull = false;
		result->type = XPATH_VAL_NUMBER;
		castXPathExprOperandToNum(exprState, valueLeft, &numLeft, false);
		castXPathExprOperandToNum(exprState, valueRight, &numRight, false);

		if (numLeft.isNull || numRight.isNull)
		{
			result->isNull = true;
			return;
		}

		if (numLeft.negative)
		{
			numLeft.v.num *= -1.0f;
		}
		if (numRight.negative)
		{
			numRight.v.num *= -1.0f;
		}

		switch (operator->id)
		{
			case XPATH_EXPR_OPERATOR_PLUS:
				result->v.num = numLeft.v.num + numRight.v.num;
				break;

			case XPATH_EXPR_OPERATOR_MINUS:
				result->v.num = numLeft.v.num - numRight.v.num;
				break;

			case XPATH_EXPR_OPERATOR_MULTIPLY:
				result->v.num = numLeft.v.num * numRight.v.num;
				break;

			case XPATH_EXPR_OPERATOR_DIV:
			case XPATH_EXPR_OPERATOR_MOD:
				result->v.num = DatumGetFloat8(DirectFunctionCall2Coll(float8div, InvalidOid,
																	   Float8GetDatum(numLeft.v.num), Float8GetDatum(numRight.v.num)));

				if (operator->id == XPATH_EXPR_OPERATOR_MOD)
				{
					float8		truncated = DatumGetFloat8(DirectFunctionCall1Coll(dtrunc, InvalidOid, Float8GetDatum(result->v.num)));

					result->v.num = numLeft.v.num - truncated * numRight.v.num;
				}
				break;
			default:
				elog(ERROR, "unrecognized operator %u", operator->id);
				break;
		}
	}
	else if (operator->id == XPATH_EXPR_OPERATOR_UNION)
	{
		XPathNodeSet nodeSetLeft,
					nodeSetRight;

		if (valueLeft->type != XPATH_VAL_NODESET || valueRight->type != XPATH_VAL_NODESET)
		{
			/* Parser shouldn't allow this, but let's check anyway. */
			elog(ERROR, "union operator expects nodeset on both sides");
		}

		nodeSetLeft = &valueLeft->v.nodeSet;
		nodeSetRight = &valueRight->v.nodeSet;
		getUnion(exprState, nodeSetLeft, nodeSetRight, &result->v.nodeSet);
		result->isNull = (result->v.nodeSet.count == 0);
		result->type = XPATH_VAL_NODESET;
	}
	else
	{
		elog(ERROR, "unknown operator to evaluate: %u", operator->id);
		result->v.boolean = false;
	}
}

void
initScanForTextNodes(XMLScan xscan, XMLCompNodeHdr root)
{
	XPath		xp = (XPath) palloc(sizeof(XPathData) + sizeof(XPathElementData));
	XPathElement xpEl = (XPathElement) ((char *) xp + sizeof(XPathData));

	xpEl->descendant = true;
	xpEl->hasPredicate = false;
	xp->depth = 1;
	xp->targNdKind = XMLNODE_TEXT;
	xp->allAttributes = false;
	xp->elements[0] = sizeof(XPathData);

	initXMLScan(xscan, NULL, xp, NULL, root, xscan->document, false);
}

void
finalizeScanForTextNodes(XMLScan xscan)
{
	pfree(xscan->xpath);
	finalizeXMLScan(xscan);
}

/*
 * Sub-scans are used to search for descendants recursively.
 * It takes the current node as the root, so it in fact scans children of the current node.
 * If any of those children has children, new sub-scan is initiated, etc.
 */
static bool
considerSubScan(XPathElement xpEl, XMLNodeHdr node, XMLScan xscan, bool subScanJustDone)
{
	if (xpEl->descendant && node->kind == XMLNODE_ELEMENT && !subScanJustDone)
	{
		XMLCompNodeHdr el = (XMLCompNodeHdr) node;

		if (el->children > 0)
		{
			xscan->subScan = (XMLScan) palloc(sizeof(XMLScanData));
			initXMLScan(xscan->subScan, xscan, xscan->xpath, xscan->xpathHeader, el, xscan->document,
						xscan->ignoreList != NULL);
			xscan->subScan->xpathRoot = xscan->xpathRoot + xscan->depth;
			return true;
		}
	}
	return false;
}

static void
addNodeToIgnoreList(XMLNodeHdr node, XMLScan scan)
{
	if (scan->ignoreList != NULL)
	{
		xmlnodePushSingleNode(scan->ignoreList, XNODE_OFFSET(node, scan->document));
	}
}

/*
 * Replace attribute names in the expression with actual values (i.e. with
 * values that the current element contains).
 *
 * 'exprState' contains the expath expression.
 * 'exprState' also receives array of attribute values referenced by the expression variables.
 * By return time the referencing variables (expression operands) have obtained
 * the appropriate subscripts for this array.
 */
static void
substituteAttributes(XPathExprState exprState, XMLCompNodeHdr element)
{
	unsigned short attrNr = 0;
	unsigned short attrCount = 0;
	XMLNodeHdr *attributes = NULL;
	unsigned int attrId = 0;
	unsigned int attrsArrayId = 0;
	XMLNodeIteratorData iterator;
	XMLNodeHdr	child;

	initXMLNodeIterator(&iterator, element, true);

	while ((child = getNextXMLNodeChild(&iterator)) != NULL)
	{
		if (child->kind == XMLNODE_ATTRIBUTE)
		{
			char	   *attrName = XNODE_CONTENT(child);
			unsigned short i;
			unsigned short matches = 0;
			XPathOffset *varOffPtr = (XPathOffset *) ((char *) exprState->expr +
												sizeof(XPathExpressionData));

			/*
			 * Check all variables and find those referencing 'child'
			 * attribute.
			 */
			for (i = 0; i < exprState->expr->variables; i++)
			{
				XPathExprOperand opnd = (XPathExprOperand) ((char *) exprState->expr +
															*varOffPtr);

				if (opnd->substituted)
				{
					varOffPtr++;
					continue;
				}

				if (opnd->common.type == XPATH_OPERAND_ATTRIBUTE)
				{
					XPathNodeSet nodeSet = &opnd->value.v.nodeSet;
					unsigned int nodeNr = exprState->count[XPATH_VAR_NODE_SINGLE] + attrNr;
					char	   *opndValue = XPATH_STRING_LITERAL(&opnd->value);

					if (*opndValue == XNODE_CHAR_ASTERISK)
					{
						if (attributes == NULL)
						{
							unsigned short j = 0;
							unsigned int size = element->children * sizeof(XMLNodeHdr);
							XMLNodeIteratorData iterAttr;
							XMLNodeHdr	attrNode;

							initXMLNodeIterator(&iterAttr, element, true);
							attributes = (XMLNodeHdr *) palloc(size);
							MemSet(attributes, 0, size);

							while ((attrNode = getNextXMLNodeChild(&iterAttr)) != NULL)
							{
								if (attrNode->kind != XMLNODE_ATTRIBUTE)
								{
									break;
								}
								attributes[j++] = attrNode;
								attrCount++;
							}
							Assert(j > 0);

							if (attrCount == 1)
							{
								attrId = getXPathOperandId(exprState, attributes[0], XPATH_VAR_NODE_SINGLE);

								/*
								 * In this case only the single attribute has
								 * been added to the cache, so the
								 * 'attributes' array is no longer needed.
								 */
								pfree(attributes);
							}
							else
							{
								attrsArrayId = getXPathOperandId(exprState, attributes, XPATH_VAR_NODE_ARRAY);
							}
						}
						if (attrCount == 1)
						{
							nodeSet->nodes.nodeId = attrId;
						}
						else
						{
							nodeSet->nodes.arrayId = attrsArrayId;
						}
						nodeSet->count = attrCount;
						nodeSet->isDocument = false;
						opnd->value.isNull = false;
						opnd->substituted = true;
						opnd->value.type = XPATH_VAL_NODESET;
					}
					else if (strcmp(attrName, opndValue) == 0)
					{
						/*
						 * Save node pointer into the variable cache and
						 * assign its id to the operand.
						 *
						 * getXPathOperandId() can't be used here because any
						 * attribute may be substituted for multiple
						 * variables. It wouldn't bee too efficient to assign
						 * a separate id (and storage) to such attribute
						 * multiple times (i.e. once for each variable that
						 * references it).
						 */
						matches++;
						Assert(nodeNr <= exprState->countMax[XPATH_VAR_NODE_SINGLE]);
						if (nodeNr == exprState->countMax[XPATH_VAR_NODE_SINGLE])
						{
							allocXPathExpressionVarCache(exprState, XPATH_VAR_NODE_SINGLE, XPATH_VAR_CACHE_DEF_SIZE, false);
						}

						/*
						 * The attribute pointer is only added to cache once.
						 * If it occurs in the expression multiple times, all
						 * occurrences share the same instance in the cache.
						 */
						if (matches == 1)
						{
							exprState->nodes[nodeNr] = child;
						}

						nodeSet->nodes.nodeId = nodeNr;
						nodeSet->count = 1;
						nodeSet->isDocument = false;

						opnd->value.isNull = false;
						opnd->substituted = true;
						opnd->value.type = XPATH_VAL_NODESET;
					}
				}
				varOffPtr++;
			}

			/*
			 * If at least one variable references the current attribute, the
			 * potential next attribute needs a new unique number.
			 */
			if (matches > 0)
			{
				attrNr++;
			}
		}
		else
		{
			/*
			 * Attributes are first of the children. Thus if the current node
			 * is not an attribute, we're done.
			 */
			break;
		}
	}
	exprState->count[XPATH_VAR_NODE_SINGLE] += attrNr;
}

/*
 * All sub-paths in 'expression' are replaced with the matching node-sets.
 *
 * expression - XPath expression containing the paths we're evaluating
 * (replacing with node-sets).
 *
 * element - context node (XML element that we'll test using 'expression' when the substitution is complete)
 */
static void
substitutePaths(XPathExpression expression, XPathExprState exprState, XMLCompNodeHdr element, xmldoc document,
				XPathHeader xpHdr)
{
	unsigned short i,
				processed;
	XPathOffset *varOffPtr = (XPathOffset *) ((char *) expression + sizeof(XPathExpressionData));

	processed = 0;
	for (i = 0; i < expression->variables; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) ((char *) expression + *varOffPtr);

		if (opnd->common.type == XPATH_OPERAND_PATH)
		{
			XMLScanData xscanSub;
			XPath		subPath = XPATH_HDR_GET_PATH(xpHdr, opnd->value.v.path);

			XMLNodeHdr	matching;
			unsigned short count = 0;
			unsigned short arrSize = 0;

			if (!subPath->relative && subPath->depth == 0)
			{
				opnd->value.isNull = false;
				opnd->value.v.nodeSet.isDocument = true;
				opnd->value.v.nodeSet.nodes.nodeId =
					getXPathOperandId(exprState, XNODE_ROOT(document),
									  XPATH_VAR_NODE_SINGLE);
				opnd->value.v.nodeSet.count = 1;
			}
			else if (!subPath->relative && subPath->depth == 1 &&
					 subPath->descendants == 0 && subPath->targNdKind == XMLNODE_ATTRIBUTE)
			{
				/*
				 * Paths like '/@attr' or '/@*' never point to a valid node.
				 * Further evaluation makes no sense in such cases.
				 */
				opnd->value.isNull = true;
				opnd->value.v.nodeSet.count = 0;
				opnd->value.v.nodeSet.isDocument = false;
			}
			else
			{
				XMLCompNodeHdr parent = (subPath->relative) ? element : (XMLCompNodeHdr) XNODE_ROOT(document);

				initXMLScan(&xscanSub, NULL, subPath, xpHdr, parent, document, subPath->descendants > 0);
				while ((matching = getNextXMLNode(&xscanSub)) != NULL)
				{
					XMLNodeHdr *array = NULL;

					if (count == 0)
					{
						opnd->value.v.nodeSet.nodes.nodeId = getXPathOperandId(exprState, matching,
													  XPATH_VAR_NODE_SINGLE);
					}
					else if (count == 1)
					{
						/*
						 * Adding 2nd node, so the array size must be at least
						 * 2.
						 */
						arrSize = (element->children > count) ? element->children : 2;
						array = (XMLNodeHdr *) palloc(sizeof(XMLNodeHdr) * arrSize);
						array[0] = getXPathOperandValue(exprState, opnd->value.v.nodeSet.nodes.nodeId,
													  XPATH_VAR_NODE_SINGLE);
						array[1] = matching;
						opnd->value.v.nodeSet.nodes.arrayId = getXPathOperandId(exprState, array,
													   XPATH_VAR_NODE_ARRAY);
					}
					else
					{
						unsigned short arrayId = opnd->value.v.nodeSet.nodes.arrayId;
						XMLNodeHdr *array = getXPathOperandValue(exprState, arrayId, XPATH_VAR_NODE_ARRAY);

						if (count >= arrSize)
						{
							/*
							 * Estimate like this might be o.k. whether the
							 * node has few or many children
							 */
							arrSize = count + (count >> 1) + 1;

							array = (XMLNodeHdr *) repalloc(array, sizeof(XMLNodeHdr) * arrSize);
							exprState->nodeSets[arrayId] = array;
						}
						array[count] = matching;
					}
					count++;
				}
				opnd->value.isNull = (count == 0);
				opnd->value.v.nodeSet.count = count;
				opnd->value.v.nodeSet.isDocument = false;
				finalizeXMLScan(&xscanSub);
			}
			opnd->substituted = true;
			opnd->value.type = XPATH_VAL_NODESET;
			processed++;
			if (processed == expression->npaths)
			{
				break;
			}
		}
		varOffPtr++;
	}
}

/*
 * Substitute functions having no arguments. Functions that do have arguments
 * are considered a special type of sub-expression.
 */
static void
substituteFunctions(XPathExpression expression, XPathExprState exprState, XMLScan xscan)
{
	unsigned short i,
				processed;
	XPathOffset *varOffPtr = (XPathOffset *) ((char *) expression + sizeof(XPathExpressionData));

	processed = 0;
	for (i = 0; i < expression->variables; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) ((char *) expression + *varOffPtr);

		if (opnd->common.type == XPATH_OPERAND_FUNC_NOARG)
		{
			XPathFunctionId funcId = opnd->value.v.funcId;
			XPathFunction func;
			XpathFuncImplNoArgs funcImpl;

			if (funcId >= XPATH_FUNCTIONS)
			{
				elog(ERROR, "unrecognized function id: %u", funcId);
			}

			func = &xpathFunctions[funcId];
			Assert(func->nargs == 0);

			funcImpl = func->impl.noargs;
			/* Assume not null result. The implementation may change it. */
			opnd->value.isNull = false;

			/* Call the function. */
			funcImpl(xscan, exprState, &opnd->value);

			opnd->value.type = func->resType;
			opnd->substituted = true;

			processed++;
			if (processed == expression->nfuncs)
			{
				break;
			}
		}
		varOffPtr++;
	}
}

/*
 * Both operands are cast to number.
 * If either cast fails, the operator evaluates to true for != operator and to false in other cases.
 */
static void
compareNumValues(XPathExprState exprState, XPathExprOperandValue valueLeft, XPathExprOperandValue valueRight,
				 XPathExprOperator operator, XPathExprOperandValue result)
{
	double		numLeft,
				numRight;

	result->type = XPATH_VAL_BOOLEAN;
	result->isNull = false;

	if (valueLeft->isNull || valueRight->isNull)
	{
		result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_NEQ);
		return;
	}

	if (!xmlNumberToDouble(exprState, valueLeft, &numLeft))
	{
		result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_NEQ);
		return;
	}
	if (!xmlNumberToDouble(exprState, valueRight, &numRight))
	{
		result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_NEQ);
		return;
	}

	compareNumbers(numLeft, numRight, operator, result);
}

static bool
xmlNumberToDouble(XPathExprState exprState, XPathExprOperandValue numOperand, double *simple)
{
	if (numOperand->type == XPATH_VAL_NUMBER)
	{
		*simple = numOperand->v.num;
		if (numOperand->negative)
		{
			*simple *= -1.0f;
		}
	}
	else
	{
		XPathExprOperandValueData opValueNum;

		castXPathExprOperandToNum(exprState, numOperand, &opValueNum, false);
		if (opValueNum.isNull)
		{
			return false;
		}
		*simple = opValueNum.v.num;
	}
	return true;
}

static void
compareNumbers(double numLeft, double numRight, XPathExprOperator operator,
			   XPathExprOperandValue result)
{

	result->type = XPATH_VAL_BOOLEAN;
	result->isNull = false;
	switch (operator->id)
	{
		case XPATH_EXPR_OPERATOR_EQ:
			result->v.boolean = (numLeft == numRight);
			return;

		case XPATH_EXPR_OPERATOR_NEQ:
			result->v.boolean = (numLeft != numRight);
			return;

		case XPATH_EXPR_OPERATOR_LT:
			result->v.boolean = (numLeft < numRight);
			break;

		case XPATH_EXPR_OPERATOR_LTE:
			result->v.boolean = (numLeft <= numRight);
			break;

		case XPATH_EXPR_OPERATOR_GT:
			result->v.boolean = (numLeft > numRight);
			break;

		case XPATH_EXPR_OPERATOR_GTE:
			result->v.boolean = (numLeft >= numRight);
			break;

		default:
			elog(ERROR, "unexpected operator %u", operator->id);
			break;
	}
}

/*
 * Returns true if the node sets are equal according to
 * http://www.w3.org/TR/1999/REC-xpath-19991116/#booleans
 */
static bool
compareNodeSets(XPathExprState exprState, XPathNodeSet ns1, XPathNodeSet ns2, XPathExprOperator operator)
{
	XMLNodeHdr	node1,
				node2;
	XMLNodeHdr *arrayOuter,
			   *arrayInner;
	XPathNodeSet nsOuter = ns1;
	XPathNodeSet nsInner = ns2;
	unsigned short i;

	if (ns1->count > 1)
	{
		arrayOuter = (XMLNodeHdr *) getXPathOperandValue(exprState, ns1->nodes.arrayId, XPATH_VAR_NODE_ARRAY);
	}
	else
	{
		node1 = (XMLNodeHdr) getXPathOperandValue(exprState, ns1->nodes.nodeId, XPATH_VAR_NODE_SINGLE);
		arrayOuter = &node1;
	}

	if (ns2->count > 1)
	{
		arrayInner = (XMLNodeHdr *) getXPathOperandValue(exprState, ns2->nodes.arrayId, XPATH_VAR_NODE_ARRAY);
	}
	else
	{
		node2 = (XMLNodeHdr) getXPathOperandValue(exprState, ns2->nodes.nodeId, XPATH_VAR_NODE_SINGLE);
		arrayInner = &node2;
	}

	for (i = 0; i < nsOuter->count; i++)
	{
		XMLNodeHdr	nodeOuter = arrayOuter[i];

		unsigned short j;
		bool		match = false;

		for (j = 0; j < nsInner->count; j++)
		{
			XMLNodeHdr	nodeInner = arrayInner[j];

			/*
			 * In order to reduce the number of combinations there's a rule
			 * that for 'element - non-element' comparisons the 'non-element
			 * node' will always be on the same side. The outer loop has been
			 * chosen.
			 */
			if (nodeOuter->kind == XMLNODE_ELEMENT && nodeInner->kind != XMLNODE_ELEMENT)
			{
				XMLNodeHdr	nodeTmp;

				nodeTmp = nodeOuter;
				nodeOuter = nodeInner;
				nodeInner = nodeTmp;
			}
			if (nodeOuter->kind == XMLNODE_ELEMENT)
			{
				/* Both nodes are elements */
				bool		matchResult = false;

				match = compareElements((XMLCompNodeHdr) nodeOuter, (XMLCompNodeHdr) nodeInner);
				matchResult = (operator->id == XPATH_EXPR_OPERATOR_EQ) ? match : !match;
				if (matchResult)
				{
					return true;
				}
			}
			else
			{
				/*
				 * The outer is non-element, the inner is any node kind.
				 */
				char	   *nodeStr = getNonElementNodeStr(nodeOuter);
				XPathExprOperandValueData valueOuter;

				if (nodeStr == NULL)
				{
					return false;
				}
				valueOuter.type = XPATH_VAL_STRING;
				valueOuter.isNull = false;
				valueOuter.castToNumber = false;
				valueOuter.v.stringId = getXPathOperandId(exprState, nodeStr, XPATH_VAR_STRING);
				if (compareValueToNode(exprState, &valueOuter, nodeInner, operator))
				{
					return true;
				}
			}
		}
	}
	return false;
}

static bool
compareElements(XMLCompNodeHdr elLeft, XMLCompNodeHdr elRight)
{
	XMLScanData scanLeft,
				scanRight;

	/* maximum text position that we have for both elements */
	unsigned int charsToCompare;
	XMLNodeHdr	nodeLeft,
				nodeRight;
	char	   *textLeft,
			   *textRight;
	unsigned int lengthLeft,
				lengthRight;
	bool		done = false;
	bool		match = false;

	initScanForTextNodes(&scanLeft, elLeft);
	initScanForTextNodes(&scanRight, elRight);

	nodeLeft = getNextXMLNode(&scanLeft);
	nodeRight = getNextXMLNode(&scanRight);
	done = false;

	if (nodeLeft == NULL && nodeRight == NULL)
	{
		match = true;
		done = true;
	}
	else if (nodeLeft == NULL || nodeRight == NULL)
	{
		match = false;
		done = true;
	}
	if (done)
	{
		finalizeScanForTextNodes(&scanLeft);
		finalizeScanForTextNodes(&scanRight);
		return match;
	}

	/*
	 * Both left and right nodes are not null at this moment.
	 */
	textLeft = XNODE_CONTENT(nodeLeft);
	lengthLeft = strlen(textLeft);
	textRight = XNODE_CONTENT(nodeRight);
	lengthRight = strlen(textRight);
	charsToCompare = (lengthLeft < lengthRight) ? lengthLeft : lengthRight;

	while (!done)
	{
		if (strncmp(textLeft, textRight, charsToCompare) != 0)
		{
			break;
		}
		if (lengthLeft < lengthRight)
		{
			nodeLeft = getNextXMLNode(&scanLeft);
			if (nodeLeft == NULL)
			{
				break;
			}
			textLeft = XNODE_CONTENT(nodeLeft);
			textRight += charsToCompare;
		}
		else if (lengthLeft > lengthRight)
		{
			nodeRight = getNextXMLNode(&scanRight);
			if (nodeRight == NULL)
			{
				break;
			}
			textLeft += charsToCompare;
			textRight = XNODE_CONTENT(nodeRight);
		}
		else
		{
			/* shift the 'cursor' on both left and right side */
			nodeLeft = getNextXMLNode(&scanLeft);
			nodeRight = getNextXMLNode(&scanRight);
			if (nodeLeft == NULL && nodeRight == NULL)
			{
				match = true;
				break;
			}
			else if (nodeLeft == NULL || nodeRight == NULL)
			{
				break;
			}
			textLeft = XNODE_CONTENT(nodeLeft);
			textRight = XNODE_CONTENT(nodeRight);
		}
		lengthLeft = strlen(textLeft);
		lengthRight = strlen(textRight);
		charsToCompare = (lengthLeft < lengthRight) ? lengthLeft : lengthRight;
	}

	finalizeScanForTextNodes(&scanLeft);
	finalizeScanForTextNodes(&scanRight);
	return match;
}

/*
 * The operator direction (whether '<' or '>') assumes 'value' is on the left
 * side in the expression and node on the right. If it's the other way round,
 * the caller must switch the direction.
 */
static bool
compareValueToNode(XPathExprState exprState, XPathExprOperandValue value, XMLNodeHdr node,
				   XPathExprOperator operator)
{
	bool		match = false;

	if (!(value->type == XPATH_VAL_STRING || value->type == XPATH_VAL_NUMBER))
	{
		elog(ERROR, "unable to compare operand value type %u to a node", value->type);
	}
	if (node->kind == XMLNODE_ELEMENT)
	{

		XMLNodeHdr	textNode;
		unsigned int strLen;
		unsigned int nodeCntLen = 0;

		if (value->type == XPATH_VAL_STRING)
		{
			char	   *cStr = (char *) getXPathOperandValue(exprState, value->v.stringId, XPATH_VAR_STRING);
			XMLScanData textScan;

			strLen = strlen(cStr);
			initScanForTextNodes(&textScan, (XMLCompNodeHdr) node);

			while ((textNode = getNextXMLNode(&textScan)) != NULL)
			{
				char	   *cntPart = XNODE_CONTENT(textNode);
				unsigned int cntPartLen = strlen(cntPart);

				nodeCntLen += cntPartLen;
				if (nodeCntLen > strLen)
				{
					break;
				}
				if (strncmp(cStr, cntPart, cntPartLen) == 0)
				{
					/*
					 * The text node content matches the corresponding part of
					 * 'str', so prepare to compare the next text node.
					 */
					match = true;
					cStr += cntPartLen;
				}
				else
				{
					match = false;
					break;
				}
			}
			finalizeScanForTextNodes(&textScan);
		}
		else
		{
			/*
			 * Compare number to element node. Concatenation of the text
			 * elements can't be avoided now.
			 */
			XPathExprOperandValueData result;
			char	   *nodeStr;

			result.type = XPATH_VAL_BOOLEAN;
			result.v.boolean = false;

			nodeStr = getElementNodeStr((XMLCompNodeHdr) node);

			if (strlen(nodeStr) > 0)
			{
				compareNumToStr(value->v.num, nodeStr, operator, &result);
			}
			pfree(nodeStr);
			return result.v.boolean;
		}

		if (value->type == XPATH_VAL_STRING)
		{
			if (match)
			{
				if (nodeCntLen != strLen)
				{
					match = false;
				}
			}
			else
			{
				/*
				 * This condition represents both strings being empty (But
				 * node set non-empty: if the node set is empty, this function
				 * shouldn't be called at all).
				 */
				if (nodeCntLen == 0 && strLen == 0)
				{
					match = true;
				}
			}
		}
	}
	else
	{
		char	   *nodeStr = getNonElementNodeStr(node);
		XPathExprOperandValueData result;

		result.type = XPATH_VAL_BOOLEAN;
		result.v.boolean = false;

		if (nodeStr == NULL)
		{
			return false;
		}
		if (value->type == XPATH_VAL_STRING)
		{
			match = strcmp((char *) getXPathOperandValue(exprState, value->v.stringId, XPATH_VAR_STRING),
						   nodeStr) == 0;
		}
		else if (value->type == XPATH_VAL_NUMBER)
		{
			compareNumToStr(value->v.num, nodeStr, operator, &result);
			return result.v.boolean;
		}
		else
		{
			elog(ERROR, "unrecognized type of value to be compared to a node: %u", value->type);
		}
	}

	/*
	 * For numbers this check is performed by compareNumToStr() and result
	 * returned immediately. Extra operators '<', '>', '<=' and '>=' exist for
	 * numbers, whereas only '=' and '!=' can be applied to strings in this
	 * function.
	 */
	Assert(value->type == XPATH_VAL_STRING);
	return (operator->id == XPATH_EXPR_OPERATOR_EQ) ? match : !match;
}

static void
compareNumToStr(double num, char *numStr, XPathExprOperator operator, XPathExprOperandValue result)
{
	char	   *end;
	double		numValue = strtod(numStr, &end);

	result->type = XPATH_VAL_BOOLEAN;
	result->v.boolean = false;

	if (end > numStr)
	{
		/* only whitespaces are accepted after the number */
		while (*end != '\0')
		{
			if (!XNODE_WHITESPACE(end))
			{
				break;
			}
			end++;
		}

		if (*end == '\0')
		{
			compareNumbers(num, numValue, operator, result);
		}
	}
	else
	{
		/* 'numStr' does not represent valid number. */
		result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_NEQ);
	}
}

/*
 * The operator direction (whether '<' or '>') assumes 'value' is on the left
 * side in the expression and node-set on the right. If it's the other way
 * round, the caller must switch the direction.
 */
static bool
compareValueToNodeSet(XPathExprState exprState, XPathExprOperandValue value, XPathNodeSet ns,
					  XPathExprOperator operator)
{
	XMLNodeHdr *array;
	unsigned short i;

	if (ns->count > 1)
	{
		array = (XMLNodeHdr *) getXPathOperandValue(exprState, ns->nodes.arrayId, XPATH_VAR_NODE_ARRAY);
	}
	else
	{
		XMLNodeHdr	node = (XMLNodeHdr) getXPathOperandValue(exprState, ns->nodes.nodeId, XPATH_VAR_NODE_SINGLE);

		array = &node;
	}

	if (!(operator->id == XPATH_EXPR_OPERATOR_EQ || operator->id == XPATH_EXPR_OPERATOR_NEQ ||
		  operator->id == XPATH_EXPR_OPERATOR_LT || operator->id == XPATH_EXPR_OPERATOR_GT ||
		  operator->id == XPATH_EXPR_OPERATOR_LTE || operator->id == XPATH_EXPR_OPERATOR_GTE))
	{
		elog(ERROR, "unexpected operator: %u", operator->id);
	}
	for (i = 0; i < ns->count; i++)
	{
		XMLNodeHdr	node = array[i];

		if (compareValueToNode(exprState, value, node, operator))
		{
			return true;
		}
	}
	return false;
}

static bool
isOnIgnoreList(XMLNodeHdr node, XMLScan scan)
{
	XMLNodeOffset nodeOff = XNODE_OFFSET(node, scan->document);

	if (scan->ignoreList != NULL)
	{
		XMLNodeContainer cont = scan->ignoreList;
		unsigned short i;

		for (i = 0; i < cont->position; i++)
		{
			XNodeListItem *item = cont->content + i;

			if (!item->valid)
			{
				continue;
			}

			if (item->kind == XNODE_LIST_ITEM_SINGLE_OFF)
			{
				if (item->value.singleOff == nodeOff)
				{
					return true;
				}
			}
			else
			{
				if (item->value.range.lower <= nodeOff && item->value.range.upper >= nodeOff)
				{
					return true;
				}
			}
		}
		return false;
	}
	else
	{
		return false;
	}
}

/*
 * Generate node set containing nodes from both 'setLeft' and 'setRight', omitting duplicate nodes.
 *
 * The simplest approach is to sort the nodes by position in the document, then iterate through the list
 * and ignore repeated values.
 *
 * Given the binary format (children at lower positions than parents) this may produce different order from
 * what user may expect. For example
 * xml.path('/root/a|/root/text()|/root/a/text()', '<root><a>b</a>c</root>')
 * gives
 * b<a>b</a>c
 *
 * Until it's clear if this is in contradiction with XPath standard, let's leave it this way.
 */
static void
getUnion(XPathExprState exprState, XPathNodeSet setLeft, XPathNodeSet setRight, XPathNodeSet setResult)
{
	unsigned int countMax = setLeft->count + setRight->count;
	unsigned int resSize = countMax * sizeof(XMLNodeHdr *);
	XMLNodeHdr *nodesAll = (XMLNodeHdr *) palloc(resSize);
	XMLNodeHdr *result = (XMLNodeHdr *) palloc(resSize);
	unsigned int pos = 0;
	unsigned int i,
				j;

	copyNodeSet(exprState, setLeft, nodesAll, &pos);
	copyNodeSet(exprState, setRight, nodesAll, &pos);

	Assert(pos == countMax);
	qsort(nodesAll, countMax, sizeof(XMLNodeHdr), nodePtrComparator);

	/* Copy nodes to the result array, but skip the repeating values. */
	MemSet(result, 0, resSize);
	j = 0;
	for (i = 0; i < countMax; i++)
	{
		if (i == 0 || (i > 0 && result[j - 1] != nodesAll[i]))
		{
			result[j++] = nodesAll[i];
		}
	}
	setResult->count = j;
	setResult->isDocument = false;

	/*
	 * Don't add anything to variable cache if the union is empty. The caller
	 * will handle such a case by setting the operand's 'isNull' to true.
	 */
	if (j == 1)
	{
		setResult->nodes.nodeId = getXPathOperandId(exprState, result[0], XPATH_VAR_NODE_SINGLE);
		pfree(result);
	}
	else if (j > 1)
	{
		setResult->nodes.arrayId = getXPathOperandId(exprState, result, XPATH_VAR_NODE_ARRAY);
	}
	pfree(nodesAll);
}

/*
 * Copy node(s) from 'nodeSet' to 'output', starting at '*position'.
 * Sufficient space must be allocated in 'output'.
 * '*position' gets increased by the number of nodes actually added.
 */
static void
copyNodeSet(XPathExprState exprState, XPathNodeSet nodeSet, XMLNodeHdr *output, unsigned int *position)
{
	XMLNodeHdr *start = output + *position;

	if (nodeSet->count == 0)
	{
		return;
	}

	if (nodeSet->count == 1)
	{
		*start = (XMLNodeHdr) getXPathOperandValue(exprState, nodeSet->nodes.nodeId, XPATH_VAR_NODE_SINGLE);
		(*position)++;
	}
	else
	{
		XMLNodeHdr *nodes = (XMLNodeHdr *) getXPathOperandValue(exprState, nodeSet->nodes.arrayId, XPATH_VAR_NODE_ARRAY);

		memcpy(start, nodes, nodeSet->count * sizeof(XMLNodeHdr *));
		*position += nodeSet->count;
	}
}

static int
nodePtrComparator(const void *arg1, const void *arg2)
{
	XMLNodeHdr *node1 = (XMLNodeHdr *) arg1;
	XMLNodeHdr *node2 = (XMLNodeHdr *) arg2;

	if (*node1 < *node2)
	{
		return -1;
	}
	else if (*node1 > *node2)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/*
 * If value->negative is 'true', set it to 'false' and ensure the sign is
 * pushed to the actual value. In some cases the value must first be cast
 * to number.
 */
static void
flipOperandValue(XPathExprOperandValue value, XPathExprState exprState)
{
	if (value->type != XPATH_VAL_NUMBER)
	{
		XPathExprOperandValueData valTmp;

		castXPathExprOperandToNum(exprState, value, &valTmp, false);
		memcpy(value, &valTmp, sizeof(XPathExprOperandValueData));
		/* The cast above must have processed the 'negative' flag. */
		Assert(!value->negative);
	}
	else
	{
		value->v.num *= -1.0f;
		value->negative = false;
	}
}
