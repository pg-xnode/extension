#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_update.h"

/*
 * TODO
 * move to xpath.c
 * as well as getNextXMLNode()
 */
static unsigned int evaluateXPathOperand(XPathExprOperand operand, XMLScanOneLevel scan, XMLCompNodeHdr element,
				unsigned short recursionLevel, XPathExprOperandValue result);
static void evaluateXPathFunction(XPathExpression funcExpr, XMLScanOneLevel scan, XMLCompNodeHdr element,
				unsigned short recursionLevel, XPathExprOperandValue result);
static void prepareLiteral(XPathExprOperand operand);
static void freeNodeSets(XPathExpression expr);
static void evaluateBinaryOperator(XPathExprOperandValue valueLeft, XPathExprOperandValue valueRight,
					   XPathExprOperator operator, XPathExprOperandValue result, XMLCompNodeHdr element);
static void compareNumValues(XPathExprOperandValue valueLeft, XPathExprOperandValue valueRight,
				 XPathExprOperator operator, XPathExprOperandValue result);
static void compareNumbers(double numLeft, double numRight, XPathExprOperator operator,
			   XPathExprOperandValue result);

static double getNumValue(char *str);
static bool compareNodeSets(XPathNodeSet ns1, XPathNodeSet ns2, XPathExprOperator operator);
static bool compareElements(XMLCompNodeHdr elLeft, XMLCompNodeHdr elRight);
static void initScanForTextNodes(XMLScan xscan, XMLCompNodeHdr root);
static void finalizeScanForTextNodes(XMLScan xscan);
static bool compareValueToNode(XPathExprOperandValue value, XMLNodeHdr node, XPathExprOperator operator);
static void compareNumToStr(double num, char *numStr, XPathExprOperator operator, XPathExprOperandValue result);
static char *getElementNodeStr(XMLCompNodeHdr element);
static char *getNonElementNodeStr(XMLNodeHdr node);
static bool compareValueToNodeSet(XPathExprOperandValue value, XPathNodeSet ns, XPathExprOperator operator);
static void substituteAttributes(XPathExpression expr, XMLCompNodeHdr element);
static void substituteSubpaths(XPathExpression expression, XMLCompNodeHdr element, xmldoc document,
				   XPathHeader xpHdr);
static void substituteFunctions(XPathExpression expression, XMLScan xscan);
static bool isNodeUnique(XMLNodeHdr node, XMLScan scan);
static void rememberResult(XMLNodeHdr node, XMLScan scan);
static void dumpNodeDebug(StringInfo output, char *data, XMLNodeOffset rootOff, unsigned short level);
static void dumpXScanDebug(StringInfo output, XMLScan scan, char *docData, XMLNodeOffset docRootOff);
static bool considerSubScan(XPathElement xpEl, XMLNodeHdr node, XMLScan xscan);

void
xmlnodeContainerInit(XMLNodeContainer cont)
{
	cont->size = XNODE_PARSER_STACK_CHUNK;
	cont->items = (XMLNodeOffset *) palloc(cont->size * sizeof(XMLNodeOffset));
	cont->position = 0;
}

void
xmlnodeContainerFree(XMLNodeContainer cont)
{
	if (cont->items != NULL)
	{
		pfree(cont->items);
	}
}

void
xmlnodePush(XMLNodeContainer cont, XMLNodeOffset value)
{
	unsigned int pos = cont->position;

	cont->items[pos] = value;
	cont->position++;
	if (cont->position == cont->size)
	{
		cont->size += XNODE_PARSER_STACK_CHUNK;
		cont->items = (XMLNodeOffset *) repalloc(cont->items, cont->size
												 * sizeof(XMLNodeOffset));
		elog(DEBUG1, "XTreeParserStack reallocated. New size: %u.", cont->size);
	}
	return;
}

XMLNodeOffset
xmlnodePop(XMLNodeContainer cont)
{

	if (cont->position == 0)
	{
		elog(ERROR, "Stack is empty.");
	}
	cont->position--;
	return cont->items[cont->position];
}

/*
 * Return binary size of a node.
 *
 * 'node' the node to be examined 'subtree' consider subtree (if exists)?
 */
unsigned int
getXMLNodeSize(XMLNodeHdr node, bool subtree)
{
	XMLNodeHdr	childNode;
	unsigned int result = 0;
	char	   *childOffPtr,
			   *lastChildOffPtr;
	char	   *content;
	unsigned int references;
	unsigned int attNameLen;

	switch (node->kind)
	{
		case XMLNODE_DOC:
		case XMLNODE_ELEMENT:
		case XMLNODE_DOC_FRAGMENT:
			result = sizeof(XMLCompNodeHdrData);
			childOffPtr = XNODE_FIRST_REF((XMLCompNodeHdr) node);
			lastChildOffPtr = XNODE_LAST_REF((XMLCompNodeHdr) node);
			references = 0;
			while (childOffPtr <= lastChildOffPtr)
			{
				XMLNodeOffset childOff = readXMLNodeOffset(&childOffPtr,
						  XNODE_GET_REF_BWIDTH((XMLCompNodeHdr) node), true);

				childNode = (XMLNodeHdr) ((char *) node - childOff);
				if (subtree)
				{
					result += getXMLNodeSize(childNode, true);
				}
				references++;
			}
			result += references * XNODE_GET_REF_BWIDTH((XMLCompNodeHdr) node);
			if (node->kind == XMLNODE_ELEMENT)
			{
				content = XNODE_ELEMENT_NAME((XMLCompNodeHdr) node);
				result += strlen(content) + 1;
			}
			else if (node->kind == XMLNODE_DOC)
			{
				if (node->flags & XNODE_DOC_XMLDECL)
				{
					result += sizeof(XMLDeclData);
				}
			}
			return result;

		case XMLNODE_DTD:
		case XMLNODE_COMMENT:
		case XMLNODE_CDATA:
		case XMLNODE_PI:
		case XMLNODE_TEXT:
			result = sizeof(XMLNodeHdrData);
			content = (char *) node + result;
			result += strlen(content) + 1;
			return result;

		case XMLNODE_ATTRIBUTE:
			result = sizeof(XMLNodeHdrData);
			content = (char *) node + result;
			attNameLen = strlen(content) + 1;
			result += attNameLen;
			content += attNameLen;
			result += strlen(content) + 1;
			return result;

		default:
			elog(ERROR, "unrecognized node kind to determine size of: %u", node->kind);
			break;
	}
	return 0;
}

char *
getXMLNodeKindStr(XMLNodeKind k)
{
	StringInfoData result;

	result.maxlen = 32;
	result.data = (char *) palloc(result.maxlen);
	resetStringInfo(&result);

	switch (k)
	{
		case XMLNODE_DOC:
			appendStringInfoString(&result, "XML document");
			break;
		case XMLNODE_DTD:
			appendStringInfoString(&result, "DTD");
			break;
		case XMLNODE_ELEMENT:
			appendStringInfoString(&result, "XML element");
			break;
		case XMLNODE_ATTRIBUTE:
			appendStringInfoString(&result, "XML element attribute");
			break;
		case XMLNODE_COMMENT:
			appendStringInfoString(&result, "XML comment");
			break;
		case XMLNODE_CDATA:
			appendStringInfoString(&result, "CDATA section");
			break;
		case XMLNODE_PI:
			appendStringInfoString(&result, "processing instruction");
			break;
		case XMLNODE_TEXT:
			appendStringInfoString(&result, "text node");
			break;
		case XMLNODE_DOC_FRAGMENT:
			appendStringInfoString(&result, "document fragment");
			break;
		default:
			elog(ERROR, "unknown node kind: %u", k);
			return NULL;
	}
	return result.data;
}

/*
 * Returns a copy of a node and it's children if there are any.
 *
 * 'node' - the node to copy 'target' - if not NULL, it's assumed that
 * sufficient space is available and the copy is written there. If NULL, the
 * appropriate chunk of memory is palloc'd. 'xmlnode' - if 'true', valid
 * value of 'xmlnode' (varlena) type is returned. Otherwise we return 'raw
 * data'. 'root' - at which position of the returned subtree its root element
 * is located. If 'xmlnode' is true, VARHDRSZ is not included in this offset.
 */
char *
copyXMLNode(XMLNodeHdr node, char *target, bool xmlnode, XMLNodeOffset * root)
{
	char	   *content = NULL;
	unsigned int cntLen = 0;
	char	   *end,
			   *start,
			   *result,
			   *data;
	unsigned int dataLength,
				resultLength;
	XMLNodeOffset *offPtr;

	start = NULL;
	if (node->kind == XMLNODE_ELEMENT || node->kind == XMLNODE_DOC || node->kind == XMLNODE_DOC_FRAGMENT)
	{
		XMLCompNodeHdr compNode = (XMLCompNodeHdr) node;

		content = XNODE_ELEMENT_NAME(compNode);
		switch (node->kind)
		{
			case XMLNODE_ELEMENT:
				cntLen = strlen(content) + 1;
				break;

			case XMLNODE_DOC:
				if (node->flags & XNODE_DOC_XMLDECL)
				{
					cntLen = sizeof(XMLDeclData);
				}
				else
				{
					cntLen = 0;
				}
				break;

			default:
				cntLen = 0;
				break;
		}
		start = (char *) getFirstXMLNodeLeaf(compNode);
	}
	else if (node->kind == XMLNODE_ATTRIBUTE || node->kind == XMLNODE_PI)
	{
		content = (char *) node + sizeof(XMLNodeHdrData);

		/*
		 * Both name and value need to be taken into account
		 */
		cntLen = strlen(content) + 1;
		if (!(node->kind == XMLNODE_PI && ((node->flags & XNODE_PI_HAS_VALUE) == 0)))
		{
			char	   *cntTmp = content + cntLen;

			cntLen += strlen(cntTmp) + 1;
		}
		start = (char *) node;
	}
	else
	{
		switch (node->kind)
		{
			case XMLNODE_DTD:
			case XMLNODE_COMMENT:
			case XMLNODE_CDATA:
			case XMLNODE_PI:
			case XMLNODE_TEXT:
				content = (char *) node + sizeof(XMLNodeHdrData);
				cntLen = strlen(content) + 1;
				break;

			default:
				elog(ERROR, "unable to copy node of type %u", node->kind);
				break;
		}
		start = (char *) node;
	}

	if (root != NULL)
	{
		*root = (char *) node - start;
	}
	end = content + cntLen;

	if (xmlnode)
	{
		dataLength = end - start + sizeof(XMLNodeOffset);
		resultLength = dataLength + VARHDRSZ;
		result = (target != NULL) ? target : (char *) palloc(resultLength);
		data = VARDATA(result);
		offPtr = (XMLNodeOffset *) (result + resultLength - sizeof(XMLNodeOffset));
		*offPtr = (char *) node - start;
		memcpy(data, start, dataLength - sizeof(XMLNodeOffset));
		SET_VARSIZE(result, resultLength);
		return result;
	}
	else
	{
		dataLength = end - start;
		result = (target != NULL) ? target : (char *) palloc(dataLength);
		memcpy(result, start, dataLength);
		return result;
	}
}


/*
 * Returns first leaf node of a subtree that starts with 'elh'. This is tight
 * to the parser behaviour: children are stored at lower addresses than
 * parents. First, the element attributes are stored, then other child nodes
 * (the same logic is applied on them recursively) and finally the element
 * itself.
 */

XMLNodeHdr
getFirstXMLNodeLeaf(XMLCompNodeHdr compNode)
{

	if (!XNODE_HAS_CHILDREN(compNode))
	{
		return (XMLNodeHdr) compNode;
	}
	else
	{
		char	   *firstRef = XNODE_FIRST_REF(compNode);
		XMLNodeHdr	childNode = (XMLNodeHdr) ((char *) (compNode) - readXMLNodeOffset(&firstRef,
									 XNODE_GET_REF_BWIDTH(compNode), false));

		if (childNode->kind == XMLNODE_ELEMENT || childNode->kind == XMLNODE_DOC ||
			childNode->kind == XMLNODE_DOC_FRAGMENT)
		{
			return getFirstXMLNodeLeaf((XMLCompNodeHdr) childNode);
		}
		else
		{
			return childNode;
		}
	}

	/*
	 * Use return just to suppress compiler warnings. Control never gets
	 * there.
	 */
	return NULL;
}

/*
 * 'xscan' - the scan to be initialized 'xpath' - location path to be used
 * for this scan 'xpHdr' - header of the 'path set' (varlena value) the
 * 'xpath' is contained in. We may need it if one or more predicates in the
 * 'xpath' contain other paths (subpaths) as operands. 'scanRoot' - node
 * where the scan starts. This node won't be tested itself, the scan starts
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
		firstLevel->matches = 0;
		firstLevel->up = (parent == NULL) ? NULL : XMLSCAN_CURRENT_LEVEL(parent);

		xscan->state = firstLevel;
	}
	else
	{
		xscan->state = NULL;
	}

	xscan->depth = 0;
	xscan->skip = false;
	xscan->subtreeDone = false;
	xscan->descsDone = false;
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
			xscan->resTmp = (XMLNodeContainer) palloc(sizeof(XMLNodeContainerData));
			xmlnodeContainerInit(xscan->resTmp);
		}
		else
		{
			xscan->resTmp = xscan->parent->resTmp;
		}
	}
	else
	{
		xscan->resTmp = NULL;
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
	if (xscan->resTmp != NULL && xscan->parent == NULL)
	{
		xmlnodeContainerFree(xscan->resTmp);
		pfree(xscan->resTmp);
	}
}

/*
 * Find the next matching node.
 *
 * 'xscan' - scan status. It remembers where the last scan ended. If used for consequent call, the function
 * will continue right after that position.
 * If caller changes the document between calls to this function, he is responsible for adjusting the
 * scan status and - if some exist - state of any sub-scan. In addition, 'xscan->document' of the scan and
 * sub-scans has to point to the new (modified) document.
 *
 * 'removed' indicates that requires special behaviour is required: when the previous call returned
 * a valid node for removal, scan has to stay at the same position when it resumes, as opposed to moving
 * forward. Due to the removal, another node appears at the position of the removed one.
 *
 * Returns a pointer to the matching node. This points to inside 'xscan->document' and therefore
 * must not be pfree'd.
 */
XMLNodeHdr
getNextXMLNode(XMLScan xscan, bool removed)
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
			XPath		xp = xscan->xpath;
			XMLCompNodeHdr eh = scanLevel->parent;
			XMLNodeHdr	currentNode = NULL;

			/*
			 * Indicates later in the loop whether sub-scan has just finished.
			 */
			bool		subScanDone = false;

			if (scanLevel->nodeRefPtr == NULL)
			{
				/*
				 * When this function is called from xmlnodeRemove(), the node
				 * pointed to by 'childRef' might have been removed. If that
				 * was the only child, NULL is there when the scan resumes.
				 */
				if (scanLevel->siblingsLeft != 1)
				{
					elog(ERROR, "unexpected state of xml scan");
				}
				else
				{
					scanLevel->siblingsLeft--;
					break;
				}

			}
			xpEl = (XPathElement) ((char *) xp + xp->elements[xscan->xpathRoot + xscan->depth]);

			if (xscan->subScan != NULL)
			{
				XMLNodeHdr	subNode = getNextXMLNode(xscan->subScan, removed);

				/*
				 * isNodeUnique() is not used here on return because the check
				 * has been performed when returning from the getNextXMLNode()
				 * above.
				 *
				 * rememberResult() is not used bellow for the same reason.
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
					xscan->descsDone = true;
					subScanDone = true;
				}
			}
			currentNode = (XMLNodeHdr) ((char *) eh -
										readXMLNodeOffset(&scanLevel->nodeRefPtr, XNODE_GET_REF_BWIDTH(eh), false));

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
				 *
				 * If 'removed' is true, sub-scan makes no sense because the
				 * node the last scan ended at no longer exists.
				 */
				if (!removed)
				{
					if (considerSubScan(xpEl, currentNode, xscan))
					{
						continue;
					}
				}

				xscan->skip = false;
				/* This applies to the next node. */
				xscan->descsDone = false;

				/*
				 * If the node we found last time has been removed, then the
				 * scan points to the next node at the same level.
				 * 'subScanDone' indicates that sub-scan had to be used and
				 * therefore a node could only have been removed from lower
				 * level. In this case we do need to move forward.
				 */

				if (!removed || (removed && (subScanDone || xscan->subtreeDone)))
				{
					scanLevel->nodeRefPtr = XNODE_NEXT_REF(scanLevel->nodeRefPtr, eh);

					if (xscan->subtreeDone)
					{
						xscan->subtreeDone = false;
					}
				}
				scanLevel->siblingsLeft--;
				continue;
			}

			/*
			 * Evaluate the node according to its type
			 */
			if (currentNode->kind == XMLNODE_ELEMENT)
			{
				XMLCompNodeHdr currentElement = (XMLCompNodeHdr) currentNode;
				char	   *childFirst = XNODE_FIRST_REF(currentElement);
				char	   *name = XNODE_ELEMENT_NAME(currentElement);
				char	   *nameTest = xpEl->name;

				if (XPATH_LAST_LEVEL(xscan) && xscan->xpath->targNdKind == XMLNODE_NODE &&
					isNodeUnique(currentNode, xscan))
				{
					xscan->skip = true;
					rememberResult(currentNode, xscan);
					return currentNode;
				}
				if (strcmp(name, nameTest) == 0)
				{
					bool		passed = true;

					scanLevel->matches++;
					if (xpEl->hasPredicate)
					{
						XPathExprOperandValueData result,
									resultBool;
						XPathExpression exprOrig = (XPathExpression) ((char *) xpEl + sizeof(XPathElementData) +
														   strlen(nameTest));
						XPathExpression expr = prepareXPathExpression(exprOrig, currentElement,
								 xscan->document, xscan->xpathHeader, xscan);

						evaluateXPathExpression(expr, scanLevel, currentElement, 0, &result);
						freeNodeSets(expr);
						pfree(expr);
						xpathValCastToBool(&result, &resultBool);
						passed = resultBool.v.boolean;
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
							nextLevel->matches = 0;
							nextLevel->up = scanLevel;
							break;
						}
					}
					else if (XPATH_LAST_LEVEL(xscan) &&

						/*
						 * Uniqueness is only checked if the last qualifying
						 * node hasn't been deleted. If that has been deleted,
						 * the current match means it's a different node that
						 * just moved to the offset of the original one.
						 */
							 (removed || isNodeUnique(currentNode, xscan)))
					{
						/*
						 * We're at the end of the xpath.
						 */
						xscan->skip = true;
						/* Return the matching node. */
						rememberResult(currentNode, xscan);
						return currentNode;
					}
				}
			}
			else if (XPATH_LAST_LEVEL(xscan) && isNodeUnique(currentNode, xscan))
			{
				if (currentNode->kind == xscan->xpath->targNdKind)
				{
					if (currentNode->kind == XMLNODE_TEXT || currentNode->kind == XMLNODE_COMMENT)
					{
						xscan->skip = true;
						rememberResult(currentNode, xscan);
						return currentNode;
					}
					else if (currentNode->kind == XMLNODE_PI)
					{
						char	   *piTarget = (char *) (currentNode + 1);
						char	   *piTargTest = xpEl->name;

						if (xscan->xpath->piTestValue)
						{
							if (strcmp(piTarget, piTargTest) == 0)
							{
								xscan->skip = true;
								rememberResult(currentNode, xscan);
								return currentNode;
							}
						}
						else
						{
							xscan->skip = true;
							rememberResult(currentNode, xscan);
							return currentNode;
						}

					}
					else if (currentNode->kind == XMLNODE_ATTRIBUTE)
					{
						if (xscan->xpath->allAttributes)
						{
							xscan->skip = true;
							rememberResult(currentNode, xscan);
							return currentNode;
						}
						else
						{
							char	   *attrName = (char *) currentNode + sizeof(XMLNodeHdrData);
							char	   *attrNameTest = xpEl->name;

							if (strcmp(attrNameTest, attrName) == 0)
							{
								xscan->skip = true;
								rememberResult(currentNode, xscan);
								return currentNode;
							}
						}
					}
				}
				else if (xscan->xpath->targNdKind == XMLNODE_NODE && currentNode->kind != XMLNODE_ATTRIBUTE)
				{
					xscan->skip = true;
					rememberResult(currentNode, xscan);
					return currentNode;
				}
			}

			/*
			 * No match found. The current path element however might be
			 * interested in descendants.
			 */
			if (considerSubScan(xpEl, currentNode, xscan))
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
				(xscan->depth)--;
				xscan->subtreeDone = true;
				continue;
			}
		}
	}
	/* keep the compiler silent */
	return NULL;
}

void
checkXMLWellFormedness(XMLCompNodeHdr root)
{
	unsigned short int i,
				elIndex,
				dtdIndex;
	char	   *refStream = XNODE_FIRST_REF(root);
	unsigned short int elements = 0;
	unsigned short int dtds = 0;

	if (root->common.kind != XMLNODE_DOC && root->common.kind != XMLNODE_DOC_FRAGMENT)
	{
		elog(ERROR, "well-formedness  can't be checked for node type %u", root->common.kind);
	}
	elIndex = 0;
	dtdIndex = 0;
	for (i = 0; i < root->children; i++)
	{
		XMLNodeOffset ref = readXMLNodeOffset(&refStream, XNODE_GET_REF_BWIDTH(root), true);
		XMLNodeHdr	currNode = (XMLNodeHdr) ((char *) root - ref);

		if (currNode->kind == XMLNODE_ELEMENT)
		{
			elements++;
			elIndex = i;
		}
		else if (currNode->kind == XMLNODE_DTD)
		{
			dtds++;
			dtdIndex = i;
		}
		else if (currNode->kind == XMLNODE_DOC || currNode->kind == XMLNODE_ATTRIBUTE ||
				 currNode->kind == XMLNODE_CDATA || currNode->kind == XMLNODE_TEXT ||
				 currNode->kind == XMLNODE_DOC_FRAGMENT)
		{
			elog(ERROR, "%s must not be a direct child of the root",
				 getXMLNodeKindStr(currNode->kind));
		}
	}
	if (elements != 1)
	{
		elog(ERROR, "well-formed document must contain exactly one root element");
	}
	if (dtds > 1)
	{
		elog(ERROR, "well-formed document must not contain more than one DTD node");
	}
	if (dtds > 0 && dtdIndex > elIndex)
	{
		elog(ERROR, "element must not be located before DTD");
	}
}


/*
 * Check if UTF-8 character 'c' fits one item of the 'intervals'.array.
 */
bool
isXMLCharInInterval(char *c, UTF8Interval * intervals, unsigned short int intCount)
{
	unsigned short int i;
	UTF8Interval *interval = intervals;

	for (i = 0; i < intCount; i++)
	{
		if (utf8cmp(c, interval->first) >= 0 && utf8cmp(c, interval->last) <= 0)
		{
			return true;
		}
		interval++;
	}
	return false;
}


int
utf8cmp(char *c1, char *c2)
{
	unsigned char len1 = pg_utf_mblen((unsigned char *) c1);
	unsigned char len2 = pg_utf_mblen((unsigned char *) c2);

	Assert(len1 <= UTF_MAX_WIDTH && len2 <= UTF_MAX_WIDTH);
	if (len1 != len2)
	{
		return len1 > len2 ? 1 : -1;
	}
	else
	{
		unsigned char j;

		for (j = 0; j < len1; j++)
		{
			if (*c1 != *c2)
			{
				return *c1 > *c2 ? 1 : -1;
			}
			c1++;
			c2++;
		}
		return 0;
	}
}

PG_FUNCTION_INFO_V1(xmlnode_add);

Datum
xmlnode_add(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);
	xpath		xpathPtr = (xpath) PG_GETARG_POINTER(1);
	XPathExpression exprBase = (XPathExpression) VARDATA(xpathPtr);
	XPathHeader xpHdr = (XPathHeader) ((char *) exprBase + exprBase->size);
	XPath		xpath = getSingleXPath(exprBase, xpHdr);
	xmlnode		newNdVar = (xmlnode) PG_GETARG_POINTER(2);
	BpChar	   *modeVar = PG_GETARG_BPCHAR_PP(3);
	char		mode = *(VARDATA_ANY(modeVar));
	XMLNodeHdr	newNode;
	xmldoc		result;
	XMLScanData xscan;
	XMLCompNodeHdr docRoot;

	if (xpath->relative)
	{
		elog(ERROR, "absolute XPath expected");
	}
	if (xpath->depth == 0)
	{
		elog(ERROR, "invalid target path");
	}
	newNode = XNODE_ROOT(newNdVar);
	if (newNode->kind == XMLNODE_ATTRIBUTE)
	{
		elog(ERROR, "invalid node type to add: %s", getXMLNodeKindStr(newNode->kind));
	}
	docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);
	initXMLScan(&xscan, NULL, xpath, xpHdr, docRoot, doc, xpath->descendants > 1);
	result = updateXMLDocument(&xscan, doc, XMLNODE_ACTION_ADD, newNode, XMLADD_MODE(mode));
	finalizeXMLScan(&xscan);
	PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(xmlnode_remove);

extern Datum
xmlnode_remove(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);
	xpath		xpathPtr = (xpath) PG_GETARG_POINTER(1);
	XPathExpression exprBase = (XPathExpression) VARDATA(xpathPtr);
	XPathHeader xpHdr = (XPathHeader) ((char *) exprBase + exprBase->size);
	XPath		xpath = getSingleXPath(exprBase, xpHdr);
	xmldoc		result;
	XMLScanData xscan;
	XMLCompNodeHdr docRoot;

	if (xpath->relative)
	{
		elog(ERROR, "absolute XPath expected");
	}
	if (xpath->depth == 0)
	{
		elog(ERROR, "invalid target path");
	}
	docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);
	initXMLScan(&xscan, NULL, xpath, xpHdr, docRoot, doc, xpath->descendants > 1);
	result = updateXMLDocument(&xscan, doc, XMLNODE_ACTION_REMOVE, NULL, XMLADD_INVALID);

	finalizeXMLScan(&xscan);
	PG_RETURN_POINTER(result);
}

/*
 * 'element' is the 'owning element' of the predicate that we're just
 * evaluating. In general - a context node.
 */
void
evaluateXPathExpression(XPathExpression expr, XMLScanOneLevel scan,
						XMLCompNodeHdr element, unsigned short recursionLevel, XPathExprOperandValue result)
{

	unsigned short i;
	char	   *currentPtr = (char *) expr;
	XPathExprOperand currentOpnd;
	unsigned int currentSize;
	XPathExprOperator operator;
	XPathExprOperatorId firstOp = 0;

	currentPtr += sizeof(XPathExpressionData);
	if (recursionLevel == 0)
	{
		unsigned short varSize = expr->variables * sizeof(XPathOffset);

		currentPtr += varSize;
	}
	currentOpnd = (XPathExprOperand) currentPtr;
	prepareLiteral(currentOpnd);
	currentSize = evaluateXPathOperand(currentOpnd, scan, element, recursionLevel, result);

	if (expr->members == 1)
	{
		/*
		 * A single numeric value indicates position.
		 */
		if (scan != NULL && recursionLevel == 0 && currentOpnd->type == XPATH_OPERAND_LITERAL &&
			currentOpnd->value.type == XPATH_VAL_NUMBER)
		{
			int			intValue = (int) currentOpnd->value.v.num;

			result->type = XPATH_VAL_BOOLEAN;
			result->isNull = false;
			result->v.boolean = (intValue == scan->matches);
			return;
		}
		else
		{
			return;
		}
	}
	for (i = 0; i < expr->members - 1; i++)
	{
		XPathExprOperandValueData resTmp,
					currentVal;

		currentPtr += currentSize;
		operator = (XPathExprOperator) currentPtr;
		currentSize = sizeof(XPathExprOperatorData);
		currentPtr += currentSize;
		if (i == 0)
		{
			firstOp = operator->id;
		}
		currentOpnd = (XPathExprOperand) currentPtr;
		prepareLiteral(currentOpnd);

		currentSize = evaluateXPathOperand(currentOpnd, scan, element, recursionLevel, &currentVal);
		evaluateBinaryOperator(result, &currentVal, operator, &resTmp, element);
		memcpy(result, &resTmp, sizeof(XPathExprOperandValueData));

		/*
		 * Short evaluation.
		 *
		 * If the first operator is OR, then all operators on the same level
		 * must be OR too. The same applies to AND. These operators are unique
		 * in terms of precedence.
		 */
		if (firstOp == XPATH_EXPR_OPERATOR_OR)
		{
			Assert(result->type == XPATH_VAL_BOOLEAN);
			if (result->v.boolean)
			{
				return;
			}
		}
		else if (firstOp == XPATH_EXPR_OPERATOR_AND)
		{
			Assert(result->type == XPATH_VAL_BOOLEAN);
			if (!result->v.boolean)
			{
				return;
			}
		}
	}
}

static unsigned int
evaluateXPathOperand(XPathExprOperand operand, XMLScanOneLevel scan, XMLCompNodeHdr element,
				 unsigned short recursionLevel, XPathExprOperandValue result)
{
	unsigned int resSize;
	XPathExpression expr = (XPathExpression) operand;

	if (operand->type == XPATH_OPERAND_EXPR_SUB)
	{
		evaluateXPathExpression(expr, scan, element, recursionLevel + 1, result);
		resSize = expr->size;
	}
	else if (operand->type == XPATH_OPERAND_FUNC)
	{
		evaluateXPathFunction(expr, scan, element, recursionLevel + 1, result);
		resSize = expr->size;
	}
	else
	{
		memcpy(result, &operand->value, sizeof(XPathExprOperandValueData));
		resSize = operand->size;
	}
	return resSize;
}

static void
evaluateXPathFunction(XPathExpression funcExpr, XMLScanOneLevel scan, XMLCompNodeHdr element,
				 unsigned short recursionLevel, XPathExprOperandValue result)
{

	char	   *c = (char *) funcExpr + sizeof(XPathExpressionData);
	unsigned short i;
	XPathExprOperandValueData argsData[XPATH_FUNC_MAX_ARGS];
	XPathExprOperandValue args = argsData;
	XPathExprOperandValue argsTmp = args;
	XPathFunctionId funcId;
	XPathFunction function;


	for (i = 0; i < funcExpr->members; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) c;
		unsigned int opndSize;

		if (opnd->type == XPATH_OPERAND_EXPR_TOP || opnd->type == XPATH_OPERAND_EXPR_SUB)
		{
			XPathExpression subExpr = (XPathExpression) opnd;

			evaluateXPathExpression(subExpr, scan, element, recursionLevel, argsTmp);
			opndSize = subExpr->size;
		}
		else
		{
			memcpy(argsTmp, &opnd->value, sizeof(XPathExprOperandValueData));

			/*
			 * prepareLiteral() can't be used here because we're setting a
			 * separate chunk of memory (not the expression buffer).
			 */
			if (opnd->type == XPATH_OPERAND_LITERAL && opnd->value.type == XPATH_VAL_STRING)
			{
				argsTmp->v.string.str = XPATH_GEN_VALUE_STRING(&opnd->value);
				argsTmp->v.string.mustFree = false;
			}
			opndSize = opnd->size;
		}
		c += opndSize;
		argsTmp++;
	}

	funcId = funcExpr->funcId;
	function = &xpathFunctions[funcId];
	function->impl(function->nargs, args, result);
}

static void
prepareLiteral(XPathExprOperand operand)
{
	if (operand->type == XPATH_OPERAND_LITERAL && operand->value.type == XPATH_VAL_STRING)
	{
		operand->value.v.string.str = XPATH_GEN_VALUE_STRING(&operand->value);
		operand->value.v.string.mustFree = false;
	}
}

/*
 * Return a copy of an expression, prepared for evaluation. That is, all
 * variables are substituted in the resulting expression according to the
 * context element.
 *
 * The function does process all sub-expressions (implicit / explicit) and
 * (sub)paths. When it tries to substitute node-sets for sub-paths, it has to
 * process predicate expressions of those sub-paths (which can again contain
 * sub-paths, etc.)
 */
XPathExpression
prepareXPathExpression(XPathExpression exprOrig, XMLCompNodeHdr ctxElem,
					   xmldoc document, XPathHeader xpHdr, XMLScan xscan)
{
	XPathExpression expr = (XPathExpression) palloc(exprOrig->size);

	memcpy(expr, exprOrig, exprOrig->size);

	/* Replace attribute names with the values found in the current node.  */
	substituteAttributes(expr, ctxElem);

	if (expr->npaths > 0)
	{
		/* Replace paths with matching node-sets. */
		substituteSubpaths(expr, ctxElem, document, xpHdr);
	}
	if (expr->nfuncs)
	{
		/*
		 * Functions with no arguments can also be replaced by value before
		 * evaluation starts.
		 */
		substituteFunctions(expr, xscan);
	}
	return expr;
}

static void
freeNodeSets(XPathExpression expr)
{
	XPathOffset *varOffPtr = (XPathOffset *) ((char *) expr + sizeof(XPathExpressionData));
	unsigned short i;

	Assert(expr->type == XPATH_OPERAND_EXPR_TOP);

	if (!expr->hasNodesets)
	{
		return;
	}
	for (i = 0; i < expr->variables; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) ((char *) expr +
													*varOffPtr);

		if (opnd->value.type == XPATH_VAL_NODESET && !opnd->value.v.nodeSet.isDocument && !opnd->value.isNull
			&& opnd->value.v.nodeSet.count > 1)
		{
			pfree(opnd->value.v.nodeSet.nodes.array);
		}
		varOffPtr++;
	}
}

/*
 * TODO divide the logic into multiple functions
 */

static void
evaluateBinaryOperator(XPathExprOperandValue valueLeft, XPathExprOperandValue valueRight,
					   XPathExprOperator operator, XPathExprOperandValue result, XMLCompNodeHdr element)
{

	if (operator->id == XPATH_EXPR_OPERATOR_OR || operator->id == XPATH_EXPR_OPERATOR_AND)
	{
		bool		left,
					right;
		XPathExprOperandValueData valueTmp;

		if (valueLeft->type == XPATH_VAL_NODESET && valueRight->type == XPATH_VAL_NODESET && valueLeft->v.nodeSet.isDocument &&
			valueRight->v.nodeSet.isDocument)
		{
			elog(ERROR, "invalid xpath expression");
		}

		/*
		 * Subexpression is currently the only type of operand of boolean type
		 */
		xpathValCastToBool(valueLeft, &valueTmp);
		left = valueTmp.v.boolean;
		xpathValCastToBool(valueRight, &valueTmp);
		right = valueTmp.v.boolean;

		result->type = XPATH_VAL_BOOLEAN;
		result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_OR) ? left || right : left && right;
		return;
	}
	else if (operator->id == XPATH_EXPR_OPERATOR_EQ || operator->id == XPATH_EXPR_OPERATOR_NEQ)
	{
		bool		equal;
		XPathValueType typeLeft = valueLeft->type;
		XPathValueType typeRight = valueRight->type;
		XPathExprOperandValueData valueTmp;

		result->type = XPATH_VAL_BOOLEAN;
		if (typeLeft == XPATH_VAL_BOOLEAN || typeRight == XPATH_VAL_BOOLEAN)
		{
			bool		boolLeft,
						boolRight;

			/*
			 * As we don't know which one is boolean and which one is
			 * something else, cast both values to boolean
			 */
			xpathValCastToBool(valueLeft, &valueTmp);
			boolLeft = valueTmp.v.boolean;
			xpathValCastToBool(valueRight, &valueTmp);
			boolRight = valueTmp.v.boolean;
			;
			equal = boolLeft == boolRight;
			result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_EQ) ? equal : !equal;
			return;
		}
		else if (typeLeft == XPATH_VAL_NUMBER || typeRight == XPATH_VAL_NUMBER)
		{
			compareNumValues(valueLeft, valueRight, operator, result);
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
				 * Document is not a typical nod-eset, let's exclude it first.
				 */
				if (nsLeft->isDocument || nsRight->isDocument)
				{
					result->v.boolean = operator->id == XPATH_EXPR_OPERATOR_EQ;
					return;
				}
				if (nsLeft->count == 0 || nsRight->count == 0)
				{
					result->v.boolean = false;
					return;
				}
				result->v.boolean = compareNodeSets(&valueLeft->v.nodeSet, &valueRight->v.nodeSet, operator);
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
					result->v.boolean = false;
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
				result->v.boolean = compareValueToNodeSet(nonSetValue, nodeSet, operator);
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

				strLeft = valueLeft->v.string.str;
				strRight = valueRight->v.string.str;

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
		compareNumValues(valueLeft, valueRight, operator, result);
		result->type = XPATH_VAL_BOOLEAN;
		return;
	}
	else
	{
		elog(ERROR, "unknown operator to evaluate: %u", operator->id);
		result->v.boolean = false;
		return;
	}
}

/*
 * At least one operand must be a number The non-numeric operand may only be
 * a string or a node-set.
 */
static void
compareNumValues(XPathExprOperandValue valueLeft, XPathExprOperandValue valueRight,
				 XPathExprOperator operator, XPathExprOperandValue result)
{

	XPathValueType typeLeft = valueLeft->type;
	XPathValueType typeRight = valueRight->type;

	if (typeLeft == XPATH_VAL_NUMBER && typeRight == XPATH_VAL_NUMBER)
	{
		compareNumbers(valueLeft->v.num, valueRight->v.num, operator, result);
		return;
	}
	else if (typeLeft == XPATH_VAL_NUMBER || typeRight == XPATH_VAL_NUMBER)
	{
		/* one operand is a number, the other is not */
		double		num1,
					num2;
		bool		reverse = false;
		XPathExprOperandValue valueNum,
					valueNonNum;

		if (typeLeft == XPATH_VAL_NUMBER)
		{
			valueNum = valueLeft;
			valueNonNum = valueRight;
		}
		else
		{
			valueNum = valueRight;
			valueNonNum = valueLeft;
			reverse = true;
		}
		if (valueNonNum->isNull)
		{
			result->v.boolean = false;
			return;
		}
		num1 = valueNum->v.num;

		/*
		 * Only string, especially one originating from attribute name
		 * substitution, may have 'xpathValCastToNum' set.
		 */
		if (valueNonNum->type == XPATH_VAL_STRING)
		{
			if (valueNonNum->castToNumber)
			{
				XPathExprOperandValueData valueTmp;

				xpathValCastToNum(valueNonNum, &valueTmp);
				num2 = valueTmp.v.num;
				if (reverse)
				{
					compareNumbers(num2, num1, operator, result);
				}
				else
				{
					compareNumbers(num1, num2, operator, result);
				}
			}
			else
			{
				/*
				 * If one operand is a number and the other is not, the XPath
				 * standard claims that bot be cast to strings and compared.
				 * However it doesn't seem to make much sense to compare
				 * number and NaN values. The strings will always differ.
				 * (i.e. valid number can't be converted to a non-numeric
				 * string).
				 */
				result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_NEQ);
			}
			return;
		}
		else if (valueNonNum->type == XPATH_VAL_NODESET)
		{
			if (valueNonNum->v.nodeSet.isDocument)
			{
				result->v.boolean = (operator->id == XPATH_EXPR_OPERATOR_NEQ);
				return;
			}
			if (reverse)
			{
				switch (operator->id)
				{
					case XPATH_EXPR_OPERATOR_LT:
						operator->id = XPATH_EXPR_OPERATOR_GT;
						break;

					case XPATH_EXPR_OPERATOR_LTE:
						operator->id = XPATH_EXPR_OPERATOR_GTE;
						break;

					case XPATH_EXPR_OPERATOR_GT:
						operator->id = XPATH_EXPR_OPERATOR_LT;
						break;

					case XPATH_EXPR_OPERATOR_GTE:
						operator->id = XPATH_EXPR_OPERATOR_LTE;
						break;

					default:
						break;

				}
			}
			result->v.boolean = compareValueToNodeSet(valueNum, &valueNonNum->v.nodeSet, operator);
			return;
		}
		else
		{
			/*
			 * 'number-to-number' comparisons must have been handled elsewhere
			 * in this function 'number-to-boolean' comparisons must have been
			 * handled above outside this function. In such a case 'number ->
			 * boolean' cast has higher precedence than 'boolean->number'
			 */
			elog(ERROR, "unexpected value type to be compared to number: %u", valueNonNum->type);
			return;
		}
	}
	else
	{
		result->v.boolean = false;
		return;
	}
}

static void
compareNumbers(double numLeft, double numRight, XPathExprOperator operator,
			   XPathExprOperandValue result)
{

	result->type = XPATH_VAL_BOOLEAN;
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

void
xpathValCastToBool(XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst)
{
	valueDst->type = XPATH_VAL_BOOLEAN;
	switch (valueSrc->type)
	{
		case XPATH_VAL_BOOLEAN:
			valueDst->v.boolean = valueSrc->v.boolean;
			break;

		case XPATH_VAL_NUMBER:
			valueDst->v.boolean = (valueSrc->v.num != 0.0);
			break;

		case XPATH_VAL_STRING:
			valueDst->v.boolean = (!valueSrc->isNull && (strlen(valueSrc->v.string.str) > 0));
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

void
xpathValCastToStr(XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst)
{
	valueDst->type = XPATH_VAL_STRING;
	valueDst->isNull = valueSrc->isNull;

	switch (valueSrc->type)
	{
		case XPATH_VAL_STRING:

			if (!valueSrc->isNull)
			{
				if (valueSrc->v.string.mustFree)
				{
					char	   *src = valueSrc->v.string.str;
					unsigned int len = sizeof(src) + 1;
					char	   *dst = (char *) palloc(len);

					memcpy(dst, src, len);
					valueDst->v.string.str = dst;
				}
				else
				{
					valueDst->v.string.str = valueSrc->v.string.str;
				}

				valueDst->v.string.mustFree = valueSrc->v.string.mustFree;
				return;
			}
			break;

		case XPATH_VAL_NODESET:
			if (valueSrc->v.nodeSet.isDocument)
			{
				elog(ERROR, "document can't be cast to string");
			}
			if (!valueSrc->isNull)
			{
				XPathNodeSet ns = &valueSrc->v.nodeSet;

				/*
				 * If the node-set contains multiple nodes,only the first on
				 * is used (http://www.w3.org/TR/1999/REC-xpath-19991116/#sect
				 * ion-String-Functions)
				 */
				XMLNodeHdr	node = ns->count == 1 ? ns->nodes.single : ns->nodes.array[0];

				if (node->kind == XMLNODE_ELEMENT)
				{
					valueDst->v.string.str = getElementNodeStr((XMLCompNodeHdr) node);
					valueDst->v.string.mustFree = true;
				}
				else
				{
					valueDst->v.string.str = getNonElementNodeStr(node);
					valueDst->v.string.mustFree = false;
				}
			}
			break;


		default:
			elog(ERROR, "unable to cast type %u to string", valueSrc->type);
			break;
	}
}

/*
 * Neither null value nor NaN strings expected.
 */
void
xpathValCastToNum(XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst)
{
	valueDst->type = XPATH_VAL_NUMBER;
	switch (valueSrc->type)
	{
		case XPATH_VAL_NUMBER:
			valueDst->v.num = valueSrc->v.num;
			break;

		case XPATH_VAL_STRING:
			Assert(operand->common.castToNumber);
			valueDst->v.num = getNumValue(valueSrc->v.string.str);
			break;

		default:
			elog(ERROR, "unable to cast type %u to number", valueSrc->type);
			break;
	}
}

static double
getNumValue(char *str)
{
	double		result;
	char	   *c;

	errno = 0;
	result = strtod(str, &c);
	if (errno != 0)
	{
		elog(ERROR, "unable to cast %s to number", str);
	}
	return result;
}

/*
 * Returns true if the node sets are equal according to
 * http://www.w3.org/TR/1999/REC-xpath-19991116/#booleans
 */
static bool
compareNodeSets(XPathNodeSet ns1, XPathNodeSet ns2, XPathExprOperator operator)
{
	XMLNodeHdr *arrayOuter = (ns1->count > 1) ? ns1->nodes.array : &(ns1->nodes.single);
	XMLNodeHdr *arrayInner = (ns2->count > 1) ? ns2->nodes.array : &(ns2->nodes.single);
	XPathNodeSet nsOuter = ns1;
	XPathNodeSet nsInner = ns2;
	unsigned short i;

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
				valueOuter.v.string.str = nodeStr;
				valueOuter.v.string.mustFree = false;
				if (compareValueToNode(&valueOuter, nodeInner, operator))
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

	nodeLeft = getNextXMLNode(&scanLeft, false);
	nodeRight = getNextXMLNode(&scanRight, false);
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
			nodeLeft = getNextXMLNode(&scanLeft, false);
			if (nodeLeft == NULL)
			{
				break;
			}
			textLeft = XNODE_CONTENT(nodeLeft);
			textRight += charsToCompare;
		}
		else if (lengthLeft > lengthRight)
		{
			nodeRight = getNextXMLNode(&scanRight, false);
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
			nodeLeft = getNextXMLNode(&scanLeft, false);
			nodeRight = getNextXMLNode(&scanRight, false);
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

static void
initScanForTextNodes(XMLScan xscan, XMLCompNodeHdr root)
{
	XPath		xpath = (XPath) palloc(sizeof(XPathData) + sizeof(XPathElementData));
	XPathElement xpEl = (XPathElement) ((char *) xpath + sizeof(XPathData));

	xpEl->descendant = true;
	xpEl->hasPredicate = false;
	xpath->depth = 1;
	xpath->targNdKind = XMLNODE_TEXT;
	xpath->allAttributes = false;
	xpath->elements[0] = sizeof(XPathData);
	initXMLScan(xscan, NULL, xpath, NULL, root, xscan->document, false);
}

static void
finalizeScanForTextNodes(XMLScan xscan)
{
	pfree(xscan->xpath);
	finalizeXMLScan(xscan);
}

/*
 * The operator direction (whether '<' or '>') assumes 'value' is on the left
 * side in the expression and node on the right. If it's the other way round,
 * the caller must switch the direction.
 */
static bool
compareValueToNode(XPathExprOperandValue value, XMLNodeHdr node, XPathExprOperator operator)
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
			char	   *cStr = value->v.string.str;
			XMLScanData textScan;

			strLen = strlen(cStr);
			initScanForTextNodes(&textScan, (XMLCompNodeHdr) node);

			while ((textNode = getNextXMLNode(&textScan, false)) != NULL)
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
			match = strcmp(value->v.string.str, nodeStr) == 0;
		}
		else
		{
			if (strlen(nodeStr) > 0)
			{
				compareNumToStr(value->v.num, nodeStr, operator, &result);
				return result.v.boolean;
			}
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
}

static char *
getElementNodeStr(XMLCompNodeHdr element)
{
	XMLScanData textScan;
	XMLNodeHdr	textNode;
	StringInfoData si;

	initScanForTextNodes(&textScan, element);

	/*
	 * Set the size to something smaller than what 'initStringInfo()' does
	 */
	si.maxlen = 32;
	si.data = (char *) palloc(si.maxlen);
	resetStringInfo(&si);

	while ((textNode = getNextXMLNode(&textScan, false)) != NULL)
	{
		char	   *cntPart = XNODE_CONTENT(textNode);

		appendStringInfoString(&si, cntPart);
	}
	finalizeScanForTextNodes(&textScan);
	return si.data;
}

/*
 * Textual form of a node is returned. Elements can be compared without
 * having to construct a single string out of the text nodes.
 */
static char *
getNonElementNodeStr(XMLNodeHdr node)
{
	if (node->kind == XMLNODE_ELEMENT)
	{
		elog(ERROR, "unexpected node kind %u", node->kind);
	}
	switch (node->kind)
	{
		case XMLNODE_ATTRIBUTE:
			{
				char	   *attName = XNODE_CONTENT(node);
				char	   *attValue = attName + strlen(attName) + 1;

				return attValue;
			}

		case XMLNODE_COMMENT:
		case XMLNODE_CDATA:
		case XMLNODE_TEXT:
			return XNODE_CONTENT(node);

		case XMLNODE_PI:
			if (node->flags & XNODE_PI_HAS_VALUE)
			{
				char	   *content = XNODE_CONTENT(node);

				content += strlen(content) + 1;
				return content;
			}
			else
			{
				return NULL;
			}

		default:
			elog(ERROR, "unable to compare node set element of type %u", node->kind);
			break;
	}
	return NULL;
}

/*
 * The operator direction (whether '<' or '>') assumes 'value' is on the left
 * side in the expression and node-set on the right. If it's the other way
 * round, the caller must switch the direction.
 */
static bool
compareValueToNodeSet(XPathExprOperandValue value, XPathNodeSet ns, XPathExprOperator operator)
{
	XMLNodeHdr *array = (ns->count > 1) ? ns->nodes.array : &(ns->nodes.single);
	unsigned short i;

	if (!(operator->id == XPATH_EXPR_OPERATOR_EQ || operator->id == XPATH_EXPR_OPERATOR_NEQ ||
		  operator->id == XPATH_EXPR_OPERATOR_LT || operator->id == XPATH_EXPR_OPERATOR_GT ||
		  operator->id == XPATH_EXPR_OPERATOR_LTE || operator->id == XPATH_EXPR_OPERATOR_GTE))
	{
		elog(ERROR, "unexpected operator: %u", operator->id);
	}
	for (i = 0; i < ns->count; i++)
	{
		XMLNodeHdr	node = array[i];

		if (compareValueToNode(value, node, operator))
		{
			return true;
		}
	}
	return false;
}


/*
 * Replace attribute names in the expression with actual values (i.e. with
 * values that the current element contains).
 */
static void
substituteAttributes(XPathExpression expr, XMLCompNodeHdr element)
{
	unsigned short childrenLeft = element->children;
	char	   *childFirst = XNODE_FIRST_REF(element);
	char	   *chldOffPtr = childFirst;

	while (childrenLeft > 0)
	{
		XMLNodeHdr	child = (XMLNodeHdr) ((char *) element -
		readXMLNodeOffset(&chldOffPtr, XNODE_GET_REF_BWIDTH(element), true));

		if (child->kind == XMLNODE_ATTRIBUTE)
		{
			char	   *attrName = XNODE_CONTENT(child);
			unsigned short i;
			XPathOffset *varOffPtr = (XPathOffset *) ((char *) expr +
												sizeof(XPathExpressionData));

			for (i = 0; i < expr->variables; i++)
			{
				XPathExprOperand opnd = (XPathExprOperand) ((char *) expr +
															*varOffPtr);

				if (opnd->substituted)
				{
					varOffPtr++;
					continue;
				}
				if (opnd->type == XPATH_OPERAND_ATTRIBUTE &&
				 strcmp(attrName, XPATH_GEN_VALUE_STRING(&opnd->value)) == 0)
				{
					char	   *attrValue = attrName + strlen(attrName) + 1;

					opnd->value.v.string.str = attrValue;
					opnd->value.v.string.mustFree = false;
					opnd->substituted = true;
					opnd->value.isNull = false;
					opnd->value.type = XPATH_VAL_STRING;
					opnd->value.castToNumber = XNODE_ATTR_IS_NUMBER(child);
				}
				varOffPtr++;
			}
		}
		childrenLeft--;
	}
}

/*
 * All sub-paths in 'expression' are replaced with the matching node-sets.
 *
 * expression - XPath expression containing the paths we're evaluating
 * (replacing with node-sets). element - context: XML element that we'll test
 * using 'expression' when the substitution is complete.
 */
static void
substituteSubpaths(XPathExpression expression, XMLCompNodeHdr element, xmldoc document,
				   XPathHeader xpHdr)
{
	unsigned short i,
				processed;
	XPathOffset *varOffPtr = (XPathOffset *) ((char *) expression + sizeof(XPathExpressionData));

	processed = 0;
	for (i = 0; i < expression->variables; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) ((char *) expression + *varOffPtr);

		if (opnd->type == XPATH_OPERAND_PATH)
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
			}
			else
			{
				initXMLScan(&xscanSub, NULL, subPath, xpHdr, element, document, subPath->descendants > 1);
				while ((matching = getNextXMLNode(&xscanSub, false)) != NULL)
				{
					XMLNodeHdr *array = NULL;

					Assert(matching->type == xscanSub.xpath.targNdType);

					if (count == 0)
					{
						opnd->value.v.nodeSet.nodes.single = matching;
					}
					else if (count == 1)
					{
						/*
						 * Adding 2nd node, so the array size must be at least
						 * 2.
						 */
						arrSize = (element->children > count) ? element->children : 2;
						array = (XMLNodeHdr *) palloc(sizeof(XMLNodeHdr) * arrSize);
						array[0] = opnd->value.v.nodeSet.nodes.single;
						array[1] = matching;
						opnd->value.v.nodeSet.nodes.array = array;
					}
					else
					{
						if (count >= arrSize)
						{
							/*
							 * This estimate might be o.k. whether the node
							 * has few or many children
							 */
							arrSize = count + (count >> 1) + 1;
							array = (XMLNodeHdr *) repalloc(opnd->value.v.nodeSet.nodes.array,
											   sizeof(XMLNodeHdr) * arrSize);
							opnd->value.v.nodeSet.nodes.array = array;

						}
						opnd->value.v.nodeSet.nodes.array[count] = matching;
					}
					count++;
				}
				opnd->value.isNull = (count == 0);
				opnd->value.v.nodeSet.count = count;
				opnd->value.v.nodeSet.isDocument = false;
				expression->hasNodesets = (expression->hasNodesets || count > 1);
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
substituteFunctions(XPathExpression expression, XMLScan xscan)
{
	unsigned short i,
				processed;
	XPathOffset *varOffPtr = (XPathOffset *) ((char *) expression + sizeof(XPathExpressionData));

	processed = 0;
	for (i = 0; i < expression->variables; i++)
	{
		XPathExprOperand opnd = (XPathExprOperand) ((char *) expression + *varOffPtr);

		if (opnd->type == XPATH_OPERAND_FUNC_NOARG)
		{
			XPathFunctionId funcId = opnd->value.v.funcId;
			XMLScanOneLevel scanLevel;

			switch (funcId)
			{
				case XPATH_FUNC_POSITION:
					opnd->value.type = XPATH_VAL_NUMBER;
					scanLevel = XMLSCAN_CURRENT_LEVEL(xscan);
					opnd->value.v.num = scanLevel->matches;
					break;

				default:
					elog(ERROR, "unexpected function id: %u", funcId);
					break;
			}
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

static bool
isNodeUnique(XMLNodeHdr node, XMLScan scan)
{
	XMLNodeOffset nodeOff = XNODE_OFFSET(node, scan->document);

	if (scan->resTmp != NULL)
	{
		XMLNodeContainer cont = scan->resTmp;
		unsigned short i;

		for (i = 0; i < cont->position; i++)
		{
			if (cont->items[i] == nodeOff)
			{
				return false;
			}
		}
		return true;
	}
	else
	{
		return true;
	}
}

static void
rememberResult(XMLNodeHdr node, XMLScan scan)
{
	if (scan->resTmp != NULL)
	{
		xmlnodePush(scan->resTmp, XNODE_OFFSET(node, scan->document));
	}
}

static void
dumpNodeDebug(StringInfo output, char *data, XMLNodeOffset rootOff, unsigned short level)
{
	XMLCompNodeHdr root = (XMLCompNodeHdr) (data + rootOff);
	char	   *refPtr = XNODE_FIRST_REF(root);
	unsigned short i;
	unsigned short bwidth = XNODE_GET_REF_BWIDTH(root);

	if (level == 0)
	{
		appendStringInfoString(output, "node:\n");
	}
	for (i = 0; i < root->children; i++)
	{
		XMLNodeOffset offRel = readXMLNodeOffset(&refPtr, bwidth, true);
		XMLNodeHdr	child = (XMLNodeHdr) ((char *) root - offRel);
		XMLNodeOffset offAbs = (char *) child - data;

		appendStringInfoSpaces(output, level);

		switch (child->kind)
		{
			case XMLNODE_ELEMENT:
				appendStringInfo(output, "%s (%u / %u)\n", XNODE_ELEMENT_NAME((XMLCompNodeHdr) child), offRel,
								 offAbs);
				if (((XMLCompNodeHdr) child)->children > 0)
				{
					dumpNodeDebug(output, data, (char *) child - data, level + 1);
				}
				break;

			case XMLNODE_ATTRIBUTE:
				appendStringInfo(output, "@%s (%u / %u)\n", XNODE_CONTENT(child), offRel, offAbs);
				break;

			default:
				appendStringInfoString(output, "...\n");
				break;
		}

	}
}

static void
dumpXScanDebug(StringInfo output, XMLScan scan, char *docData, XMLNodeOffset docRootOff)
{
	unsigned short i;
	XMLScanOneLevel level = scan->state;

	appendStringInfo(output, "xscan [dpth: %u, xpthroot: %u, xpthdpth: %u]\n", scan->depth,
					 scan->xpathRoot, scan->xpath->depth);
	for (i = 0; i <= scan->depth; i++)
	{
		XMLCompNodeHdr parent = level->parent;
		char	   *firstRefPtr = XNODE_FIRST_REF(parent);
		char		bwidth = XNODE_GET_REF_BWIDTH(parent);

		appendStringInfo(output, "level: %u, at: %u, sbl: %u, ", i,
						 (unsigned int) (((char *) level->nodeRefPtr - firstRefPtr) / bwidth), level->siblingsLeft);

		appendStringInfo(output, "parent[");
		if (i > 0)
		{
			appendStringInfo(output, "%u", (XMLNodeOffset) ((char *) parent - docData));
		}
		else
		{
			appendStringInfoString(output, scan->parent == NULL ? "document" : "upper scan target");
		}
		appendStringInfoString(output, "]\n");
		level++;
	}
}

/*
 * Sub-scans are used to search for descendants recursively.
 * It takes the current node as the root, so it in fact scans children of the current node.
 * If any of those children has children, new sub-scan is initiated, etc.
 */
static bool
considerSubScan(XPathElement xpEl, XMLNodeHdr node, XMLScan xscan)
{
	if (xpEl->descendant && node->kind == XMLNODE_ELEMENT && !xscan->descsDone)
	{
		XMLCompNodeHdr el = (XMLCompNodeHdr) node;

		if (el->children > 0)
		{
			xscan->subScan = (XMLScan) palloc(sizeof(XMLScanData));
			initXMLScan(xscan->subScan, xscan, xscan->xpath, xscan->xpathHeader, el, xscan->document,
						xscan->resTmp != NULL);
			xscan->subScan->xpathRoot = xscan->xpathRoot + xscan->depth;
			xscan->descsDone = false;
			return true;
		}
	}
	return false;
}
