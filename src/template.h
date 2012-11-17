/*
 * Copyright (C) 2012, Antonin Houska
 */

#ifndef TEMPLATE_H_
#define TEMPLATE_H_

#include "xml_parser.h"
#include "xnt.h"

typedef struct XMLTemplateHeaderData
{
	/*
	 * Number substitution nodes (or rather their offsets). These immediately
	 * follow the structure.
	 */
	unsigned short substNodesCount;

	/*
	 * Number of *unique* parameters, i.e. number of parameter names following
	 * the substitution node offsets;
	 */
	unsigned short paramCount;
} XMLTemplateHeaderData;

#define XNODE_ALIGNOF_TEMPL_HDR ALIGNOF_SHORT

typedef struct XMLTemplateHeaderData *XMLTemplateHeader;

typedef struct XMLParamNameSorted
{
	unsigned short order;
	char	   *name;
} XMLParamNameSorted;

/*
 * If attribute value contains xpath expressions, it's understood as a sequence of
 * tokens where each token is either a NULL-terminated string or XPathExpressionData.
 */
#define XMLTEMPL_ATTR_VALUE_MAX_TOKENS		16


extern void parseXMLTemplateNode(XMLParserState state,
	 XMLParserNodeInfo nodeInfo, int specialNodeKind, unsigned int attrCount,
					 XNodeListItem *attrOffsets, unsigned int nmspDecls,
					 unsigned int *attrCountNew, bool **specAttrsValid,
					 unsigned int *specAttrCount);
extern XMLParamNameSorted *getXMLTemplateParamNames(ArrayType *parNameArray,
					   unsigned int templateParamCount, char **templParNames,
						 unsigned short *paramMap);
extern XPathExprOperandValue getXMLTemplateParamValues(Datum row,
	   unsigned int templateParamCount, XPathExprState exprState, Oid fnOid);
extern XPathExpression substituteXMLTemplateParams(XPathExprState exprState,
			   XPathExpression expression, XPathExprOperandValue paramValues,
							unsigned short *paramMap);
extern char *preprocessXMLTemplateAttrValues(XNodeListItem *attrOffsets,
		 unsigned short attrCount, char *parserOutput, unsigned int *outSize,
								XMLNodeContainer paramNames);
extern char *dumpXMLAttrBinaryValue(char *binValue, char **paramNames,
				 XPathExprOperandValue paramValues, unsigned short *paramMap,
					   XPathExprState exprState);
extern XMLNodeHdr getNewXMLAttribute(char *name, uint8 flags, char *value,
				   char decideOnDelim, unsigned int *size);
extern XPathExpression getXPathExpressionForXMLTemplate(char *src,
						 unsigned int termFlags, XMLNodeContainer paramNames,
								 unsigned short *endPos);
extern char *getAttrValueForXMLTemplate(char *attrValue, unsigned int *valueSize,
						   XMLNodeContainer paramNames);

#endif   /* TEMPLATE_H_ */
