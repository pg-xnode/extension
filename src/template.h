/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#ifndef TEMPLATE_H_
#define TEMPLATE_H_

#include "postgres.h"

#include "access/htup.h"
#include "catalog/pg_cast.h"
#include "catalog/pg_proc.h"
#include "catalog/pg_type.h"
#include "utils/array.h"
#include "utils/builtins.h"
#include "utils/syscache.h"
#include "utils/lsyscache.h"
#include "utils/typcache.h"
#if (PG_VERSION_NUM >= 90300)
#include "access/htup_details.h"
#endif

#include "xml_parser.h"

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

extern XMLTemplateHeader getXMLTemplateHeader(XMLCompNodeHdr docRoot);
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
						   XMLNodeContainer paramNames, bool acceptLocPaths);
extern char *dumpXMLAttrBinaryValue(char *binValue, char **paramNames,
				 XPathExprOperandValue paramValues, unsigned short *paramMap,
					   XPathExprState exprState);
extern XMLNodeHdr getNewXMLAttribute(char *name, uint8 flags, char *value,
				   char decideOnDelim, unsigned int *size);
extern XPathHeader getXPathExpressionForXMLTemplate(char *src,
						 unsigned int termFlags, XMLNodeContainer paramNames,
		unsigned short *endPos, unsigned short *outSize, bool accepLcoPaths);
extern char *getAttrValueForXMLTemplate(char *attrValue, unsigned short *valueSize,
						   XMLNodeContainer paramNames, bool acceptLocPaths);

#endif   /* TEMPLATE_H_ */
