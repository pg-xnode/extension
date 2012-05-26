/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"
#include "mb/pg_wchar.h"

#include "xmlnode.h"
#include "xpath.h"

#ifndef XML_PARSER_H
#define XML_PARSER_H

#define XNODE_PARSER_LIST_CHUNK		8

/*
 * This constant is used to estimate how much additional space we need for
 * the binary value, If this value is N, then we allocate strlen(inputText) +
 * 1 / (2^N * strlent(inputText)) for the binary value.
 *
 * This may be implemented as a configuration parameter in future versions.
 */
#define XNODE_OUT_OVERHEAD_BITS		3

/*
 * If changing this,  reflect value of XNODE_OUT_OVERHEAD_BITS.
 *
 * For example it makes little sense to set this to 32 while
 * XNODE_OUT_OVERHEAD_BITS is equal to 3. In such a case, xmlnodeInitParser()
 * function will only add 4 bytes to the input document length when computing
 * output space needed. However, with such small documents the binary size is
 * multiple times bigger than the source text.
 */

#define XNODE_PARSER_OUTPUT_MIN		256

#define XNODE_SPEC_STR_MAX_LEN		16
#define XNODE_DTD_ATTR_TYPES		8
#define XNODE_XDECL_MAX_ATTRS		3
#define XNODE_XDECL_VERSIONS		1

typedef enum XNodeSpecString
{
	XNODE_STR_XDECL_START = 0,
	XNODE_STR_XDECL_END,
	XNODE_STR_DTD_START,
	XNODE_STR_DTD_END,
	XNODE_STR_CMT_START,
	XNODE_STR_CMT_END,
	XNODE_STR_CDATA_START,
	XNODE_STR_CDATA_END,
	XNODE_STR_PI_START,
	XNODE_STR_PI_END
} XNodeSpecString;

typedef enum XNodeSpecStringDTD
{
	XNODE_STR_DTD_ELEMENT = 0,
	XNODE_STR_DTD_ATTLIST,
	XNODE_STR_DTD_ENTITY,
	XNODE_STR_DTD_NOTATION,
	XNODE_STR_DTD_SYSTEM,
	XNODE_STR_DTD_PUBLIC,
	XNODE_STR_DTD_EMPTY,
	XNODE_STR_DTD_ANY,
	XNODE_STR_DTD_PCDATA,
	XNODE_STR_DTD_NDATA,
	XNODE_STR_DTD_REQUIRED,
	XNODE_STR_DTD_IMPLIED,
	XNODE_STR_DTD_FIXED
}	XNodeSpecStringDTD;

typedef enum XNodeXDeclAttNames
{
	XNODE_XDECL_ATNAME_VERSION = 0,
	XNODE_XDECL_ATNAME_ENCODING = 1,
	XNODE_XDECL_ATNAME_STANDALONE = 2
} XNodeXDeclAttNames;

/*
 * Only XNODE_STR_CDATA_END or XNODE_STR_CMT_END expected as 'i'
 */
#define XNODE_SPEC_TEXT_END(i)	((state->srcPos + 2 < state->sizeIn) && strncmp(state->c, specStrings[i],	strlen(specStrings[i])) == 0)


typedef struct XMLParserStateData
{
	/*
	 * Besides document/node parsing, the parser is sometimes used to check
	 * attribute value in a different context (e.g. when 'element constructor
	 * receives the attributes in array)
	 *
	 * 'attrValue' must be set to 'true' in such special cases.
	 */
	XMLNodeKind targetKind;

	char	   *inputText;
	unsigned int sizeIn;
	unsigned int srcPos,
				dstPos;
	unsigned int srcRow,
				srcRowIncr;
	unsigned int srcCol;

	char	   *c;
	int			cWidth;

	unsigned int nestLevel;

	/*
	 * The parsed document
	 */
	char	   *result,
			   *tree;
	unsigned int sizeOut;
	bool		saveHeader;
	XMLNodeContainerData stack;
	XMLDecl		decl;

	/*
	 * List of (non-defalut) namespace declarations, from the context (i.e.
	 * currently being parsed) node) node up to the document root.
	 */
	XMLNodeContainerData nmspDecl;

	/*
	 * Set to true if at least one descendant (element or attribute) of the
	 * root node uses namespace prefix.
	 */
	bool		nmspPrefix;
} XMLNodeParserStateData;

typedef struct XMLParserStateData *XMLParserState;

#define XNODE_INPUT_END(state)	(*(state)->c == '\0')

#define UNEXPECTED_CHARACTER elog(ERROR, "Unexpected character at row %u, column %u.",\
	state->srcRow, state->srcCol)

extern void initXMLParserState(XMLParserState state, char *inputText, XMLNodeKind targetKind);
extern void finalizeXMLParserState(XMLParserState state);

extern void xmlnodeParseDoc(XMLParserState state);
extern void xmlnodeParseNode(XMLParserState state);
extern void readXMLName(XMLParserState state, bool whitespace, bool checkColons, bool separate, unsigned int *firstColPos);
extern char *readXMLAttValue(XMLParserState state, bool output, bool *refs);
extern bool xmlAttrValueIsNumber(char *value);

extern void xmlnodeDumpNode(char *input, XMLNodeOffset nodeOff,
				char **output, unsigned int *pos);
extern char *dumpXMLDecl(XMLDecl decl);

#endif   /* XML_PARSER_H */
