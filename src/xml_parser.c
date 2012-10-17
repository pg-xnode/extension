/*
 * This aims to be compliant with http://www.w3.org/TR/2008/REC-xml-20081126/
 * some time.
 *
 * The parser only scans the input string once. As the total size of children
 * needs to be known before we know where the current node can be stored
 * itself, the parser first saves child nodes.
 *
 * This 'child first' order prevents us from having to copy nodes during
 * parsing. Some functions, such as getFirstLeaf() do rely on such ordering.
 *
 * Copyright (C) 2012, Antonin Houska
 */

/*
 * TODO error handling: use 'ereport()' and define numeric error codes.
 */

#include "xml_parser.h"
#include "xmlnode_util.h"
#include "xnt.h"

/*
 * Input tokens, for internal use only.
 *
 * TOKEN_WHITESPACE only applies to white spaces at the top level. In tag
 * content a white space is always considered to be TOKEN_TEXT.
 * TOKEN_WHITESPACE and TOKEN_TEXT are mutually exclusive.
 */
typedef enum XMLNodeToken
{
	TOKEN_XMLDECL = (1 << 0),
	TOKEN_DTD = (1 << 1),
	TOKEN_STAG = (1 << 2),
	TOKEN_ETAG = (1 << 3),
	TOKEN_EMPTY_ELEMENT = (1 << 4),
	TOKEN_COMMENT = (1 << 5),
	TOKEN_CDATA = (1 << 6),
	TOKEN_PI = (1 << 7),
	TOKEN_WHITESPACE = (1 << 8),
	TOKEN_TEXT = (1 << 9),
	TOKEN_REFERENCE = (1 << 10),
	TOKEN_MISC = TOKEN_COMMENT | TOKEN_PI | TOKEN_WHITESPACE,
} XMLNodeToken;

/*
 * Order of these strings must follow the order of items enumerated in
 * 'XNodeSpecString'
 */
char		specXMLStrings[][XNODE_SPEC_STR_MAX_LEN] =
{
	"<?xml", "?>",
	"<!DOCTYPE", ">",
	"<!--", "-->",
	"<![CDATA[", "]]>",
	"<?", "?>",
};

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Char
 */
UTF8Interval charIntervals[CHAR_INTERVALS] =
{
	{{0x21, 0x0, 0x0, 0x0}, {0xed, 0x9f, 0xbf, 0x0}},
	{{0xee, 0x80, 0x80, 0x0}, {0xef, 0xbf, 0xbd, 0x0}},
	{{0xf0, 0x90, 0x80, 0x80}, {0xf4, 0x8f, 0xbf, 0xbf}}
};


static const char *xmldeclAttNames[XNODE_XDECL_MAX_ATTRS] = {
	"version", "encoding", "standalone"
};

static const char *xmldeclVersions[XNODE_XDECL_VERSIONS] = {
	"1.0"
};

#define XMLDECL_STANDALONE_YES	"yes"

/*
 * Parser functions use this structure for output information about nodes.
 * Mostly, where the data can be find in the input text when being saved to
 * the output array.
 */
typedef struct XMLNodeInternalData
{
	/*
	 * Where the node starts in 'state.tree' (varlena header size not
	 * included). Used to return the output position to caller of
	 * processToken().
	 */
	XMLNodeOffset nodeOut;

	/*
	 * Where content starts, relative to 'XMLParserStateData.inputText', and
	 * how many bytes (not MB characters) it takes. For STag, ETag and
	 * EmptyElement 'content' means tag name.
	 */
	XMLNodeOffset cntSrc;
	unsigned int cntLength;

	bool		entPredef;
	bool		headerSaved;

	XMLNodeToken tokenType;

	/*
	 * Greater than zero if the node name has namespace prefix. Only important
	 * for TOKEN_STAG or TOKEN_EMPTY_ELEMENT
	 */
	unsigned int nmspLength;
} XMLNodeInternalData;

typedef struct XMLNodeInternalData *XMLNodeInternal;

static void processToken(XMLParserState state, XMLNodeInternal nodeInfo, XMLNodeToken allowed);
static void forgetNamespaceDeclarations(unsigned int count, XMLNodeContainer stack);
static XMLNodeToken processTag(XMLParserState state, XMLNodeInternal nodeInfo, XMLNodeToken allowed,
		   XMLNodeHdr *declAttrs, unsigned short *declAttrNum, unsigned int *nmspDecls, unsigned int *attrsPrefixed);
static void checkXMLDeclaration(XMLNodeHdr *declAttrs, unsigned int attCount, XMLDecl decl);
static char *getEncodingSimplified(const char *original);
static bool isPredefinedEntity(char *refStart, char *value);

static void evaluateWhitespace(XMLParserState state);
static void ensureSpaceIn(unsigned int size, XMLParserState state);
static unsigned int saveNodeHeader(XMLParserState state, XMLNodeInternal nodeInfo, char flags);
static void saveContent(XMLParserState state, XMLNodeInternal nodeInfo);
static void saveReferences(XMLParserState state, XMLNodeInternal nodeInfo, XMLCompNodeHdr compNode,
  unsigned short children, bool *specAttrsValid, unsigned int specAttrCount);
static char *getContentToLog(char *input, unsigned int offset, unsigned int length, unsigned int maxLen);
static void saveRootNodeHeader(XMLParserState state, XMLNodeKind kind);

static void checkNamespaces(XMLParserState state, XMLNodeInternal nodeInfo, unsigned int attrsPrefixedCount,
				bool *elNmspIsSpecial);
static unsigned int dumpAttributes(XMLCompNodeHdr element, StringInfo output,
			   char **paramNames);
static void dumpContentEscaped(XMLNodeKind kind, StringInfo output, char *input, unsigned int inputLen);
static void dumpSpecString(StringInfo output, char *outNew, unsigned int *incrInput);
static char *ensureSpaceOut(StringInfo output, unsigned int toWrite);

typedef struct PredefinedEntity
{
	char	   *escaped;
	char		simple;
}	PredefinedEntity;

#define XNODE_PREDEFINED_ENTITIES	5

#define XNODE_CHAR_CDATA_LT		"lt;"
#define XNODE_CHAR_CDATA_GT		"gt;"
#define XNODE_CHAR_CDATA_AMP	"amp;"

static PredefinedEntity predefEntities[XNODE_PREDEFINED_ENTITIES] = {
	{XNODE_CHAR_CDATA_LT, XNODE_CHAR_LARROW},
	{XNODE_CHAR_CDATA_GT, XNODE_CHAR_RARROW},
	{XNODE_CHAR_CDATA_AMP, XNODE_CHAR_AMPERSAND},
	{"apos;", XNODE_CHAR_APOSTR},
	{"quot;", XNODE_CHAR_QUOTMARK}
};

void
xmlnodeParseDoc(XMLParserState state)
{
	unsigned int tagRow,
				tagCol;
	XMLNodeInternalData nodeInfo;

	/*
	 * Expecting either http://www.w3.org/TR/2008/REC-xml-20081126/#NT-prolog
	 * or http://www.w3.org/TR/2008/REC-xml-20081126/#NT-element
	 */
	tagRow = tagCol = 1;
	processToken(state, &nodeInfo, TOKEN_XMLDECL | TOKEN_MISC | TOKEN_DTD
				 | TOKEN_STAG);

	if (nodeInfo.tokenType != TOKEN_WHITESPACE && nodeInfo.tokenType != TOKEN_XMLDECL)
	{
		xmlnodePushSingleNode(&(state->stack), nodeInfo.nodeOut);
	}
	if ((nodeInfo.tokenType == TOKEN_XMLDECL) || ((nodeInfo.tokenType
												   & TOKEN_MISC) != 0))
	{
		do
		{
			nextXMLChar(state, false);
			tagRow = state->srcRow;
			tagCol = state->srcCol;
			processToken(state, &nodeInfo, TOKEN_MISC | TOKEN_DTD | TOKEN_STAG);
			if (nodeInfo.tokenType != TOKEN_WHITESPACE)
			{
				xmlnodePushSingleNode(&(state->stack), nodeInfo.nodeOut);
			}
		} while ((nodeInfo.tokenType & TOKEN_MISC) != 0);
	}
	if (nodeInfo.tokenType == TOKEN_DTD)
	{
		do
		{
			nextXMLChar(state, false);
			tagRow = state->srcRow;
			tagCol = state->srcCol;
			processToken(state, &nodeInfo, TOKEN_MISC | TOKEN_DTD | TOKEN_STAG);
			if (nodeInfo.tokenType != TOKEN_WHITESPACE)
			{
				xmlnodePushSingleNode(&(state->stack), nodeInfo.nodeOut);
			}
		} while ((nodeInfo.tokenType & TOKEN_MISC) != 0);
	}

	/*
	 * Either the root element has just been processed or an invalid
	 * tag/character found.
	 */
	if ((nodeInfo.tokenType != TOKEN_ETAG) && (nodeInfo.tokenType
											   != TOKEN_EMPTY_ELEMENT))
	{
		elog(ERROR, "Root element expected at row %u, column %u.", tagRow, tagCol);
	}

	/*
	 * Now that the root element is processed, document end is legal.
	 */
	nextXMLChar(state, true);
	if (XNODE_INPUT_END(state))
	{
		saveRootNodeHeader(state, state->targetKind);
		return;
	}

	/*
	 * Expecting Misc*, see
	 * http://www.w3.org/TR/2008/REC-xml-20081126/#sec-documents
	 */
	do
	{
		tagRow = state->srcRow;
		tagCol = state->srcCol;
		processToken(state, &nodeInfo, TOKEN_MISC);
		if ((nodeInfo.tokenType & TOKEN_MISC) == 0)
		{
			elog(ERROR, "Unexpected character or tag on row %u, column %u.",
				 tagRow, tagCol);
		}
		if (nodeInfo.tokenType != TOKEN_WHITESPACE)
		{
			xmlnodePushSingleNode(&(state->stack), nodeInfo.nodeOut);
		}
		nextXMLChar(state, true);
	} while (!XNODE_INPUT_END(state));

	saveRootNodeHeader(state, state->targetKind);
}

void
xmlnodeParseNode(XMLParserState state)
{
	XMLNodeInternalData nodeInfo;
	bool		entPredef = false;
	unsigned int maskAll = TOKEN_DTD | TOKEN_STAG | TOKEN_EMPTY_ELEMENT | TOKEN_COMMENT |
	TOKEN_CDATA | TOKEN_PI | TOKEN_TEXT | TOKEN_REFERENCE;

	nodeInfo.entPredef = false;
	processToken(state, &nodeInfo, maskAll);
	xmlnodePushSingleNode(&state->stack, nodeInfo.nodeOut);
	nextXMLChar(state, true);

	while (!XNODE_INPUT_END(state))
	{
		/*
		 * TOKEN_TEXT and TOKEN_REFERENCE deserve special treatment: if
		 * there's a sequence of characters and/or entity references, it's all
		 * concatenated into a single text node. If at least one entity
		 * reference appears in that sequence, XNODE_TEXT_CONTAINS_REF flag is
		 * set;
		 */
		if (nodeInfo.tokenType & (TOKEN_TEXT | TOKEN_REFERENCE))
		{
			XMLNodeOffset textStart = nodeInfo.nodeOut;

			entPredef = false;
			if (nodeInfo.tokenType == TOKEN_REFERENCE && nodeInfo.entPredef)
			{
				entPredef = true;
			}
			state->saveHeader = false;

			/*
			 * Continue until the sequence of characters / entity references
			 * ends or the node / node fragment does.
			 */
			do
			{
				/*
				 * xmlnodePush() is not called in this loop because the whole
				 * sequence of characters / references will constitute for a
				 * single text node.
				 */
				nodeInfo.entPredef = false;
				processToken(state, &nodeInfo, maskAll);
				if (nodeInfo.tokenType == TOKEN_REFERENCE && nodeInfo.entPredef && !entPredef)
				{
					entPredef = true;
				}
				nextXMLChar(state, true);
			} while (!XNODE_INPUT_END(state) &&
					 (nodeInfo.tokenType & (TOKEN_TEXT | TOKEN_REFERENCE)));

			/*
			 * Ordinary node (no char or reference), treat it as a separate
			 * node. We might be at the end, but don't have to.
			 */
			if (!(nodeInfo.tokenType & (TOKEN_TEXT | TOKEN_REFERENCE)))
			{
				xmlnodePushSingleNode(&state->stack, nodeInfo.nodeOut);
			}
			if (!XNODE_INPUT_END(state))
			{
				state->saveHeader = true;
			}
			if (entPredef)
			{
				XMLNodeHdr	node = (XMLNodeHdr) (state->tree + textStart);

				node->flags |= XNODE_TEXT_SPEC_CHARS;
				entPredef = false;
			}
		}
		else
		{
			/*
			 * Process any other node type.
			 */
			processToken(state, &nodeInfo, maskAll);
			xmlnodePushSingleNode(&state->stack, nodeInfo.nodeOut);
			nextXMLChar(state, true);

			/*
			 * 'stand-alone' entity reference (i.e. neither preceded nor
			 * followed by any characters or other references) at the end of
			 * the node or node fragment.
			 */
			if (XNODE_INPUT_END(state) && nodeInfo.tokenType == TOKEN_REFERENCE && nodeInfo.entPredef)
			{
				entPredef = true;
			}
		}
	}

	/*
	 * Either the node (fragment) ends with a 'stand-alone' reference (the
	 * 'else' branch above) or such a reference is the only character in the
	 * input text.
	 */
	if (entPredef || (state->stack.position == 1 && nodeInfo.entPredef))
	{
		XMLNodeHdr	node = (XMLNodeHdr) (state->tree + nodeInfo.nodeOut);

		node->flags |= XNODE_TEXT_SPEC_CHARS;
	}
	if (state->stack.position == 1)
	{
		XMLNodeOffset *rootOffPtr;
		char	   *ptrUnaligned;
		unsigned int padding;

		ptrUnaligned = state->tree + state->dstPos;
		rootOffPtr = (XMLNodeOffset *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, ptrUnaligned);
		padding = (char *) rootOffPtr - ptrUnaligned;
		ensureSpaceIn(padding + sizeof(XMLNodeOffset), state);

		*rootOffPtr = xmlnodePopOffset(&state->stack);
		state->dstPos += padding + sizeof(XMLNodeOffset);
		SET_VARSIZE(state->result, state->dstPos + VARHDRSZ);
	}
	else
	{
		saveRootNodeHeader(state, XMLNODE_DOC_FRAGMENT);
	}
}

void
initXMLParserState(XMLParserState state, char *inputText, XMLNodeKind targetKind, GetSpecialXNodeKindFunc checkFunc)
{
	unsigned int hdrSize = (targetKind == XMLNODE_ATTRIBUTE || targetKind == XMLNODE_ELEMENT) ? 0 : VARHDRSZ;

	state->targetKind = targetKind;
	state->srcPos = 0;
	state->srcRow = 1;
	state->srcCol = 1;
	state->inputText = state->c = inputText;
	state->sizeIn = strlen(inputText);

	if (!XNODE_WHITESPACE(state->c))
	{
		state->cWidth = pg_utf_mblen((unsigned char *) state->c);
		state->srcRowIncr = 0;
	}
	else
	{
		evaluateWhitespace(state);
	}

	Assert(sizeof(XMLNodeOffset) < XNODE_PARSER_OUTPUT_MIN);

	state->dstPos = 0;
	state->nestLevel = 0;
	state->saveHeader = true;

	if (state->targetKind == XMLNODE_ATTRIBUTE || state->targetKind == XMLNODE_ELEMENT)
	{
		state->sizeOut = 32;
	}
	else
	{

		state->sizeOut = state->sizeIn + (state->sizeIn >> XNODE_OUT_OVERHEAD_BITS);
		state->sizeOut = (state->sizeOut > XNODE_PARSER_OUTPUT_MIN) ? state->sizeOut
			: XNODE_PARSER_OUTPUT_MIN;
		/* state->sizeOut = 16384; */
		elog(DEBUG1, "source xml size: %u, binary xml size (initial estimate): %u", state->sizeIn,
			 state->sizeOut);
	}

	/*
	 * Output storage is never needed if we're only checking element name
	 * passed as an argument to xml.element() function.
	 */
	if (state->targetKind != XMLNODE_ELEMENT)
	{
		state->result = (char *) palloc(state->sizeOut + hdrSize);
		state->tree = state->result + hdrSize;
	}

	if (state->targetKind != XMLNODE_ATTRIBUTE && state->targetKind != XMLNODE_ELEMENT)
	{
		xmlnodeContainerInit(&state->stack);
		xmlnodeContainerInit(&state->nmspDecl);
	}

	state->decl = NULL;
	state->nmspPrefix = false;
	if (targetKind == XNTNODE_ROOT)
	{
		state->nmspSpecialName = XNTNODE_NAMESPACE_PREFIX;
		state->nmspSpecialValue = XNTNODE_NAMESPACE_VALUE;
		state->getXNodeKindFunc = checkFunc;
		xmlnodeContainerInit(&state->paramNames);
		xmlnodeContainerInit(&state->substNodes);
	}
	else
	{
		state->nmspSpecialName = NULL;
		state->nmspSpecialValue = NULL;
		state->getXNodeKindFunc = NULL;
	}
}

void
finalizeXMLParserState(XMLParserState state)
{
	if (state->targetKind != XMLNODE_ATTRIBUTE && state->targetKind != XMLNODE_ELEMENT)
	{
		xmlnodeContainerFree(&state->stack);

		Assert(state->nmspDecl.position == 0);
		xmlnodeContainerFree(&state->nmspDecl);
	}
	if (state->decl != NULL)
	{
		pfree(state->decl);
		state->decl = NULL;
	}

	if (state->targetKind == XNTNODE_ROOT)
	{
		xmlnodeContainerFree(&state->paramNames);
		xmlnodeContainerFree(&state->substNodes);
	}
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Name
 *
 * 'whitespace' indicates whether a whitespace is expected right after the name.
 *
 * '*firstColPos' receives position of the first colon (relative to the first character of the name).
 * If there's no colon in the name or if the colon is the first character, 0 is returned.
 */
void
readXMLName(XMLParserState state, bool whitespace, bool checkColons, bool separate, unsigned int *firstColPos)
{
	unsigned int colons = 0;
	unsigned int posInit = state->srcPos;
	char	   *firstChar = state->c;

	if (*state->c == '\0')
	{
		elog(ERROR, "expected xml name, found end of string.");
	}
	if (!XNODE_VALID_NAME_START(state->c))
	{
		elog(ERROR, "unrecognized leading character or xml name: '%c'", *state->c);
	}

	if (checkColons)
	{
		if (firstColPos != NULL)
		{
			*firstColPos = 0;
		}
		if (*firstChar == XNODE_CHAR_COLON)
		{
			colons = 1;
		}
	}

	do
	{
		nextXMLChar(state, separate);
		if (checkColons && *state->c == XNODE_CHAR_COLON)
		{
			/*
			 * If this is the first colon, set output argument 'firstColPos'
			 * to its position.
			 */
			if (checkColons && colons == 0 && firstColPos != NULL)
			{
				*firstColPos = state->srcPos - posInit;
			}

			if (*(state->c - 1) == XNODE_CHAR_COLON)
			{
				elog(ERROR, "2 consecutive colons not allowed in xml name");
			}
			colons++;
		}
	} while (XNODE_VALID_NAME_CHAR(state->c));

	if (colons >= 2)
	{
		elog(ERROR, "incorrect usage of namespace prefix (too many colons)");
	}

	/* Colon is valid leading character but alone would mean empty name. */
	if ((state->c - firstChar) == 1 && *firstChar == XNODE_CHAR_COLON)
	{
		elog(ERROR, "colon is not allowed as a name");
	}

	if (whitespace)
	{
		if (!XNODE_WHITESPACE(state->c))
		{
			elog(ERROR, "whitespace expected at row %u, column %u.", state->srcRow, state->srcCol);
		}
		else
		{
			nextXMLChar(state, separate);
		}
	}
}

bool
isValidXMLName(char *str)
{
	if (!XNODE_VALID_NAME_START(str))
	{
		return false;
	}

	str += pg_utf_mblen((unsigned char *) str);

	while (*str != '\0')
	{
		if (!XNODE_VALID_NAME_CHAR(str))
		{
			return false;
		}
		str += pg_utf_mblen((unsigned char *) str);
	}
	return true;
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-AttValue
 *
 * 'output'  - whether the attribute should be written to output or not.
 *	Sometimes we just need to check the value.
 *
 *	'refs' - gets set to true if the attribute contains at least one reference.
 *
 * returns attribute value (NULL-terminated)
 */
char *
readXMLAttValue(XMLParserState state, bool output, bool *refs)
{
	char		term;
	char		delimFirst = '\0';
	unsigned int pos = state->dstPos;

	*refs = false;

	if (state->targetKind != XMLNODE_ATTRIBUTE)
	{
		if (*state->c != XNODE_CHAR_APOSTR && *state->c != XNODE_CHAR_QUOTMARK)
		{
			elog(ERROR, "quotation mark or apostrophe expected at row %u, column %u.",
				 state->srcRow, state->srcCol);
		}
		term = *state->c;
		nextXMLChar(state, false);
	}
	else
	{
		term = '\0';
	}

	while (*state->c != term)
	{
		if (!XNODE_VALID_CHAR(state->c))
		{
			if (state->targetKind == XMLNODE_ATTRIBUTE)
			{
				elog(ERROR, "invalid XML character in attribute value");
			}
			else
			{
				elog(ERROR, "invalid XML character at row %u, column %u",
					 state->srcRow, state->srcCol);
			}
		}

		if (*state->c == XNODE_CHAR_LARROW)
		{
			if (state->targetKind == XMLNODE_ATTRIBUTE)
			{
				elog(ERROR, "invalid XML character in attribute value");
			}
			else
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (*state->c == XNODE_CHAR_AMPERSAND)
		{
			pg_wchar	value;
			char	   *refStart = state->c;

			*refs = true;
			if (readXMLReference(state, &value))
			{
				char		utf8char[5];

				memset(utf8char, 0, 5);
				unicode_to_utf8(value, (unsigned char *) utf8char);
				if (!XNODE_VALID_CHAR(utf8char))
				{
					if (state->targetKind == XMLNODE_ATTRIBUTE)
					{
						elog(ERROR, "invalid XML character reference in attribute value");
					}
					else
					{
						elog(ERROR, "invalid XML character reference at row %u, column %u", state->srcRow,
							 state->srcCol);
					}
				}
				if (output)
				{
					unsigned int len = strlen(utf8char);

					ensureSpaceIn(len, state);
					memcpy(state->tree + state->dstPos, utf8char, len);
					state->dstPos += len;
				}
			}
			else
			{
				char		predefValue;

				if (!isPredefinedEntity(refStart + 1, &predefValue))
				{
					elog(ERROR, "this parser only supports character and predefined references");
				}
				if (output)
				{
					ensureSpaceIn(1, state);
					*(state->tree + state->dstPos) = predefValue;
					state->dstPos++;
				}
			}
		}
		else
		{
			/*
			 * If attribute value is passed in an array, then the array
			 * literal delimiter is not considered the value delimiter. We
			 * need to decide whether the value is delimited by quotation mark
			 * or by apostrophe.
			 */
			if ((state->targetKind == XMLNODE_ATTRIBUTE) &&
				(*state->c == XNODE_CHAR_APOSTR || *state->c == XNODE_CHAR_QUOTMARK))
			{
				if (delimFirst == '\0')
				{
					delimFirst = *state->c;
				}
				else if (delimFirst != *state->c)
				{
					elog(ERROR, "attribute value may contain either quotation marks or apostrophes but not both");
				}
			}
			/* 'Ordinary' character, just write it (if the caller wants it). */
			if (output)
			{
				ensureSpaceIn(state->cWidth, state);
				memcpy(state->tree + state->dstPos, state->c, state->cWidth);
				state->dstPos += state->cWidth;
			}
		}

		nextXMLChar(state, state->targetKind == XMLNODE_ATTRIBUTE);
	}

	if (output)
	{
		ensureSpaceIn(1, state);
		*(state->tree + state->dstPos) = '\0';
		state->dstPos++;
	}
	return (state->targetKind == XMLNODE_ATTRIBUTE) ? state->result : state->tree + pos;
}

bool
xmlAttrValueIsNumber(char *value)
{
	char	   *end;

	strtod(value, &end);
	while (XNODE_WHITESPACE(end) && *end != '\0')
	{
		end++;
	}
	return (*end == '\0');
}

uint8
getXMLAttributeFlags(char *attrValue, bool refs, bool quotApostr)
{
	uint8		flags = 0;

	if (refs)
	{
		flags |= XNODE_ATTR_CONTAINS_REF;
	}
	if (quotApostr)
	{
		flags |= XNODE_ATTR_APOSTROPHE;
	}
	if (strlen(attrValue) > 0 && xmlAttrValueIsNumber(attrValue))
	{
		flags |= XNODE_ATTR_NUMBER;
	}
	return flags;
}

/*
 * The state variables are incremented by values retrieved during the
 * previous call. The reason is that sometimes we need to have the current
 * character width at hand (in order to check the next character).
 */
void
nextXMLChar(XMLParserState state, bool endAllowed)
{
	state->srcPos += state->cWidth;
	state->c += state->cWidth;
	state->srcRow += state->srcRowIncr;
	if (state->srcRowIncr == 0)
	{
		state->srcCol++;
	}
	else
	{
		state->srcCol = 0;
	}

	if (XNODE_INPUT_END(state))
	{
		if (!endAllowed)
		{
			elog(ERROR, "Unexpected end of XML document");
		}
		else
		{
			return;
		}
	}
	if (XNODE_WHITESPACE(state->c))
	{
		evaluateWhitespace(state);
	}
	else
	{
		state->cWidth = pg_utf_mblen((unsigned char *) state->c);
		state->srcRowIncr = 0;
	}
}

unsigned int
readXMLPI(XMLParserState state)
{
	unsigned int len,
				i;
	unsigned int startPos = state->srcPos;
	char	   *targName = state->c;

	if (!XNODE_VALID_NAME_START(state->c))
	{
		UNEXPECTED_CHARACTER;
	}
	while (XNODE_VALID_NAME_CHAR(state->c))
	{
		nextXMLChar(state, false);
	}
	if (state->srcPos - startPos >= 3)
	{
		char	   *targNameRef = specXMLStrings[XNODE_STR_XDECL_START] + 2;
		bool		found = true;

		for (i = 0; i < 3; i++)
		{
			if (*targName != *targNameRef && *targName != toupper(*targNameRef))
			{
				found = false;
				break;
			}
			else
			{
				targName++;
				targNameRef++;
			}
		}
		if (found && XNODE_WHITESPACE(targName))
		{
			elog(ERROR, "reserved PI target name found on row %u, column %u",
				 state->srcRow, state->srcCol);
		}
	}
	if (XNODE_WHITESPACE(state->c))
	{
		nextXMLChar(state, false);
		while (*state->c != XNODE_CHAR_QUESTMARK)
		{
			if (!XNODE_VALID_CHAR(state->c))
			{
				elog(ERROR, "Invalid XML character at row %u, column %u",
					 state->srcRow, state->srcCol);
			}
			nextXMLChar(state, false);
		}
	}
	len = state->srcPos - startPos;
	if (*state->c != XNODE_CHAR_QUESTMARK)
	{
		UNEXPECTED_CHARACTER;
	}
	nextXMLChar(state, false);
	if (*state->c != XNODE_CHAR_RARROW)
	{
		UNEXPECTED_CHARACTER;
	}
	return len;
}

/*
 * 'offset' - how many characters of the (possible) special string have been
 * processed before this function was called.
 */
bool
readSpecialXMLStringPart(char specXMLStrings[][XNODE_SPEC_STR_MAX_LEN],
				 XNodeSpecString strIndex, XMLParserState state, char offset)
{
	char	   *specString = specXMLStrings[strIndex] + offset;
	unsigned int len = strlen(specString);

	if (state->srcPos + len <= state->sizeIn &&
		strncmp(state->c, specString, len) == 0)
	{

		unsigned int i;

		for (i = 0; i < len; i++)
		{
			nextXMLChar(state, false);
		}
		return true;
	}
	else
	{
		return false;
	}
}

bool
readSpecialXMLString(char specStrings[][XNODE_SPEC_STR_MAX_LEN], XNodeSpecString strIndex,
					 XMLParserState state)
{
	return readSpecialXMLStringPart(specStrings, strIndex, state, 0);
}

void
readXMLWhitespace(XMLParserState state, bool optional)
{
	if (XNODE_WHITESPACE(state->c))
	{
		nextXMLChar(state, false);
	}
	else if (!optional)
	{
		elog(ERROR, "whitespace expected at row %u, column %u", state->srcRow, state->srcCol);
	}
}

unsigned int
readXMLComment(XMLParserState state)
{
	unsigned int len;
	unsigned short int i;
	unsigned int startPos = state->srcPos;
	char		prev = 0x00;

	while (!XNODE_SPEC_TEXT_END(XNODE_STR_CMT_END))
	{
		if (!XNODE_VALID_CHAR(state->c))
		{
			elog(ERROR, "Invalid XML character at row %u, column %u", state->srcRow, state->srcCol);
		}
		prev = *state->c;
		nextXMLChar(state, false);
	}

	if (prev == XNODE_CHAR_DASH)
	{
		elog(ERROR, "Comment must not end with %c%s", XNODE_CHAR_DASH,
			 specXMLStrings[XNODE_STR_CMT_END]);
	}
	len = state->srcPos - startPos;

	/*
	 * The convention is to end up tag processing when '>' is the current
	 * character.
	 */
	for (i = 0; i < strlen(specXMLStrings[XNODE_STR_CDATA_END]) - 1; i++)
	{
		nextXMLChar(state, false);
	}
	return len;
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Reference
 *
 * Reads (entity or character) reference.
 * At return time 'state' points to terminating semicolon.
 */
bool
readXMLReference(XMLParserState state, pg_wchar *value)
{
	bool		charRef = false;

	nextXMLChar(state, false);
	if (*state->c == XNODE_CHAR_HASH)
	{
		/*
		 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-CharRef
		 */
		bool		hex = false;
		unsigned int digits = 0;
		char	   *valueStart;

		charRef = true;

		nextXMLChar(state, false);
		if (*state->c == 'x')
		{
			hex = true;
			nextXMLChar(state, false);
		}
		valueStart = state->c;
		while ((hex && isxdigit(*state->c)) || (!hex && isdigit(*state->c)))
		{
			nextXMLChar(state, false);
			digits++;
		}
		if (digits == 0)
		{
			if (state->targetKind == XMLNODE_ATTRIBUTE)
			{
				elog(ERROR, "decimal or hexadecimal value expected in reference");
			}
			else
			{
				elog(ERROR, "decimal or hexadecimal value expected at row %u, column %u.",
					 state->srcRow, state->srcCol);
			}
		}
		if (hex)
		{
			sscanf(valueStart, "%x", value);
		}
		else
		{
			sscanf(valueStart, "%d", value);
		}
	}
	else
	{
		/*
		 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-EntityRef
		 */
		readXMLName(state, false, false, false, NULL);
	}

	if (*state->c != XNODE_CHAR_SEMICOLON)
	{
		if (state->targetKind == XMLNODE_ATTRIBUTE)
		{
			elog(ERROR, "'%c' expected in reference", XNODE_CHAR_SEMICOLON);
		}
		else
		{
			elog(ERROR, "'%c' expected at row %u, column %u.", XNODE_CHAR_SEMICOLON,
				 state->srcRow, state->srcCol);
		}
	}
	return charRef;
}

/*
 * Returns the last token processed. In case we start at STag, ETag is
 * returned.
 *
 * allowed	- tokens that don't cause error
 */
static void
processToken(XMLParserState state, XMLNodeInternal nodeInfo, XMLNodeToken allowed)
{
	unsigned int tagRow,
				tagCol;

	nodeInfo->headerSaved = false;

	tagRow = state->srcRow;
	tagCol = state->srcCol;
	if (*state->c != XNODE_CHAR_LARROW)
	{
		char		next;

		nodeInfo->cntSrc = state->srcPos;
		nodeInfo->nodeOut = state->dstPos;
		if (*state->c == XNODE_CHAR_AMPERSAND)
		{
			unsigned int row = state->srcRow;
			unsigned int col = state->srcCol;
			char	   *start = state->c;
			pg_wchar	value;
			char		valueSingle;

			if (!(allowed & TOKEN_REFERENCE))
			{
				elog(ERROR, "reference not allowed at row %u, column %u.", state->srcRow,
					 state->srcCol);
			}
			nodeInfo->tokenType = TOKEN_REFERENCE;
			if (readXMLReference(state, &value))
			{
				char		refChar[5];

				memset(refChar, 0, 5);
				unicode_to_utf8(value, (unsigned char *) refChar);
				if (!XNODE_VALID_CHAR(refChar))
				{
					elog(ERROR, "Invalid character reference at row %u, column %u", row, col);
				}
				nodeInfo->entPredef = false;
				nodeInfo->cntLength = strlen((char *) refChar);
				if (state->saveHeader)
				{
					saveNodeHeader(state, nodeInfo, 0);
				}
				else
				{
					/*
					 * Overwrite terminating NULL character of the previous
					 * text/reference node.
					 */
					state->dstPos--;
				}
				ensureSpaceIn(nodeInfo->cntLength + 1, state);
				memcpy(state->tree + state->dstPos, refChar, nodeInfo->cntLength);
			}
			else if (isPredefinedEntity(start + 1, &valueSingle))
			{
				nodeInfo->entPredef = true;
				nodeInfo->cntLength = 1;
				if (state->saveHeader)
				{
					saveNodeHeader(state, nodeInfo, 0);
				}
				else
				{
					state->dstPos--;
				}
				ensureSpaceIn(nodeInfo->cntLength + 1, state);
				*(state->tree + state->dstPos) = valueSingle;
			}
			else
			{
				elog(ERROR, "this parser version only supports character and predefined references");
			}
			state->dstPos += nodeInfo->cntLength;
			*(state->tree + state->dstPos) = 0x00;
			state->dstPos++;
			return;
		}
		else if (allowed & TOKEN_TEXT)
		{
			/*
			 * http://www.w3.org/TR/xml/#NT-CharData
			 */

			/*
			 * First character of the next tag terminates the content, so we
			 * need to check one character ahead:
			 */
			while (
			   ((next = *(state->c + state->cWidth)) != XNODE_CHAR_LARROW) &&
				   next != XNODE_CHAR_AMPERSAND && next != '\0'
				)
			{
				if (*state->c == XNODE_CHAR_RBRACKET)
				{
					if (XNODE_SPEC_TEXT_END(XNODE_STR_CDATA_END))
					{
						elog(ERROR, "Sequence '%s' not allowed in character data.",
							 specXMLStrings[XNODE_STR_CDATA_END]);
					}
				}
				if (!XNODE_VALID_CHAR(state->c))
				{
					elog(ERROR, "invalid XML character at row %u, column %u", state->srcRow, state->srcCol);
				}
				nextXMLChar(state, false);
			}
			nodeInfo->cntLength = state->srcPos - nodeInfo->cntSrc + state->cWidth;
			nodeInfo->tokenType = TOKEN_TEXT;
			if (state->saveHeader)
			{
				saveNodeHeader(state, nodeInfo, 0);
			}
			else
			{
				state->dstPos--;
			}
			saveContent(state, nodeInfo);
			return;
		}
		else if (allowed & TOKEN_WHITESPACE)
		{
			if (XNODE_WHITESPACE(state->c))
			{
				nodeInfo->tokenType = TOKEN_WHITESPACE;
				return;
			}
			else
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else
		{
			UNEXPECTED_CHARACTER;
		}
	}
	nextXMLChar(state, false);
	if (*state->c == XNODE_CHAR_QUESTMARK)
	{
		char	   *declStart = specXMLStrings[XNODE_STR_XDECL_START];

		nextXMLChar(state, false);
		if (strncmp(state->c, declStart + 2, strlen(declStart + 2)) == 0 &&
			XNODE_WHITESPACE(state->c + strlen(declStart + 2)))
		{
			XMLNodeHdr	declAttrs[XNODE_XDECL_MAX_ATTRS];
			unsigned short declAttNum;

			if (!(allowed & TOKEN_XMLDECL))
			{
				elog(ERROR, "XML declaration not allowed at row %u, column %u", tagRow, tagCol);
			}
			processTag(state, nodeInfo, TOKEN_XMLDECL, declAttrs, &declAttNum, NULL, NULL);
			nodeInfo->tokenType = TOKEN_XMLDECL;
			state->decl = palloc(sizeof(XMLDeclData));
			checkXMLDeclaration(declAttrs, declAttNum, state->decl);
			return;
		}
		else
		{
			char	   *piCnt,
					   *cPtr;
			unsigned short i;
			unsigned int cntLen,
						valLen;
			XMLNodeOffset piNodeOff;
			XMLNodeHdr	piNode;

			if (!(allowed & TOKEN_PI))
			{
				elog(ERROR, "Processing instruction not allowed at row %u, column %u", tagRow, tagCol);
			}
			nodeInfo->cntSrc = state->srcPos;
			nodeInfo->nodeOut = state->dstPos;
			piNodeOff = nodeInfo->nodeOut;
			nodeInfo->tokenType = TOKEN_PI;
			saveNodeHeader(state, nodeInfo, 0);

			cntLen = readXMLPI(state);
			piCnt = state->inputText + nodeInfo->cntSrc;
			cPtr = piCnt;
			for (i = 0; i < cntLen; i++)
			{
				if (XNODE_WHITESPACE(cPtr))
				{
					break;
				}
				cPtr++;
			}
			nodeInfo->cntLength = i;
			saveContent(state, nodeInfo);

			if (i == cntLen)
			{
				return;
			}
			for (; i < cntLen; i++)
			{
				if (!XNODE_WHITESPACE(cPtr))
				{
					break;
				}
				cPtr++;
			}
			if (i == cntLen)
			{
				return;
			}
			piCnt = cPtr;
			valLen = 0;
			nodeInfo->cntLength = cntLen - i;
			nodeInfo->cntSrc += i - valLen;
			saveContent(state, nodeInfo);
			piNode = (XMLNodeHdr) (state->tree + piNodeOff);
			piNode->flags = XNODE_PI_HAS_VALUE;
			return;
		}
	}
	else if (*state->c == XNODE_CHAR_EXCLMARK)
	{
		unsigned char charsProcessed;

		nextXMLChar(state, false);

		/*
		 * Left arrow followed by exclamation mark
		 */
		charsProcessed = 2;

		if (readSpecialXMLStringPart(specXMLStrings, XNODE_STR_DTD_START, state, charsProcessed))
		{
			if (!(allowed & TOKEN_DTD))
			{
				elog(ERROR, "DTD not allowed here, see row %u, column %u", tagRow, tagCol);
			}
			nodeInfo->cntSrc = state->srcPos;
			nodeInfo->nodeOut = state->dstPos;
			xmlnodeParseDTD(state);
			nodeInfo->cntLength = state->srcPos - nodeInfo->cntSrc;
			nodeInfo->tokenType = TOKEN_DTD;
			saveNodeHeader(state, nodeInfo, 0);
			saveContent(state, nodeInfo);
			return;
		}
		else if (readSpecialXMLStringPart(specXMLStrings, XNODE_STR_CMT_START, state, charsProcessed))
		{
			if (!(allowed & TOKEN_COMMENT))
			{
				elog(ERROR, "Comment not allowed here, see row %u, column %u", tagRow, tagCol);
			}
			nodeInfo->cntSrc = state->srcPos;
			nodeInfo->nodeOut = state->dstPos;
			nodeInfo->cntLength = readXMLComment(state);
			nodeInfo->tokenType = TOKEN_COMMENT;
			saveNodeHeader(state, nodeInfo, 0);
			saveContent(state, nodeInfo);
			return;
		}
		else if (readSpecialXMLStringPart(specXMLStrings, XNODE_STR_CDATA_START, state, charsProcessed))
		{
			unsigned int i;
			unsigned char flags = 0;

			if (!(allowed & TOKEN_CDATA))
			{
				elog(ERROR, "CDATA not allowed here, see row %u, column %u", tagRow, tagCol);
			}
			nodeInfo->cntSrc = state->srcPos;
			nodeInfo->nodeOut = state->dstPos;
			while (!XNODE_SPEC_TEXT_END(XNODE_STR_CDATA_END))
			{
				char		c = *state->c;

				if (!XNODE_VALID_CHAR(state->c))
				{
					elog(ERROR, "Invalid XML character at row %u, column %u",
						 state->srcRow, state->srcCol);
				}
				if ((c == XNODE_CHAR_LARROW || c == XNODE_CHAR_RARROW || c == XNODE_CHAR_AMPERSAND))
				{
					flags |= XNODE_TEXT_SPEC_CHARS;
				}
				nextXMLChar(state, false);
			}
			nodeInfo->cntLength = state->srcPos - nodeInfo->cntSrc;
			for (i = 0; i < strlen(specXMLStrings[XNODE_STR_CDATA_END]) - 1; i++)
			{
				nextXMLChar(state, false);
			}
			nodeInfo->tokenType = TOKEN_CDATA;
			saveNodeHeader(state, nodeInfo, flags);
			saveContent(state, nodeInfo);
			return;
		}
		else
		{
			elog(ERROR, "Unrecognized tag at row %u, column %u.", tagRow, tagCol);
		}
	}
	else if (*state->c == XNODE_CHAR_SLASH)
	{
		if (!(allowed & TOKEN_ETAG))
		{
			elog(ERROR, "End tag not allowed at row %u, column %u", tagRow, tagCol);
		}
		nextXMLChar(state, false);
		if (!XNODE_VALID_NAME_START(state->c))
		{
			elog(ERROR, "Invalid tag name at row %u, column %u.", tagRow, tagCol);
		}

		/*
		 * If a valid ETag name starts here, we need to record the starting
		 * position.
		 */
		processTag(state, nodeInfo, TOKEN_ETAG, NULL, NULL, NULL, NULL);
		(state->nestLevel)--;
		nodeInfo->tokenType = TOKEN_ETAG;
		return;
	}
	else
	{
		/*
		 * Assume this is either STag or EmptyElement
		 */
		XMLNodeToken tagType;
		unsigned int stackPosOrig = state->stack.position;
		unsigned short int children,
					attributes;
		unsigned int nmspDecls = 0;
		unsigned int attrsPrefixedCount = 0;
		bool		isSpecialNode = false;
		XMLNodeKind specialNodeKind = 0;
		unsigned int specAttrCount = 0;
		bool	   *specAttrsValid = NULL;

		if (!(allowed & TOKEN_STAG))
		{
			elog(ERROR, "element not allowed at row %u, column %u.", tagRow, tagCol);
		}
		if (!XNODE_VALID_NAME_START(state->c))
		{
			elog(ERROR, "Invalid tag name at row %u, column %u.", tagRow, tagCol);
		}

		tagType = processTag(state, nodeInfo, TOKEN_STAG | TOKEN_EMPTY_ELEMENT, NULL, NULL, &nmspDecls,
							 &attrsPrefixedCount);

		if ((nodeInfo->nmspLength > 0 || attrsPrefixedCount > 0) && !state->nmspPrefix)
		{
			state->nmspPrefix = true;
		}

		/*
		 * Check if all namespaces are bound. This is only necessary for
		 * document. Node will be checked if/when it gets added to a document.
		 */
		if ((state->targetKind == XMLNODE_DOC || state->targetKind == XNTNODE_ROOT) &&
			(nodeInfo->nmspLength > 0 || attrsPrefixedCount > 0))
		{

			checkNamespaces(state, nodeInfo, attrsPrefixedCount, &isSpecialNode);

			if (isSpecialNode)
			{
				char	   *name;

				Assert(state->targetKind == XNTNODE_ROOT);
				Assert(state->getXNodeKindFunc != NULL);

				name = (char *) palloc(nodeInfo->cntLength + 1);
				memcpy(name, state->inputText + nodeInfo->cntSrc, nodeInfo->cntLength);
				name[nodeInfo->cntLength] = '\0';
				specialNodeKind = state->getXNodeKindFunc(name);
				pfree(name);
			}
		}

		/*
		 * The initial number of children equals to number of attributes
		 */
		attributes = children = state->stack.position - stackPosOrig;

		if (isSpecialNode || state->targetKind == XNTNODE_ROOT)
		{
			unsigned int attrsNewMaxCount;
			XNodeListItem *attrOffsets,
					   *attrOffsetsNew;
			char	   *attrsNew = NULL;
			unsigned int newSize = 0;
			unsigned int newAttrCount = 0;

			/*
			 * One may think processTag() can do this, but it does not have
			 * sufficient information to decide whether given attribute should
			 * accept a template containing parameters. It's accepted, in most
			 * cases, e.g.
			 *
			 * <myelement myattr="{myparam}"/>
			 *
			 * However not always: <xnt:copy-of-param name="myparam"/>
			 *
			 * Inside processTag() we don't know yet if e.g the last attribute
			 * of the current tag isn't just overriding the 'xnt' namespace
			 * and thus making an 'ordinary tag' out of the '<copy-of-param/>'
			 *
			 * While special format has to be used in both cases, the parsing
			 * algorithm is different.
			 */

			attrsNewMaxCount = attributes + XNT_SPECIAL_ATTRS_MAX;
			attrOffsets = state->stack.content + stackPosOrig;
			attrOffsetsNew = (XNodeListItem *) palloc(attrsNewMaxCount * sizeof(XNodeListItem));

			if (attributes > 0)
			{
				/* Prepare input data for preprocessXNTAttributes(). */
				memcpy(attrOffsetsNew, attrOffsets, attributes * sizeof(XNodeListItem));
			}

			if (isSpecialNode)
			{
				/*
				 * preprocessXNTAttributes() is called even if there are no
				 * attributes:
				 *
				 * 1. To check whether the node does not miss any required
				 * attribute. 2. To ensure that missing optional attributes
				 * have the (invalid) offset set at the appropriate position.
				 */
				specAttrsValid = (bool *) palloc(attrsNewMaxCount * sizeof(bool));
				attrsNew = preprocessXNTAttributes(attrOffsetsNew, attributes, state->tree, specialNodeKind,
												   specAttrsValid, &specAttrCount, &newSize, &newAttrCount, &state->paramNames);
			}
			else if (state->targetKind == XNTNODE_ROOT)
			{
				/*
				 * When parsing template, non-special attributes need to be
				 * checked too: some of them may reference parameters in the
				 * value.
				 */
				attrsNew = preprocessXNTAttrValues(attrOffsetsNew, attributes, state->tree, &newSize, &state->paramNames);
				newAttrCount = attributes;
			}

			if (attrsNew != NULL)
			{
				unsigned int i;
				char	   *attrDataOrig,
						   *attrDataNew;
				unsigned int attrDataOrigSize,
							padding;

				attrDataOrig = state->tree + attrOffsets->value.singleOff;

				/*
				 * The new binary data are more restrictive, because of XPath
				 * expressions.
				 */
				attrDataNew = (char *) TYPEALIGN(XPATH_ALIGNOF_EXPR, attrDataOrig);
				padding = attrDataNew - attrDataOrig;
				attrDataOrigSize = state->dstPos - attrOffsets->value.singleOff;

				Assert(newAttrCount > 0 && newAttrCount >= attributes);

				/*
				 * Copy the possibly adjusted values back into the stack and
				 * add new if appropriate.
				 */
				for (i = 0; i < newAttrCount; i++)
				{
					XNodeListItem *itemNew = attrOffsetsNew + i;
					XMLNodeOffset attrOffNew = itemNew->value.singleOff + padding;

					if (i < attributes)
					{
						XNodeListItem *itemOrig = attrOffsets + i;

						itemOrig->value.singleOff = attrOffNew;
					}
					else
					{
						xmlnodePushSingleNode(&state->stack, attrOffNew);
					}

					/*
					 * If attribute of an 'ordinary' element contains
					 * parameter it's considered a 'substitution node'.
					 * Special node is a substitution node itself, so we don't
					 * pay attention to its attributes.
					 */
					if (!isSpecialNode)
					{
						XMLNodeHdr	attrNode = (XMLNodeHdr) (state->tree + attrOffNew);

						/* Not all attributes are special (binary) */
						if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
						{
							xmlnodePushSingleNode(&state->substNodes, attrOffNew);
						}
					}
				}

				/*
				 * Replace the original attribute data with those (possibly)
				 * changed.
				 */
				newSize += padding;
				if (newSize > attrDataOrigSize)
				{				/* Reallocation must be anticipated. */
					XMLNodeOffset offOrig = attrDataOrig - state->tree;
					XMLNodeOffset offNew = attrDataNew - state->tree;

					ensureSpaceIn(newSize - attrDataOrigSize, state);
					attrDataOrig = state->tree + offOrig;
					attrDataNew = state->tree + offNew;
				}

				memcpy(attrDataOrig + padding, attrsNew, newSize - padding);

				if (newSize != attrDataOrigSize)
				{
					if (newSize > attrDataOrigSize)
					{
						state->dstPos += newSize - attrDataOrigSize;
					}
					else
					{
						state->dstPos -= attrDataOrigSize - newSize;
					}
				}
				pfree(attrsNew);

				children = attributes = newAttrCount;

				/*
				 * If all the attributes of the current node have been
				 * shifted, then we have to update the corresponding namespace
				 * declaration pointers.
				 */
				if (state->nmspDecl.content != NULL && state->nmspDecl.position > 0)
				{
					unsigned short i;
					unsigned int declsTotal = state->nmspDecl.position;
					XNodeListItem *declItem,
							   *newAttrItem;
					unsigned int declsUpdated = 0;
					unsigned int newAttrsToCheck;

					Assert(nmspDecls <= declsTotal);

					/*
					 * Start at the first namespace declaration that the
					 * current node has introduced.
					 */
					declItem = state->nmspDecl.content + declsTotal - nmspDecls;

					/* First non-special attribute the current node contains. */
					Assert(specAttrCount <= newAttrCount);
					newAttrsToCheck = newAttrCount - specAttrCount;
					newAttrItem = state->stack.content + state->stack.position - newAttrsToCheck;

					for (i = 0; i < newAttrsToCheck; i++)
					{
						unsigned int nmspPrefLen = strlen(XNODE_NAMESPACE_DEF_PREFIX);
						XMLNodeHdr	attrNode = (XMLNodeHdr) (state->tree + newAttrItem->value.singleOff);
						char	   *attrName;

						if ((attrNode->flags & XNODE_ATTR_VALUE_BINARY) ||
							(!(attrNode->flags & XNODE_NMSP_PREFIX)))
							/* Can't be namespace declaration. */
							continue;

						attrName = XNODE_CONTENT(attrNode);

						if (strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX, nmspPrefLen) == 0 &&
							attrName[nmspPrefLen] == XNODE_CHAR_COLON)
						{
							declItem->value.singleOff = newAttrItem->value.singleOff;
							declItem++;
							declsUpdated++;
						}
						newAttrItem++;
					}
					Assert(declsUpdated == nmspDecls);
				}
			}
			pfree(attrOffsetsNew);
		}

		if (tagType == TOKEN_EMPTY_ELEMENT)
		{
			XMLCompNodeHdr element;
			XMLNodeOffset elementOff;
			unsigned int padding;

			/*
			 * In this case 'child' always means 'attribute'
			 */

			nodeInfo->nodeOut = state->dstPos;
			nodeInfo->tokenType = TOKEN_EMPTY_ELEMENT;
			padding = saveNodeHeader(state, nodeInfo, XNODE_EMPTY);
			nodeInfo->nodeOut += padding;
			elementOff = nodeInfo->nodeOut;
			element = (XMLCompNodeHdr) (state->tree + elementOff);

			/*
			 * Overwrite the node kind additionally. This is quite rare action
			 * so we don't have to make the 'saveNodeHeader() function less
			 * generic.
			 */
			if (isSpecialNode)
			{
				element->common.kind = specialNodeKind;
				element->common.flags |= XNODE_EL_SPECIAL;

				/*
				 * Remember which nodes need to be constructed when using the
				 * template.
				 */
				if (element->common.kind != XNTNODE_ROOT && element->common.kind != XNTNODE_TEMPLATE)
				{
					xmlnodePushSingleNode(&state->substNodes, elementOff);
				}
			}

			element->children = children;
			if (children > 0)
			{
				saveReferences(state, nodeInfo, element, children, specAttrsValid, specAttrCount);
			}
			if (specAttrsValid != NULL)
			{
				pfree(specAttrsValid);
			}

			/*
			 * Node name would be redundant in this case, the 'kind' is
			 * enough.
			 */
			if (!isSpecialNode)
			{
				saveContent(state, nodeInfo);
			}

			/*
			 * The most recent namespace declarations might only be relevant
			 * for one element.
			 */
			forgetNamespaceDeclarations(nmspDecls, &state->nmspDecl);
			return;
		}
		else
		{
			/*
			 * STag
			 */
			XMLNodeInternalData childTag;
			unsigned int nlBefore = ++(state->nestLevel);
			bool		childrenProcessed = false;
			bool		match;
			XMLCompNodeHdr element;
			XMLNodeHdr	firstText = NULL;
			XMLNodeOffset elementOff;
			unsigned int padding;

			state->saveHeader = true;

			/*
			 * Process children
			 */
			do
			{
				nextXMLChar(state, false);
				processToken(state, &childTag, TOKEN_STAG | TOKEN_ETAG | TOKEN_EMPTY_ELEMENT
							 | TOKEN_TEXT | TOKEN_CDATA | TOKEN_COMMENT | TOKEN_PI | TOKEN_REFERENCE);

				/*
				 * If text is mixed with character references, everything
				 * should be saved into a single text node.
				 */
				state->saveHeader = (childTag.tokenType != TOKEN_TEXT &&
									 childTag.tokenType != TOKEN_REFERENCE);
				if (!state->saveHeader)
				{
					if (firstText == NULL)
					{
						firstText = (XMLNodeHdr) (state->tree + childTag.nodeOut);
					}
					if (childTag.tokenType == TOKEN_REFERENCE && childTag.entPredef)
					{
						firstText->flags |= XNODE_TEXT_SPEC_CHARS;
					}
				}
				if (nlBefore != state->nestLevel)
				{
					/*
					 * ETag has just been processed and thus the nesting level
					 * was decreased
					 */
					Assert(childTag.tokenType == TOKEN_ETAG);

					/*
					 * Namespaces declared in the current STag are no longer
					 * applicable.
					 */
					forgetNamespaceDeclarations(nmspDecls, &state->nmspDecl);

					childrenProcessed = true;
				}
				if (!childrenProcessed)
				{
					if (children == XMLNODE_MAX_CHILDREN)
					{
						elog(ERROR, "Maximum number of %u children exceeded for node '%s'.",
							 XMLNODE_MAX_CHILDREN, getContentToLog(state->inputText, nodeInfo->cntSrc,
												   nodeInfo->cntLength, 16));
					}
					if (childTag.headerSaved)
					{
						xmlnodePushSingleNode(&state->stack, childTag.nodeOut);
						children++;
					}
				}
			} while (!childrenProcessed);

			Assert(childTag.tokenType == TOKEN_ETAG);
			match = true;

			/*
			 * When the previous loop has ended, 'childTag' actually contains
			 * ETag of the current level, instead of any child element's tag
			 */
			if (childTag.cntLength != nodeInfo->cntLength)
			{
				match = false;
			}
			else
			{
				/*
				 * Compare start and end tag
				 */
				char	   *sTag = state->inputText + nodeInfo->cntSrc;
				char	   *eTag = state->inputText + childTag.cntSrc;
				unsigned int i;

				for (i = 0; i < childTag.cntLength; i++)
				{
					if (*sTag != *eTag)
					{
						match = false;
						break;
					}
					sTag++;
					eTag++;
				}
			}
			if (!match)
			{
				elog(ERROR, "There's no matching start tag for end tag '%s' or it's not at the appropriate level.",
					 getContentToLog(state->inputText, childTag.cntSrc, childTag.cntLength, 16));
			}

			/*
			 * Now that children are processed, the node can be written to
			 * output.
			 */
			nodeInfo->nodeOut = state->dstPos;
			nodeInfo->tokenType = childTag.tokenType;
			padding = saveNodeHeader(state, nodeInfo, children == attributes ? XNODE_EMPTY : 0);
			nodeInfo->nodeOut += padding;
			elementOff = nodeInfo->nodeOut;
			element = (XMLCompNodeHdr) (state->tree + elementOff);

			if (isSpecialNode)
			{
				element->common.kind = specialNodeKind;
				element->common.flags |= XNODE_EL_SPECIAL;

				if (element->common.kind != XNTNODE_ROOT && element->common.kind != XNTNODE_TEMPLATE)
				{
					xmlnodePushSingleNode(&state->substNodes, elementOff);
				}
			}

			element->children = children;
			if (children > 0)
			{
				saveReferences(state, nodeInfo, element, children, specAttrsValid, specAttrCount);
			}

			if (specAttrsValid != NULL)
			{
				pfree(specAttrsValid);
			}

			if (!isSpecialNode)
			{
				saveContent(state, nodeInfo);
			}
			return;
		}
	}
	elog(ERROR, "Unrecognized tag at row %u, column %u.", tagRow, tagCol);
}

static void
forgetNamespaceDeclarations(unsigned int count, XMLNodeContainer stack)
{
	if (count > 0)
	{
		unsigned short i;

		for (i = 0; i < count; i++)
		{
			xmlnodePopOffset(stack);
		}
	}
}

/*
 * Check for the element being parsed whether its name as well as attribute names don't use
 * unbound namespace prefixes.
 */
static void
checkNamespaces(XMLParserState state, XMLNodeInternal nodeInfo, unsigned int attrsPrefixedCount, bool *elNmspIsSpecial)
{
	/*
	 * The element name must be taken from the source text because the element
	 * is not saved yet.
	 */
	char	   *elNmspName = state->inputText + nodeInfo->cntSrc;
	bool		elNmspNameResolved = (nodeInfo->nmspLength == 0);
	unsigned short attrsUnresolved;
	XMLNodeHdr *attrsPrefixed = NULL;
	bool	   *attrFlags = NULL;

	if (attrsPrefixedCount > 0)
	{
		unsigned int i = 0;
		unsigned int posOrig;
		unsigned int flagsSize = attrsPrefixedCount * sizeof(bool);
		unsigned int nmspPrefLen = strlen(XNODE_NAMESPACE_DEF_PREFIX);

		attrFlags = (bool *) palloc(flagsSize);
		/* Set all to 'false' to indicate that none is resolved yet. */
		memset(attrFlags, false, flagsSize);

		/* Remember all the attributes that need to be checked. */
		attrsPrefixed = (XMLNodeHdr *) palloc(attrsPrefixedCount * sizeof(XMLNodeHdr));

		posOrig = state->stack.position;

		/*
		 * The total number of attributes that the current element has is not
		 * easily available from the node itself. Therefore pick the most
		 * recent prefixed attributes one after another, until the count
		 * reaches 'attrsPrefixedCount'.
		 */
		while (i < attrsPrefixedCount)
		{
			XMLNodeOffset attrOff = xmlnodePopOffset(&state->stack);
			XMLNodeHdr	attrNode = (XMLNodeHdr) (state->tree + attrOff);
			char	   *attrName = XNODE_CONTENT(attrNode);

			/*
			 * Record prefixed attributes but omit namespace declarations
			 * ('xmlns:...')
			 */
			if ((attrNode->flags & XNODE_NMSP_PREFIX) &&
			  !(strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX, nmspPrefLen) &&
				attrName[nmspPrefLen] == XNODE_CHAR_COLON))
			{
				attrsPrefixed[i] = attrNode;
				i++;
			}
		}
		/* Reset the original position of the stack. */
		state->stack.position = posOrig;

		attrsUnresolved = attrsPrefixedCount;
	}
	else
	{
		attrsUnresolved = 0;
	}

	if (!elNmspNameResolved || attrsUnresolved > 0)
	{
		resolveXMLNamespaces(state->tree, &state->nmspDecl, state->nmspDecl.position, elNmspName, &elNmspNameResolved, attrsPrefixed,
							 attrsPrefixedCount, attrFlags, &attrsUnresolved, state->nmspSpecialName,
							 state->nmspSpecialValue, elNmspIsSpecial);
	}

	if (!elNmspNameResolved)
	{
		elog(ERROR, "element '%s' references unbound namespace",
			 getContentToLog(state->inputText, nodeInfo->cntSrc, nodeInfo->cntLength, 16));
	}

	if (attrsUnresolved > 0)
	{
		unsigned int i;
		XMLNodeHdr	attr;

		/*
		 * Multiple unresolved attributes may be found. Exactly one is
		 * reported each time.
		 */
		for (i = 0; i < attrsPrefixedCount; i++)
		{
			if (!attrFlags[i])
			{
				break;
			}
		}
		Assert(i < attrsPrefixedCount);
		attr = attrsPrefixed[i];
		elog(ERROR, " attribute '%s' of element '%s' references unbound namespace",
			 XNODE_CONTENT(attr),
			 getContentToLog(state->inputText, nodeInfo->cntSrc, nodeInfo->cntLength, 16));
	}

	if (attrFlags != NULL)
	{
		pfree(attrFlags);
	}
	if (attrsPrefixed != NULL)
	{
		pfree(attrsPrefixed);
	}

}

/*
 * Process the rest of the current tag (http://www.w3.org/TR/xml/#NT-STag,
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-ETag or
 * http://www.w3.org/TR/xml/#NT-EmptyElemTag)
 *
 * 'allowed'	(STag | EmptyElement) or XML declaration. In this case,
 * exactly 1 bit can be set in 'allowed'.
 *
 * '*nmspDecls' - namespaces declared in the tag (other than the default namespace).
 *
 * '*attrsPrefixed' - number of attributes having namespace prefix. Declarations
 * (e.g. xmlns:me="...") are not counted here.
 *
 * Both 'declAttrs' and 'declAttrNum' must be not-NULL if XML declaration is
 * to be parsed
 */

static XMLNodeToken
processTag(XMLParserState state, XMLNodeInternal nodeInfo, XMLNodeToken allowed,
		   XMLNodeHdr *declAttrs, unsigned short *declAttrNum, unsigned int *nmspDecls, unsigned int *attrsPrefixed)
{
	bool		mustEnd = false;
	unsigned short attributes = 0;
	unsigned int stackInit = state->stack.position;
	unsigned int firstColPos = 0;
	char	   *elNameSrc = state->c;
	unsigned int declAttrFirst = 0;
	bool		defNamespaceDeclared = false;
	unsigned int nmspDefPrefixLen = strlen(XNODE_NAMESPACE_DEF_PREFIX);
	unsigned int elNameLen;

	if (nmspDecls != NULL)
	{
		*nmspDecls = 0;
	}
	if (attrsPrefixed != NULL)
	{
		*attrsPrefixed = 0;
	}

	/*
	 * We're at the first character of the name.
	 */
	nodeInfo->cntSrc = state->srcPos;
	readXMLName(state, false, true, false, &firstColPos);
	nodeInfo->nmspLength = firstColPos;

	if (firstColPos > 0)
	{
		if (firstColPos == nmspDefPrefixLen &&
			strncmp(elNameSrc, XNODE_NAMESPACE_DEF_PREFIX, firstColPos) == 0)
		{
			elog(ERROR, "tag name must not have '%s' as a prefix", XNODE_NAMESPACE_DEF_PREFIX);
		}
	}
	else
	{
		/* If colon is the first character, ignore it. */
		if (*elNameSrc == XNODE_CHAR_COLON)
		{
			nodeInfo->cntSrc++;
		}
	}

	nodeInfo->cntLength = elNameLen = state->srcPos - nodeInfo->cntSrc;

	while (true)
	{
		/*
		 * End of the tag?
		 */
		switch (*state->c)
		{
			case XNODE_CHAR_RARROW:
				if ((allowed & (TOKEN_STAG | TOKEN_ETAG)) == 0)
				{
					UNEXPECTED_CHARACTER;
				}
				else
				{
					return TOKEN_STAG | TOKEN_ETAG;
				}
				break;

			case XNODE_CHAR_SLASH:
				if ((allowed & TOKEN_EMPTY_ELEMENT) == 0)
				{
					UNEXPECTED_CHARACTER;
				}
				nextXMLChar(state, false);
				if (*state->c == XNODE_CHAR_RARROW)
				{
					return TOKEN_EMPTY_ELEMENT;
				}
				else
				{
					UNEXPECTED_CHARACTER;
				}
				break;

			case XNODE_CHAR_QUESTMARK:
				if ((allowed & TOKEN_XMLDECL) == 0)
				{
					UNEXPECTED_CHARACTER;
				}
				nextXMLChar(state, false);
				if (*state->c == XNODE_CHAR_RARROW)
				{
					if (attributes > 0)
					{
						unsigned int i;

						/*
						 * The XML declaration attributes have been stored to
						 * the output array just temporarily and should be
						 * overwritten by the next nodes.
						 */
						state->dstPos = declAttrFirst;

						for (i = attributes; i > 0; i--)
						{
							declAttrs[i - 1] = (XMLNodeHdr) (state->tree + xmlnodePopOffset(&state->stack));
						}
						*declAttrNum = attributes;
					}
					return TOKEN_XMLDECL;
				}
				else
				{
					UNEXPECTED_CHARACTER;
				}
				break;
		}
		if (mustEnd)
		{
			elog(ERROR, "Attribute or tag end expected at row %u, column %u.", state->srcRow, state->srcCol);
		}

		if (!XNODE_WHITESPACE(state->c))
		{
			UNEXPECTED_CHARACTER;
		}
		else
		{
			char		quotMark;

			nextXMLChar(state, false);

			/*
			 * Process a single attribute
			 * (http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Att ribute)
			 */
			if (XNODE_VALID_NAME_START(state->c))
			{
				unsigned int nameStartPos,
							nameLength;
				XMLNodeHdr	attrNode;
				XNodeListItem *stackItems;
				unsigned short int i;
				char	   *attrName,
						   *attrValue;
				char	   *attrNameSrc = state->c;
				bool		refsInValue;
				bool		emptyAllowed = true;

				if (allowed == TOKEN_ETAG)
				{
					elog(ERROR, "End tag is not allowed to have attributes, see row %u, column %u.",
						 state->srcRow, state->srcCol);
				}
				if (attributes == XMLNODE_MAX_CHILDREN)
				{
					elog(ERROR, "Maximum number of %u children exceeded for node '%s'.",
						 XMLNODE_MAX_CHILDREN, getContentToLog(state->inputText, nodeInfo->cntSrc, nodeInfo->cntLength,
															   16));
				}
				nameStartPos = state->srcPos;

				readXMLName(state, false, true, false, &firstColPos);

				if (firstColPos > 0)
				{
					if (firstColPos == nmspDefPrefixLen &&
						strncmp(attrNameSrc, XNODE_NAMESPACE_DEF_PREFIX, firstColPos) == 0)
					{
						/* (non-default) namespace declaration. */
						emptyAllowed = false;
					}
					else
					{
						/* namespace use */
						if (attrsPrefixed != NULL)
						{
							(*attrsPrefixed)++;
						}
					}
				}
				else
				{
					/* If colon is the first character, ignore it. */
					if (*attrNameSrc == XNODE_CHAR_COLON)
					{
						nameStartPos++;
					}
				}

				nameLength = state->srcPos - nameStartPos;

				if (XNODE_WHITESPACE(state->c))
				{
					nextXMLChar(state, false);
				}
				if (*state->c != XNODE_CHAR_EQ)
				{
					elog(ERROR, "'%c' expected at row %u, column %u.", XNODE_CHAR_EQ,
						 state->srcRow, state->srcCol);
				}
				else
				{
					nextXMLChar(state, false);
				}
				if (XNODE_WHITESPACE(state->c))
				{
					nextXMLChar(state, false);
				}

				/*
				 * Save attribute, the name first.
				 *
				 * Let's store the attributes, whether the node is XML
				 * declaration or not. Even if it's the declaration, we store
				 * the names&values to the output array.
				 *
				 */
				xmlnodePushSingleNode(&state->stack, state->dstPos);
				if (allowed == TOKEN_XMLDECL)
				{
					if (attributes == 0)
					{
						declAttrFirst = state->dstPos;
					}
					else if (attributes >= XNODE_XDECL_MAX_ATTRS)
					{
						elog(ERROR, "XML declaration may contain %u attributes at maximum.", XNODE_XDECL_MAX_ATTRS);
					}
				}
				ensureSpaceIn(sizeof(XMLNodeHdrData) + nameLength + 1, state);
				attrNode = (XMLNodeHdr) (state->tree + state->dstPos);
				attrNode->kind = XMLNODE_ATTRIBUTE;
				attrNode->flags = 0;

				if (firstColPos > 0)
				{
					attrNode->flags |= XNODE_NMSP_PREFIX;
				}
				state->dstPos += sizeof(XMLNodeHdrData);
				attrName = state->tree + state->dstPos;
				memcpy(attrName, state->inputText + nameStartPos, nameLength);
				*(state->tree + state->dstPos + nameLength) = '\0';
				state->dstPos += nameLength + 1;

				/*
				 * Is the attribute name unique?
				 */
				stackItems = &(state->stack.content[stackInit]);
				for (i = 0; i < attributes; i++)
				{
					char	   *nameOld;
					XMLNodeHdr	attrOld = (XMLNodeHdr) (state->tree + stackItems->value.singleOff);

					nameOld = (char *) attrOld + sizeof(XMLNodeHdrData);
					stackItems++;
					if (strcmp(attrName, nameOld) == 0)
					{
						elog(ERROR, "Attribute '%s' of node '%s' is not unique.",
							 getContentToLog(state->inputText, nameStartPos, nameLength, 16),
							 getContentToLog(state->inputText, nodeInfo->cntSrc, nodeInfo->cntLength, 16));
					}
				}

				if (strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX, nmspDefPrefixLen) == 0)
				{
					/*
					 * Both xmlns="..." and xmlns:="..." are valid
					 * declarations of the default namespace.
					 */
					if (nameLength == nmspDefPrefixLen ||
						(nameLength == (nmspDefPrefixLen + 1) && attrName[nmspDefPrefixLen] == XNODE_CHAR_COLON))
					{

						if (defNamespaceDeclared)
						{
							elog(ERROR, "default name space is already declared for element '%s'",
								 getContentToLog(state->inputText, nodeInfo->cntSrc, nodeInfo->cntLength, 16));
						}
						else
						{
							defNamespaceDeclared = true;
						}
					}
					else if (attrName[nmspDefPrefixLen] == XNODE_CHAR_COLON)
					{
						/*
						 * Declaration of non-default namespace, e.g.
						 * xmlns:a="..."
						 */

						/* readName() does not allow for 2 or more colons. */
						Assert(attrName[nameLength - 1] != XNODE_CHAR_COLON);

						xmlnodePushSingleNode(&state->nmspDecl, (char *) attrNode - state->tree);
						if (nmspDecls != NULL)
						{
							(*nmspDecls)++;
						}
					}
				}

				quotMark = *state->c;
				attrValue = readXMLAttValue(state, true, &refsInValue);

				if (strlen(attrValue) == 0 && !emptyAllowed)
				{
					elog(ERROR, "attribute '%s' is not allowed to have empty value", attrName);
				}

				attrNode->flags |= getXMLAttributeFlags(attrValue, refsInValue, quotMark == XNODE_CHAR_APOSTR);
				attributes++;
				nextXMLChar(state, false);
			}
			else
			{
				mustEnd = true;
				continue;
			}
		}
	}

	return 0;					/* Keep the compiler silent - control should
								 * never get here. */
}

/*
 * http://www.w3.org/TR/xml/#NT-XMLDecl
 */
static void
checkXMLDeclaration(XMLNodeHdr *declAttrs, unsigned int attCount, XMLDecl decl)
{
	unsigned int i;

	decl->flags = 0;
	if (attCount == 0)
	{
		elog(ERROR, "XML declaration must specify XML version");
	}
	for (i = 0; i < attCount; i++)
	{
		char	   *name,
				   *value;
		XMLNodeHdr	attr = declAttrs[i];

		name = XNODE_CONTENT(attr);
		value = name + strlen(name) + 1;
		if (i == 0)
		{
			unsigned char j;
			bool		versFound = false;

			if (strcmp(name, xmldeclAttNames[i]) != 0)
			{
				elog(ERROR, "value '%s' not allowed for XML declaration attribute #%u", name, i + 1);
			}
			for (j = 0; j < XNODE_XDECL_VERSIONS; j++)
			{
				if (strcmp(value, xmldeclVersions[j]) == 0)
				{
					versFound = true;
					decl->version = j;
					break;
				}
			}
			if (!versFound)
			{
				elog(ERROR, "unsupported XML version: '%s'", value);
			}
		}
		else
		{
			unsigned char encDeclNow = (strcmp(name, xmldeclAttNames[XNODE_XDECL_ATNAME_ENCODING]) == 0) ?
			XMLDECL_HAS_ENC : 0;
			unsigned char standaloneNow = (strcmp(name, xmldeclAttNames[XNODE_XDECL_ATNAME_STANDALONE]) == 0) ?
			XMLDECL_HAS_SD_DECL : 0;

			decl->flags |= encDeclNow;
			decl->flags |= standaloneNow;

			if ((i == 2 && (encDeclNow ||
				 (standaloneNow && ((decl->flags & XMLDECL_HAS_ENC) == 0)) ||
				 (encDeclNow && ((decl->flags & XMLDECL_HAS_SD_DECL) == 0))))
				)
			{
				elog(ERROR, "value '%s' not allowed for XML declaration attribute number %u", name, i + 1);
			}
			if (encDeclNow)
			{
				const char *clEnc = pg_get_client_encoding_name();
				char	   *clEncSmp = getEncodingSimplified(clEnc);
				char	   *valueSmp = getEncodingSimplified(value);

				if (strcmp(clEncSmp, valueSmp) != 0)
				{
					elog(ERROR, "declared encoding '%s' doesn't match client encoding '%s'", value,
						 clEnc);
				}
				pfree(clEncSmp);
				pfree(valueSmp);
				decl->enc = pg_get_client_encoding();
				decl->flags |= XMLDECL_HAS_ENC;
			}
			else
			{
				if (strcmp(value, XMLDECL_STANDALONE_YES) != 0)
				{
					elog(ERROR, "unsupported value of XML declaration's 'standalone' attribute: '%s'",
						 value);
				}
				decl->standalone = true;
				decl->flags |= XMLDECL_HAS_SD_DECL;
			}
		}
		if (attr->flags & XNODE_ATTR_APOSTROPHE)
		{
			decl->flags |= (1 << (i + 2));
		}
	}
}

/*
 * Remove '-' and '_' characters and convert to upper case
 */
static char *
getEncodingSimplified(const char *original)
{
	unsigned int i;
	const char *c;
	char	   *d;
	char	   *result = (char *) palloc(strlen(original) + 1);

	c = original;
	d = result;
	for (i = 0; i < strlen(original); i++)
	{
		if (*c != XNODE_CHAR_UNDERSCORE && *c != XNODE_CHAR_DASH)
		{
			*d = toupper(*c);
			d++;
		}
		c++;
	}
	*d = 0x0;
	return result;
}


static bool
isPredefinedEntity(char *refStart, char *value)
{
	unsigned char i;

	for (i = 0; i < XNODE_PREDEFINED_ENTITIES; i++)
	{
		if (strstr(refStart, predefEntities[i].escaped) == refStart)
		{
			*value = predefEntities[i].simple;
			return true;
		}
	}
	return false;
}

/*
 * Check how many bytes the white space spans and how many line breaks it
 * contains
 */
static void
evaluateWhitespace(XMLParserState state)
{
	char	   *prev = NULL;
	char	   *cTmp = state->c;

	state->cWidth = 0;
	state->srcRowIncr = 0;

	do
	{
		if (*cTmp == 0xA)
		{
			state->srcRowIncr++;
		}
		prev = cTmp;
		cTmp++;
		state->cWidth++;

		/*
		 * 0xD, not followed by 0xA, should also be considered a row delimiter
		 */
		if (*prev == 0xD && *cTmp != 0xA)
		{
			state->srcRowIncr++;
		}
	} while (XNODE_WHITESPACE(cTmp) && *cTmp != 0x00);
}

static void
ensureSpaceIn(unsigned int size, XMLParserState state)
{
	unsigned int chunks = 0;
	unsigned int orig = state->sizeOut;

	while (state->dstPos + size > state->sizeOut)
	{
		if (state->targetKind != XMLNODE_ATTRIBUTE)
		{
			state->sizeOut += state->sizeOut >> XNODE_OUT_OVERHEAD_BITS;
		}
		else
		{
			state->sizeOut += 16;
		}
		chunks++;
	}

	if (chunks > 0)
	{
		unsigned int hdrSize = (state->targetKind == XMLNODE_ATTRIBUTE) ? 0 : VARHDRSZ;

		state->result = (char *) repalloc(state->result, state->sizeOut
										  + hdrSize);
		state->tree = state->result + hdrSize;
		elog(DEBUG1, "output array expanded from %u to %u bytes.", orig, state->sizeOut);
	}
}

static unsigned int
saveNodeHeader(XMLParserState state, XMLNodeInternal nodeInfo, char flags)
{
	unsigned int incr;
	char	   *outPtr;
	XMLNodeHdr	node;
	unsigned int padding = 0;

	outPtr = state->tree + nodeInfo->nodeOut;

	if (nodeInfo->tokenType & (TOKEN_ETAG | TOKEN_EMPTY_ELEMENT))
	{
		char	   *outPtrAligned;

		incr = sizeof(XMLCompNodeHdrData);
		outPtrAligned = (char *) TYPEALIGN(XNODE_ALIGNOF_COMPNODE, outPtr);
		padding = outPtrAligned - outPtr;
		incr += padding;
	}
	else
	{
		incr = sizeof(XMLNodeHdrData);
	}

	ensureSpaceIn(incr, state);

	/*
	 * 'outPtrAligned can't be used because reallocation might have taken
	 * place.
	 */
	node = (XMLNodeHdr) (state->tree + nodeInfo->nodeOut + padding);;
	node->flags = flags;
	switch (nodeInfo->tokenType)
	{
		case TOKEN_ETAG:
			node->kind = XMLNODE_ELEMENT;
			break;
		case TOKEN_EMPTY_ELEMENT:
			node->kind = XMLNODE_ELEMENT;
			break;
		case TOKEN_CDATA:
			node->kind = XMLNODE_CDATA;
			break;
		case TOKEN_COMMENT:
			node->kind = XMLNODE_COMMENT;
			break;
		case TOKEN_DTD:
			node->kind = XMLNODE_DTD;
			break;
		case TOKEN_PI:
			node->kind = XMLNODE_PI;
			break;
		case TOKEN_TEXT:
			node->kind = XMLNODE_TEXT;
			break;
		case TOKEN_REFERENCE:

			/*
			 * As long as only character references are supported, text node
			 * will be used.
			 */
			node->kind = XMLNODE_TEXT;
			break;
		default:
			elog(ERROR, "saveNodeHeader(): unrecognized token type: %u", nodeInfo->tokenType);
			break;
	}

	if (node->kind == XMLNODE_ELEMENT && nodeInfo->nmspLength > 0)
	{
		node->flags |= XNODE_NMSP_PREFIX;
	}

	state->dstPos += incr;
	nodeInfo->headerSaved = true;

	return padding;
}

static void
saveContent(XMLParserState state, XMLNodeInternal nodeInfo)
{
	if (nodeInfo->tokenType & (TOKEN_ETAG | TOKEN_EMPTY_ELEMENT |
							   TOKEN_CDATA | TOKEN_COMMENT | TOKEN_DTD | TOKEN_PI | TOKEN_TEXT | TOKEN_REFERENCE))
	{
		ensureSpaceIn(nodeInfo->cntLength + 1, state);
	}
	else
	{
		elog(ERROR, "saveContent(): unrecognized token type %u", nodeInfo->tokenType);
	}

	memcpy(state->tree + state->dstPos, state->inputText + nodeInfo->cntSrc,
		   nodeInfo->cntLength);
	state->dstPos += nodeInfo->cntLength;
	*(state->tree + state->dstPos) = '\0';
	state->dstPos++;
}

static void
saveReferences(XMLParserState state, XMLNodeInternal nodeInfo, XMLCompNodeHdr compNode,
   unsigned short children, bool *specAttrsValid, unsigned int specAttrCount)
{
	/*
	 * Find out the range of reference values and the corresponding storage.
	 */
	XMLNodeOffset dist = nodeInfo->nodeOut -
	state->stack.content[state->stack.position - children].value.singleOff;
	char		bwidth = getXMLNodeOffsetByteWidth(dist);
	unsigned int refsTotal = children * bwidth;
	char	   *childOffTarg;
	unsigned short int i;
	XMLNodeOffset elementOff = (char *) compNode - state->tree;

	ensureSpaceIn(refsTotal, state);
	compNode = (XMLCompNodeHdr) (state->tree + elementOff);
	XNODE_SET_REF_BWIDTH(compNode, bwidth);

	/*
	 * Later references will be stored first so that xmlnodePopOffset() can be
	 * used to remove the items from the stack.
	 */
	childOffTarg = XNODE_LAST_REF(compNode);

	for (i = 0; i < children; i++)
	{
		XMLNodeOffset childOffset = xmlnodePopOffset(&state->stack);

		/* The actual subscript (we're proceeding backwards). */
		unsigned int j = children - i - 1;

		if (specAttrsValid != NULL && !specAttrsValid[j] && j < specAttrCount)
		{
			dist = XMLNodeOffsetInvalid;
		}
		else
		{
			/*
			 * Distance between parent and child
			 */
			dist = nodeInfo->nodeOut - childOffset;
		}
		writeXMLNodeOffset(dist, &childOffTarg, bwidth, false);
		childOffTarg = XNODE_PREV_REF(childOffTarg, compNode);
	}
	state->dstPos += refsTotal;
}

/*
 * As the tag content (name) length is unlimited in general (actually limited
 * by varlena type size), we may need to truncate it for log/error messages.
 *
 * input - the input text offset - where the content starts; length - actual
 * length of the content (in bytes, not always equal to the number of MB
 * characters) maxLen - maximum length to be printed out (in MB chars)
 */
static char *
getContentToLog(char *input, unsigned int offset, unsigned int length, unsigned int maxLen)
{
	char	   *result;
	unsigned short bytes = 0;
	unsigned short chars = 0;
	char	   *c = input + offset;
	unsigned short cLen = pg_utf_mblen((unsigned char *) c);

	/*
	 * Find out how many bytes/characters match the 'length' and 'maxLen'
	 * parameters.
	 */
	while ((bytes + cLen) <= length && (chars + 1) <= maxLen)
	{
		bytes += cLen;
		chars++;
		c += cLen;
		cLen = pg_utf_mblen((unsigned char *) c);
	}

	if (bytes == 0)
	{
		elog(ERROR, "unable to get string suitable for logging");
	}

	/*
	 * There has to be some space for terminating NULL character, as well as
	 * that for (possible) dots.
	 */
	result = (char *) palloc(bytes + 4);

	strncpy(result, input + offset, bytes);
	if (bytes < length)
	{
		unsigned short i;

		for (i = 0; i < 3; i++)
		{
			result[bytes + i] = XNODE_CHAR_DOT;
		}
		result[bytes + 3] = '\0';
	}
	else
	{
		result[bytes] = '\0';
	}
	return result;
}

static void
saveRootNodeHeader(XMLParserState state, XMLNodeKind kind)
{
	unsigned int i;
	unsigned int rootHdrSz;
	XMLCompNodeHdr rootNode;
	XNodeListItem *rootOffSrc;
	char	   *rootOffTarg;
	XMLNodeOffset rootNodeOff;
	XMLNodeOffset *rootNodeOffPtr;
	char	   *ptrUnaligned;
	unsigned short int childCount;
	unsigned int refsTotal;
	char		bwidth;
	unsigned int declSize = 0;
	unsigned int xntHdrSize = 0;
	unsigned int extraSize;
	char	  **paramNames = NULL;
	unsigned short paramCount = 0;
	unsigned short substNodesCount = 0;
	unsigned int padding;

	/*
	 * The 'absolute' address should be aligned. Note that 'rootNodeOff' is an
	 * offset from the first address following VARLENA header If we applied
	 * TYPEALIGN() on 'rootNodeOff', the resulting absolute address might not
	 * (in theory) aligned correctly.
	 */
	rootNodeOff = (char *) TYPEALIGN(XNODE_ALIGNOF_COMPNODE, (state->tree + state->dstPos)) -
		state->tree;

	padding = rootNodeOff - state->dstPos;
	rootHdrSz = sizeof(XMLCompNodeHdrData);

	/*
	 * If the initial call to processToken() did not fail, at least one root
	 * node must have been processed.
	 */
	Assert(state->stack.position >= 1);

	childCount = state->stack.position;
	rootOffSrc = state->stack.content;
	bwidth = getXMLNodeOffsetByteWidth(rootNodeOff - rootOffSrc->value.singleOff);
	refsTotal = childCount * bwidth;
	ensureSpaceIn(padding + rootHdrSz + refsTotal, state);
	state->dstPos += padding + rootHdrSz;

	rootNode = (XMLCompNodeHdr) (state->tree + rootNodeOff);
	rootNode->common.flags = 0;
	XNODE_SET_REF_BWIDTH(rootNode, bwidth);
	rootNode->common.kind = kind;
	rootNode->children = childCount;

	/*
	 * Not using xmlnodePop() on purpose. We need to get the nodes in the
	 * correct order.
	 */
	rootOffTarg = XNODE_FIRST_REF(rootNode);
	for (i = 0; i < childCount; i++)
	{
		XMLNodeOffset dist = rootNodeOff - rootOffSrc->value.singleOff;

		writeXMLNodeOffset(dist, &rootOffTarg, bwidth, true);
		rootOffSrc++;
	}
	state->dstPos += refsTotal;

	if (kind == XMLNODE_DOC && state->decl != NULL)
	{
		/* No padding needed for the current version. */
		declSize = sizeof(XMLDeclData);
	}

	if (state->targetKind == XNTNODE_ROOT)
	{
		XMLNodeContainer paramNameCont = &state->paramNames;
		XMLNodeContainer substNodesCont = &state->substNodes;

		if (paramNameCont->position > 0)
		{
			XNodeListItem *lItem = paramNameCont->content;
			unsigned short i;

			paramCount = paramNameCont->position;
			paramNames = (char **) palloc(paramCount * sizeof(char *));
			for (i = 0; i < paramCount; i++)
			{
				char	   *parName = lItem->value.singlePtr;

				xntHdrSize += strlen(parName) + 1;
				paramNames[i] = parName;
				lItem++;
			}
		}
		xntHdrSize += MAX_PADDING(XNODE_ALIGNOF_XNT_HDR) + sizeof(XNTHeaderData);

		substNodesCount = substNodesCont->position;
		xntHdrSize += MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET) + substNodesCount * sizeof(XMLNodeOffset);
	}

	extraSize = declSize + xntHdrSize;
	ensureSpaceIn(extraSize + MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET) + sizeof(XMLNodeOffset), state);
	/* re-initialize the 'rootNode', re-allocation might have taken place. */
	rootNode = (XMLCompNodeHdr) (state->tree + rootNodeOff);

	if (declSize > 0)
	{
		memcpy(state->tree + state->dstPos, state->decl, declSize);
		state->dstPos += declSize;
		rootNode->common.flags |= XNODE_DOC_XMLDECL;
	}

	if (xntHdrSize > 0)
	{
		XNTHeaderData hdr;
		char	   *dst,
				   *dstAligned;

		hdr.paramCount = paramCount;
		hdr.substNodesCount = substNodesCount;

		dst = state->tree + state->dstPos;
		dstAligned = (char *) TYPEALIGN(XNODE_ALIGNOF_XNT_HDR, dst);
		padding = dstAligned - dst;

		memcpy(dstAligned, &hdr, sizeof(XNTHeaderData));
		state->dstPos += padding + sizeof(XNTHeaderData);

		if (paramCount > 0)
		{
			unsigned short i;

			for (i = 0; i < paramCount; i++)
			{
				strcpy(state->tree + state->dstPos, paramNames[i]);
				state->dstPos += strlen(paramNames[i]) + 1;
			}
			pfree(paramNames);
		}

		if (substNodesCount > 0)
		{
			XMLNodeContainer substNodesCont = &state->substNodes;
			XNodeListItem *item = substNodesCont->content;
			unsigned short i;
			XMLNodeOffset *offPtr;

			ptrUnaligned = state->tree + state->dstPos;
			offPtr = (XMLNodeOffset *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, ptrUnaligned);
			padding = (char *) offPtr - ptrUnaligned;

			for (i = 0; i < substNodesCount; i++)
			{
				*offPtr = item->value.singleOff;
				offPtr++;
				item++;
			}
			state->dstPos += padding + substNodesCount * sizeof(XMLNodeOffset);
		}
	}

	ptrUnaligned = state->tree + state->dstPos;
	rootNodeOffPtr = (XMLNodeOffset *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, ptrUnaligned);
	padding = (char *) rootNodeOffPtr - ptrUnaligned;

	*rootNodeOffPtr = ((char *) rootNode - state->tree);
	state->dstPos += padding + sizeof(XMLNodeOffset);
	SET_VARSIZE(state->result, state->dstPos + VARHDRSZ);
}

void
xmlnodeDumpNode(char *input, XMLNodeOffset nodeOff, StringInfo output, char **paramNames, bool terminate)
{
	char	   *content = NULL;
	unsigned int cntLen = 0;
	XMLNodeHdr	node = (XMLNodeHdr) (input + nodeOff);
	char	   *cursor;

	switch (node->kind)
	{
			unsigned short int i,
						incr;
			char	   *childOffPtr,
					   *lastChild;

		case XMLNODE_DOC:
		case XMLNODE_ELEMENT:

		case XNTNODE_ROOT:
		case XNTNODE_TEMPLATE:
		case XNTNODE_COPY_OF:
		case XNTNODE_ELEMENT:
		case XNTNODE_ATTRIBUTE:
			if (node->kind == XMLNODE_ELEMENT || (node->flags & XNODE_EL_SPECIAL))
			{
				XMLCompNodeHdr element = (XMLCompNodeHdr) node;

				if (node->flags & XNODE_EL_SPECIAL)
				{
					content = getXNTNodeName(node->kind);
				}
				else
				{
					content = XNODE_ELEMENT_NAME(element);
				}
				cntLen = strlen(content);
				cursor = ensureSpaceOut(output, cntLen + 1);

				/*
				 * STag
				 */
				*cursor++ = XNODE_CHAR_LARROW;
				memcpy(cursor, content, cntLen);
				/* '<' + tag name  written so far */
			}

			childOffPtr = XNODE_FIRST_REF(((XMLCompNodeHdr) node));

			if (node->kind == XMLNODE_ELEMENT || (node->flags & XNODE_EL_SPECIAL))
			{
				XMLCompNodeHdr eh = (XMLCompNodeHdr) node;

				i = dumpAttributes(eh, output, paramNames);
				childOffPtr = childOffPtr + i * XNODE_GET_REF_BWIDTH(eh);

				if (node->flags & XNODE_EMPTY)
				{
					/*
					 * EmptyElement
					 */
					cursor = ensureSpaceOut(output, 2);
					*cursor++ = XNODE_CHAR_SLASH;
					*cursor = XNODE_CHAR_RARROW;
				}
				else
				{
					char	   *lastChild = XNODE_LAST_REF((XMLCompNodeHdr) node);

					cursor = ensureSpaceOut(output, 1);
					*cursor = XNODE_CHAR_RARROW;

					while (childOffPtr <= lastChild)
					{
						xmlnodeDumpNode(input, nodeOff - readXMLNodeOffset(&childOffPtr,
																		   XNODE_GET_REF_BWIDTH(eh), true), output, paramNames, false);
					}

					/*
					 * Etag
					 */
					cursor = ensureSpaceOut(output, cntLen + 3);
					*cursor++ = XNODE_CHAR_LARROW;
					*cursor++ = XNODE_CHAR_SLASH;
					memcpy(cursor, content, cntLen);
					cursor += cntLen;
					*cursor = XNODE_CHAR_RARROW;
					/* '</' + 'Name' + '>' */
				}

				if (node->flags & XNODE_EL_SPECIAL)
				{
					/*
					 * 'content' is the constructed full name as opposed to
					 * pointer to the binary tree;
					 */
					pfree(content);
				}
			}
			else
			{
				/*
				 * This is a document node.
				 */
				char	   *lastChild = XNODE_LAST_REF((XMLCompNodeHdr) node);

				while (childOffPtr <= lastChild)
				{
					xmlnodeDumpNode(input, nodeOff -
									readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH((XMLCompNodeHdr) node), true),
									output, paramNames, false);
				}
			}
			break;

		case XMLNODE_DTD:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);

			cursor = ensureSpaceOut(output, incr = strlen(specXMLStrings[XNODE_STR_DTD_START]));
			memcpy(cursor, specXMLStrings[XNODE_STR_DTD_START], incr);

			cursor = ensureSpaceOut(output, cntLen);
			memcpy(cursor, content, cntLen);

			cursor = ensureSpaceOut(output, incr = strlen(specXMLStrings[XNODE_STR_DTD_END]));
			memcpy(cursor, specXMLStrings[XNODE_STR_DTD_END], incr);
			break;

		case XMLNODE_ATTRIBUTE:
			content = XNODE_CONTENT(node);

			/* Skip the name. */
			content += strlen(content) + 1;

			cntLen = strlen(content);
			cursor = ensureSpaceOut(output, cntLen);
			memcpy(cursor, content, cntLen);
			break;

		case XMLNODE_COMMENT:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);
			cursor = ensureSpaceOut(output, incr = strlen(specXMLStrings[XNODE_STR_CMT_START]));
			memcpy(cursor, specXMLStrings[XNODE_STR_CMT_START], incr);

			cursor = ensureSpaceOut(output, cntLen);
			memcpy(cursor, content, cntLen);

			cursor = ensureSpaceOut(output, incr = strlen(specXMLStrings[XNODE_STR_CMT_END]));
			memcpy(cursor, specXMLStrings[XNODE_STR_CMT_END], incr);
			break;

		case XMLNODE_PI:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);
			incr = strlen(specXMLStrings[XNODE_STR_PI_START]);

			cursor = ensureSpaceOut(output, incr);
			memcpy(cursor, specXMLStrings[XNODE_STR_PI_START], incr);

			cursor = ensureSpaceOut(output, cntLen);
			memcpy(cursor, content, cntLen);

			if (node->flags & XNODE_PI_HAS_VALUE)
			{
				content += cntLen + 1;
				cntLen = strlen(content);
				cursor = ensureSpaceOut(output, cntLen + 1);
				*cursor++ = ' ';
				memcpy(cursor, content, cntLen);
			}
			incr = strlen(specXMLStrings[XNODE_STR_PI_END]);
			cursor = ensureSpaceOut(output, incr);
			memcpy(cursor, specXMLStrings[XNODE_STR_PI_END], incr);
			break;

		case XMLNODE_CDATA:
		case XMLNODE_TEXT:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);

			if (node->flags & XNODE_TEXT_SPEC_CHARS)
			{
				dumpContentEscaped(node->kind, output, content, cntLen);
			}
			else
			{
				cursor = ensureSpaceOut(output, cntLen);
				memcpy(cursor, content, cntLen);
			}
			break;

		case XMLNODE_DOC_FRAGMENT:
			childOffPtr = XNODE_FIRST_REF(((XMLCompNodeHdr) node));
			lastChild = XNODE_LAST_REF((XMLCompNodeHdr) node);
			while (childOffPtr <= lastChild)
			{
				XMLCompNodeHdr eh = (XMLCompNodeHdr) node;
				XMLNodeOffset offRel = readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH(eh), true);

				xmlnodeDumpNode(input, nodeOff - offRel, output, paramNames, false);
			}
			break;

		default:
			elog(ERROR, "unable to dump node: %u", node->kind);
			break;
	}

	if (terminate)
	{
		cursor = ensureSpaceOut(output, 1);
		*cursor = '\0';
	}
}

/*
 * Dumps attributes of 'element' and returns their count.
 */
static unsigned int
dumpAttributes(XMLCompNodeHdr element,
			   StringInfo output, char **paramNames)
{

	unsigned int i = 0;
	char	   *childOffPtr = XNODE_FIRST_REF(element);

	for (i = 0; i < element->children; i++)
	{
		XMLNodeOffset attrOffset = readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH(element), true);
		XMLNodeHdr	attrNode;
		char	   *attrName = NULL;
		char	   *attrValue = NULL;
		unsigned int attrNameLen,
					attrValueLen;
		char		qMark;
		bool		isSpecialAttr = false;
		bool		valueCopy = false;
		char	   *cursor;

		if (attrOffset == XMLNodeOffsetInvalid)
		{
			/* Empty slot for an optional attribute. */
			continue;
		}

		attrNode = (XMLNodeHdr) ((char *) element - attrOffset);
		if (attrNode->kind != XMLNODE_ATTRIBUTE)
		{
			break;
		}

		if (element->common.flags & XNODE_EL_SPECIAL)
		{
			/*
			 * Special node shouldn't have other-than-special attributes too
			 * often, so it's not wasting time if we always assume that this
			 * attribute is special. The worst case is that we receive NULL,
			 * indicating that it's an ordinary attribute.
			 */
			attrName = getXNTAttributeName(element->common.kind, i);
			if (attrName != NULL)
			{
				isSpecialAttr = true;
			}
		}
		if (!isSpecialAttr)
		{
			attrName = XNODE_CONTENT(attrNode);
		}

		qMark = (attrNode->flags & XNODE_ATTR_APOSTROPHE) ? XNODE_CHAR_APOSTR : XNODE_CHAR_QUOTMARK;

		attrNameLen = strlen(attrName);

		cursor = ensureSpaceOut(output, attrNameLen + 3);
		*cursor++ = XNODE_CHAR_SPACE;
		memcpy(cursor, attrName, attrNameLen);
		cursor += attrNameLen;
		*cursor++ = XNODE_CHAR_EQ;
		*cursor = qMark;

		if (isSpecialAttr)
		{
			/*
			 * Special attribute has no name stored, value immediately follows
			 * the header.
			 */
			attrValue = XNODE_CONTENT(attrNode);

			if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
			{
				if (element->common.kind == XNTNODE_COPY_OF && i == XNT_COPY_OF_EXPR)
				{
					XPathExpression xpExpr = (XPathExpression) attrValue;
					StringInfoData output;

					xnodeInitStringInfo(&output, 32);
					dumpXPathExpression(xpExpr, NULL, &output, true, paramNames, false);
					attrValue = output.data;
					valueCopy = true;
				}
				else if ((element->common.kind == XNTNODE_ELEMENT && i == XNT_ELEMENT_NAME) ||
						 (element->common.kind == XNTNODE_ATTRIBUTE &&
					  (i == XNT_ATTRIBUTE_NAME || i == XNT_ATTRIBUTE_VALUE)))
				{
					attrValue = dumpBinaryAttrValue(attrValue, paramNames, NULL, NULL, NULL);
					valueCopy = true;
				}
				else
				{
					elog(ERROR, "element of kind %u has unrecognized special attribute %u",
						 element->common.kind, i);
				}
			}
		}
		else
		{
			attrValue = attrName + attrNameLen + 1;

			/*
			 * Ordinary element can have binary (tokenized) value too. This
			 * happens when it's used in a template.
			 */
			if (attrNode->flags & XNODE_ATTR_VALUE_BINARY)
			{
				attrValue = dumpBinaryAttrValue(attrValue, paramNames, NULL, NULL, NULL);
				valueCopy = true;
			}
		}
		attrValueLen = strlen(attrValue);

		if (attrNode->flags & XNODE_ATTR_CONTAINS_REF)
		{
			dumpContentEscaped(XMLNODE_ATTRIBUTE, output, attrValue, attrValueLen);
		}
		else
		{
			cursor = ensureSpaceOut(output, attrValueLen);
			memcpy(cursor, attrValue, attrValueLen);
		}

		if (valueCopy)
		{
			pfree(attrValue);
		}
		cursor = ensureSpaceOut(output, 1);
		*cursor = qMark;
	}
	return i;
}

static void
dumpContentEscaped(XMLNodeKind kind, StringInfo output, char *input, unsigned int inputLen)
{

	unsigned int i = 0;

	while (i < inputLen)
	{
		unsigned char j;
		unsigned int incrInput;
		bool		special = false;

		if (kind == XMLNODE_TEXT || kind == XMLNODE_ATTRIBUTE)
		{
			for (j = 0; j < XNODE_PREDEFINED_ENTITIES; j++)
			{
				if (*input == predefEntities[j].simple)
				{
					char	   *escaped = predefEntities[j].escaped;

					special = true;
					dumpSpecString(output, escaped, &incrInput);
					break;
				}
			}
		}
		else if (kind == XMLNODE_CDATA)
		{
			char	   *outStr = NULL;

			switch (*input)
			{
				case XNODE_CHAR_LARROW:
					outStr = XNODE_CHAR_CDATA_LT;
					special = true;
					break;

				case XNODE_CHAR_RARROW:
					outStr = XNODE_CHAR_CDATA_GT;
					special = true;
					break;

				case XNODE_CHAR_AMPERSAND:
					outStr = XNODE_CHAR_CDATA_AMP;
					special = true;
					break;
			}

			if (special)
			{
				dumpSpecString(output, outStr, &incrInput);
			}
		}
		else
		{
			elog(ERROR, "unexpected node kind %u", kind);
		}

		if (!special)
		{
			char	   *cursor;

			incrInput = pg_utf_mblen((unsigned char *) input);
			cursor = ensureSpaceOut(output, incrInput);
			memcpy(cursor, input, incrInput);
		}
		i += incrInput;
		input += incrInput;
	}
}

static void
dumpSpecString(StringInfo output, char *outNew, unsigned int *incrInput)
{
	unsigned short len = strlen(outNew);
	char	   *cursor = ensureSpaceOut(output, len + 1);

	*cursor++ = XNODE_CHAR_AMPERSAND;
	memcpy(cursor, outNew, len);
	*incrInput = 1;
}

/*
 * This function is used instead of stringinfo.c:enlargeStringInfo()
 * The reason is that we must expect large documents and be able to tune
 * the amount of memory added. enlargeStringInfo() does not provide any
 * flexibility - it always allocates twice the original size.
 *
 * 'toWrite' is the nuber of bytes the caller is going to add.
 *
 * Returns a pointer where the data can be written. On return the 'output->len'
 * contains the 'toWrite' size.
 */
static char *
ensureSpaceOut(StringInfo output, unsigned int toWrite)
{
	char	   *result;

	if (output->len + toWrite > output->maxlen)
	{
		unsigned int increment;

		/* Try to add a fraction of the original size. */
		increment = output->maxlen >> 2;
		/* If the original size was too low, add it whole again. */
		if (increment == 0)
		{
			increment = output->maxlen;
		}

		output->maxlen += increment;
		output->data = (char *) repalloc(output->data, output->maxlen);
		elog(DEBUG1, "output memory to dump XML data increased from %u to %u bytes",
			 output->maxlen - increment, output->maxlen);
	}
	result = output->data + output->len;
	output->len += toWrite;
	return result;
}

/*
 * Write node offset into a character array. Little-endian byte ordering is
 * used, regardless the actual platform endianness.
 *
 * ref - the value to be written.
 * outPtr - where pointer to the current position in the output stream is stored.
 * bytes - how many bytes the value takes. This could be computed for each 'ref' value again,
 * however all references within a particular node must have the same 'byte width'.
 * Thus we need to determine this value outside.
 * step - whether '*outPtr' should be moved so that it's ready for the next write.
 *
 * As each byte is stored separate, there's no concern about alignment.
 */

void
writeXMLNodeOffset(XMLNodeOffset ref, char **outPtr, unsigned char bytes, bool step)
{
	unsigned int mask = 0xFF;
	unsigned short int shift = 0;
	unsigned char i;
	char	   *out = *outPtr;

	Assert(bytes > 0);

	for (i = 0; i < bytes; i++)
	{
		unsigned int d = (ref & mask);

		*out = (d >> shift);
		out++;
		mask <<= 8;
		shift += 8;
	}

	if (step)
	{
		*outPtr += bytes;
	}

	/*
	 * If byte with was not sufficient, some bits will remain at higher
	 * positions and this test will fail.
	 */
	Assert((ref >> shift) == 0);
}

/*
 * Return node offset value from '*input' char array where value takes 'bool'
 * bytes. If 'step' is true then the '*input' value is incremented so that
 * the next value can be retrieved.
 */
XMLNodeOffset
readXMLNodeOffset(char **input, unsigned char bytes, bool step)
{
	char	   *inpTmp = *input;
	unsigned char i;
	XMLNodeOffset result = 0;
	unsigned short int shift = 0;

	for (i = 0; i < bytes; i++)
	{
		unsigned int posValue = *((unsigned char *) inpTmp);

		result += (posValue << shift);
		inpTmp++;
		shift += 8;
	}
	if (step)
	{
		*input = inpTmp;
	}
	return result;
}

char *
dumpXMLDecl(XMLDecl decl)
{
	char		qMark;
	unsigned short i = 0;
	StringInfoData outDecl;

	xnodeInitStringInfo(&outDecl, 16);
	qMark = XMLDECL_GET_QUOT_MARK(decl, i);
	appendStringInfoString(&outDecl, specXMLStrings[XNODE_STR_XDECL_START]);
	appendStringInfo(&outDecl, " %s=%c%s%c", xmldeclAttNames[XNODE_XDECL_ATNAME_VERSION],
					 qMark, xmldeclVersions[decl->version], qMark);
	if (decl->flags & XMLDECL_HAS_ENC)
	{
		i++;
		qMark = XMLDECL_GET_QUOT_MARK(decl, i);
		appendStringInfo(&outDecl, " %s=%c%s%c", xmldeclAttNames[XNODE_XDECL_ATNAME_ENCODING],
						 qMark, pg_encoding_to_char(decl->enc), qMark);
	}
	if (decl->flags & XMLDECL_HAS_SD_DECL)
	{
		i++;
		qMark = XMLDECL_GET_QUOT_MARK(decl, i);
		appendStringInfo(&outDecl, " %s=%c%s%c", xmldeclAttNames[XNODE_XDECL_ATNAME_STANDALONE],
						 qMark, XMLDECL_STANDALONE_YES, qMark);
	}
	appendStringInfoString(&outDecl, specXMLStrings[XNODE_STR_XDECL_END]);
	appendStringInfoChar(&outDecl, '\0');
	return outDecl.data;
}
