/*
 * This aims to be compliant with http://www.w3.org/TR/2008/REC-xml-20081126/
 * some time.
 *
 * The parser only scans the input string once. As the total size of children
 * needs to be known before we know where the current node can be stored
 * itself, the parser first saves child nodes.
 *
 * This 'child first' order prevents us from having to copy nodes during
 * parsing. Some functions, such as getFirstLeaf() do rely on such ordering..
 */

/*
 * TODO error handling: use 'ereport()' and define numeric error codes.
 */

#include "xml_parser.h"
#include "xmlnode_util.h"

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
}	XMLNodeToken;

/*
 * Order of these strings must follow the order of items enumerated in
 * 'XNodeSpecString'
 */
static char specStrings[][XNODE_SPEC_STR_MAX_LEN] =
{
	"<?xml", "?>",
	"<!DOCTYPE", "]>",
	"<!--", "-->",
	"<![CDATA[", "]]>",
	"<?", "?>",
};

/*
 * Likewise, order of the DTD keywords must follow the order of
 * 'XNodeSpecStringDTD' items.
 */
static char specStringsDTD[][XNODE_SPEC_STR_MAX_LEN] =
{
	"ELEMENT", "ATTLIST", "ENTITY",
	"NOTATION", "SYSTEM", "PUBLIC",
	"EMPTY", "ANY", "#PCDATA",
	"NDATA", "#REQUIRED", "#IMPLIED", "#FIXED"
};

static char dtdAttTypes[][XNODE_SPEC_STR_MAX_LEN] = {
	"CDATA", "ID", "IDREF", "IDREFS", "ENTITY", "ENTITIES", "NMTOKEN", "NMTOKENS"
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
	 * included)
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
}	XMLNodeInternalData;

typedef struct XMLNodeInternalData *XMLNodeInternal;

static void processToken(XMLParserState state, XMLNodeInternal nodeInfo, XMLNodeToken allowed);
static XMLNodeToken processTag(XMLParserState state, XMLNodeInternal nodeInfo, XMLNodeToken allowed,
		   XMLNodeHeader * declAttrs, unsigned short *declAttrNum);
static void checkXMLDeclaration(XMLNodeHeader * declAttrs, unsigned int attCount, XMLDecl decl);
static char *getEncodingSimplified(const char *original);
static void readName(XMLParserState state, bool whitespace);
static unsigned int readComment(XMLParserState state);
static unsigned int readPI(XMLParserState state);
static inline void readWhitespace(XMLParserState state, bool optional);
static void readDTD_CP(XMLParserState state);
static void readDTD_ChoiceOrSeq(XMLParserState state, bool started);
static void readDTDExternalID(XMLParserState state);
static void readDTDEntityValue(XMLParserState state);
static char *readAttValue(XMLParserState state, bool output, bool *refs);
static bool readReference(XMLParserState state, pg_wchar *value);
static bool isPredefinedEntity(char *refStart, char *value);
static void readDTD(XMLParserState state);
static void processDTDNode(XMLParserState state);
static void processLiteral(XMLParserState state, bool public);
static bool readSpecialStringPart(char specStrings[][XNODE_SPEC_STR_MAX_LEN], XNodeSpecString strIndex,
					  XMLParserState state, char offset);
static bool readSpecialString(char specStrings[][XNODE_SPEC_STR_MAX_LEN], XNodeSpecString strIndex,
				  XMLParserState state);
static void evaluateWhitespace(XMLParserState state);
static void nextChar(XMLParserState state, bool endAllowed);
static void ensureSpace(unsigned int size, XMLParserState state);
static void saveNodeHeader(XMLParserState state, XMLNodeInternal nodeInfo, char flags);
static void saveContent(XMLParserState state, XMLNodeInternal nodeInfo);
static void saveReferences(XMLParserState state, XMLNodeInternal nodeInfo, XMLElementHeader element,
			   unsigned short int children);
static char *getContentToLog(char *input, unsigned int offset,
				unsigned int length, unsigned int maxLen);
static void saveRootNodeHeader(XMLParserState state, XMLNodeKind kind);
static unsigned int dumpAttributes(XMLElementHeader element, char *input,
			   char **output, unsigned int *pos);
static void dumpContentEscaped(XMLNodeKind kind, char **output, char *input, unsigned int inputLen,
				   unsigned int *outPos);
static void dumpSpecString(char **output, char *outNew, unsigned int *outPos, unsigned int *incrInput);

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

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Char
 */
#define CHAR_INTERVALS		3

static UTF8Interval charIntervals[CHAR_INTERVALS] =
{
	{{0x21, 0x0, 0x0, 0x0}, {0xed, 0x9f, 0xbf, 0x0}},
	{{0xee, 0x80, 0x80, 0x0}, {0xef, 0xbf, 0xbd, 0x0}},
	{{0xf0, 0x90, 0x80, 0x80}, {0xf4, 0x8f, 0xbf, 0xbf}}
};

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-PubidChar (Except for
 * intervals)
 */
static char pubIdChars[] = {0x20, 0xd, 0xa, 0x2d, 0x27, 0x28, 0x29, 0x2b, 0x2c, 0x2e, 0x2f,
0x3a, 0x3d, 0x3f, 0x3b, 0x21, 0x2a, 0x23, 0x40, 0x24, 0x5f, 0x25};
static bool isPubIdChar(char c);

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
		xmlnodePush(&(state->stack), nodeInfo.nodeOut);
	}
	if ((nodeInfo.tokenType == TOKEN_XMLDECL) || ((nodeInfo.tokenType
												   & TOKEN_MISC) != 0))
	{
		do
		{
			nextChar(state, false);
			tagRow = state->srcRow;
			tagCol = state->srcCol;
			processToken(state, &nodeInfo, TOKEN_MISC | TOKEN_DTD | TOKEN_STAG);
			if (nodeInfo.tokenType != TOKEN_WHITESPACE)
			{
				xmlnodePush(&(state->stack), nodeInfo.nodeOut);
			}
		} while ((nodeInfo.tokenType & TOKEN_MISC) != 0);
	}
	if (nodeInfo.tokenType == TOKEN_DTD)
	{
		do
		{
			nextChar(state, false);
			tagRow = state->srcRow;
			tagCol = state->srcCol;
			processToken(state, &nodeInfo, TOKEN_MISC | TOKEN_DTD | TOKEN_STAG);
			if (nodeInfo.tokenType != TOKEN_WHITESPACE)
			{
				xmlnodePush(&(state->stack), nodeInfo.nodeOut);
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
	nextChar(state, true);
	if (XNODE_INPUT_END(state))
	{
		saveRootNodeHeader(state, XMLNODE_DOC);
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
			xmlnodePush(&(state->stack), nodeInfo.nodeOut);
		}
		nextChar(state, true);
	} while (!XNODE_INPUT_END(state));

	saveRootNodeHeader(state, XMLNODE_DOC);
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
	xmlnodePush(&state->stack, nodeInfo.nodeOut);
	nextChar(state, true);

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
				nextChar(state, true);
			} while (!XNODE_INPUT_END(state) &&
					 (nodeInfo.tokenType & (TOKEN_TEXT | TOKEN_REFERENCE)));

			/*
			 * Ordinary node (no char or reference), treat it as a separate
			 * node. We might be at the end, but don't have to.
			 */
			if (!(nodeInfo.tokenType & (TOKEN_TEXT | TOKEN_REFERENCE)))
			{
				xmlnodePush(&state->stack, nodeInfo.nodeOut);
			}
			if (!XNODE_INPUT_END(state))
			{
				state->saveHeader = true;
			}
			if (entPredef)
			{
				XMLNodeHeader node = (XMLNodeHeader) (state->tree + textStart);

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
			xmlnodePush(&state->stack, nodeInfo.nodeOut);
			nextChar(state, true);

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
		XMLNodeHeader node = (XMLNodeHeader) (state->tree + nodeInfo.nodeOut);

		node->flags |= XNODE_TEXT_SPEC_CHARS;
	}
	if (state->stack.position == 1)
	{
		XMLNodeOffset *rootOffPtr;

		ensureSpace(sizeof(XMLNodeOffset), state);
		rootOffPtr = (XMLNodeOffset *) (state->tree + state->dstPos);
		*rootOffPtr = xmlnodePop(&state->stack);
		state->dstPos += sizeof(XMLNodeOffset);
		SET_VARSIZE(state->result, state->dstPos + VARHDRSZ);
	}
	else
	{
		saveRootNodeHeader(state, XMLNODE_DOC_FRAGMENT);
	}
}

void
initXMLParser(XMLParserState state, char *inputText)
{

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

	state->sizeOut = state->sizeIn + (state->sizeIn >> XNODE_OUT_OVERHEAD_BITS);
	state->sizeOut = (state->sizeOut > XNODE_PARSER_OUTPUT_MIN) ? state->sizeOut
		: XNODE_PARSER_OUTPUT_MIN;
	elog(DEBUG1, "source xml size: %u, binary xml size (initial estimate): %u", state->sizeIn,
		 state->sizeOut);
	state->result = (char *) palloc(state->sizeOut + VARHDRSZ);
	state->tree = state->result + VARHDRSZ;
	xmlnodeContainerInit(&state->stack);
	state->decl = NULL;
}

void
finalizeXMLParser(XMLParserState state)
{
	xmlnodeContainerFree(&state->stack);
	if (state->decl != NULL)
	{
		pfree(state->decl);
		state->decl = NULL;
	}
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
			if (readReference(state, &value))
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
				ensureSpace(nodeInfo->cntLength + 1, state);
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
				ensureSpace(nodeInfo->cntLength + 1, state);
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
							 specStrings[XNODE_STR_CDATA_END]);
					}
				}
				if (!XNODE_VALID_CHAR(state->c))
				{
					elog(ERROR, "invalid XML character at row %u, column %u", state->srcRow, state->srcCol);
				}
				nextChar(state, false);
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
	nextChar(state, false);
	if (*state->c == XNODE_CHAR_QUESTMARK)
	{
		char	   *declStart = specStrings[XNODE_STR_XDECL_START];

		nextChar(state, false);
		if (strncmp(state->c, declStart + 2, strlen(declStart + 2)) == 0 &&
			XNODE_WHITESPACE(state->c + strlen(declStart + 2)))
		{
			XMLNodeHeader declAttrs[XNODE_XDECL_MAX_ATTRS];
			unsigned short declAttNum;

			if (!(allowed & TOKEN_XMLDECL))
			{
				elog(ERROR, "XML declaration not allowed at row %u, column %u", tagRow, tagCol);
			}
			processTag(state, nodeInfo, TOKEN_XMLDECL, declAttrs, &declAttNum);
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
			XMLNodeHeader piNode;

			if (!(allowed & TOKEN_PI))
			{
				elog(ERROR, "Processing instruction not allowed at row %u, column %u", tagRow, tagCol);
			}
			nodeInfo->cntSrc = state->srcPos;
			nodeInfo->nodeOut = state->dstPos;
			piNodeOff = nodeInfo->nodeOut;
			nodeInfo->tokenType = TOKEN_PI;
			saveNodeHeader(state, nodeInfo, 0);

			cntLen = readPI(state);
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
			piNode = (XMLNodeHeader) (state->tree + piNodeOff);
			piNode->flags = XNODE_PI_HAS_VALUE;
			return;
		}
	}
	else if (*state->c == XNODE_CHAR_EXCLMARK)
	{
		unsigned char charsProcessed;

		nextChar(state, false);

		/*
		 * Left arrow followed by exclamation mark
		 */
		charsProcessed = 2;

		if (readSpecialStringPart(specStrings, XNODE_STR_DTD_START, state, charsProcessed))
		{
			if (!(allowed & TOKEN_DTD))
			{
				elog(ERROR, "DTD not allowed here, see row %u, column %u", tagRow, tagCol);
			}
			nodeInfo->cntSrc = state->srcPos;
			nodeInfo->nodeOut = state->dstPos;
			readDTD(state);
			nodeInfo->cntLength = state->srcPos - nodeInfo->cntSrc - 1;
			nodeInfo->tokenType = TOKEN_DTD;
			saveNodeHeader(state, nodeInfo, 0);
			saveContent(state, nodeInfo);
			return;
		}
		else if (readSpecialStringPart(specStrings, XNODE_STR_CMT_START, state, charsProcessed))
		{
			if (!(allowed & TOKEN_COMMENT))
			{
				elog(ERROR, "Comment not allowed here, see row %u, column %u", tagRow, tagCol);
			}
			nodeInfo->cntSrc = state->srcPos;
			nodeInfo->nodeOut = state->dstPos;
			nodeInfo->cntLength = readComment(state);
			nodeInfo->tokenType = TOKEN_COMMENT;
			saveNodeHeader(state, nodeInfo, 0);
			saveContent(state, nodeInfo);
			return;
		}
		else if (readSpecialStringPart(specStrings, XNODE_STR_CDATA_START, state, charsProcessed))
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
				nextChar(state, false);
			}
			nodeInfo->cntLength = state->srcPos - nodeInfo->cntSrc;
			for (i = 0; i < strlen(specStrings[XNODE_STR_CDATA_END]) - 1; i++)
			{
				nextChar(state, false);
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
		nextChar(state, false);
		if (!XNODE_VALID_NAME_START(state->c))
		{
			elog(ERROR, "Invalid tag name at row %u, column %u.", tagRow, tagCol);
		}

		/*
		 * If a valid ETag name starts here, we need to record the starting
		 * position.
		 */
		processTag(state, nodeInfo, TOKEN_ETAG, NULL, NULL);
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

		if (!(allowed & TOKEN_STAG))
		{
			elog(ERROR, "element not allowed at row %u, column %u.", tagRow, tagCol);
		}
		if (!XNODE_VALID_NAME_START(state->c))
		{
			elog(ERROR, "Invalid tag name at row %u, column %u.", tagRow, tagCol);
		}
		tagType = processTag(state, nodeInfo, TOKEN_STAG | TOKEN_EMPTY_ELEMENT, NULL, NULL);

		/*
		 * The initial number of children equals to number of attributes
		 */
		attributes = children = state->stack.position - stackPosOrig;

		if (tagType == TOKEN_EMPTY_ELEMENT)
		{
			XMLElementHeader element;

			/*
			 * In this case 'child' always means 'attribute'
			 */
			nodeInfo->nodeOut = state->dstPos;
			nodeInfo->tokenType = TOKEN_EMPTY_ELEMENT;

			element = (XMLElementHeader) (state->tree + nodeInfo->nodeOut);
			saveNodeHeader(state, nodeInfo, XNODE_ELEMENT_EMPTY);
			element->children = children;
			saveReferences(state, nodeInfo, element, children);
			saveContent(state, nodeInfo);
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
			XMLElementHeader element;
			XMLNodeHeader firstText = NULL;

			state->saveHeader = true;

			/*
			 * Process children
			 */
			do
			{
				nextChar(state, false);
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
						firstText = (XMLNodeHeader) (state->tree + childTag.nodeOut);
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
						xmlnodePush(&state->stack, childTag.nodeOut);
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
			element = (XMLElementHeader) (state->tree + nodeInfo->nodeOut);
			saveNodeHeader(state, nodeInfo, children == attributes ? XNODE_ELEMENT_EMPTY : 0);
			element->children = children;
			if (children > 0)
			{
				saveReferences(state, nodeInfo, element, children);
			}
			saveContent(state, nodeInfo);
			return;
		}
	}
	elog(ERROR, "Unrecognized tag at row %u, column %u.", tagRow, tagCol);
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Name 'whitespace' indicates
 * whether a whitespace is expected right after the name.
 */
static void
readName(XMLParserState state, bool whitespace)
{
	if (!XNODE_VALID_NAME_START(state->c))
	{
		UNEXPECTED_CHARACTER;
	}
	do
	{
		nextChar(state, false);
	} while (XNODE_VALID_NAME_CHAR(state->c));
	if (whitespace)
	{
		if (!XNODE_WHITESPACE(state->c))
		{
			elog(ERROR, "Whitespace expected at row %u, column %u.", state->srcRow, state->srcCol);
		}
		else
		{
			nextChar(state, false);
		}
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
 * Both 'declAttrs' and 'declAttrNum' must be not-NULL if XML declaration is
 * to be parsed
 */

static XMLNodeToken
processTag(XMLParserState state, XMLNodeInternal nodeInfo, XMLNodeToken allowed,
		   XMLNodeHeader * declAttrs, unsigned short *declAttrNum)
{

	bool		mustEnd = false;
	unsigned short attributes = 0;
	unsigned int stackInit = state->stack.position;

	unsigned int declAttrFirst = 0;

	/*
	 * We're at the first character of the name.
	 */
	nodeInfo->cntSrc = state->srcPos;
	readName(state, false);
	nodeInfo->cntLength = state->srcPos - nodeInfo->cntSrc;

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
				nextChar(state, false);
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
				nextChar(state, false);
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
							declAttrs[i - 1] = (XMLNodeHeader) (state->tree + xmlnodePop(&state->stack));
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

			nextChar(state, false);

			/*
			 * Process a single attribute
			 * (http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Att ribute)
			 */
			if (XNODE_VALID_NAME_START(state->c))
			{
				unsigned int nameStart,
							nameLength;
				XMLNodeHeader attrNode;
				XMLNodeOffset *stackItems;
				unsigned short int i;
				char	   *attrName,
						   *attrValue;
				bool		refsInValue;

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
				nameStart = state->srcPos;
				readName(state, false);
				nameLength = state->srcPos - nameStart;

				if (XNODE_WHITESPACE(state->c))
				{
					nextChar(state, false);
				}
				if (*state->c != XNODE_CHAR_EQ)
				{
					elog(ERROR, "'%c' expected at row %u, column %u.", XNODE_CHAR_EQ,
						 state->srcRow, state->srcCol);
				}
				else
				{
					nextChar(state, false);
				}
				if (XNODE_WHITESPACE(state->c))
				{
					nextChar(state, false);
				}

				/*
				 * Save attribute, the name first.
				 *
				 * Let's store the attributes, whether the node is XML
				 * declaration or not. Even if it's the declaration, we store
				 * the names&values to the output array.
				 *
				 */

				xmlnodePush(&state->stack, state->dstPos);
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
				ensureSpace(sizeof(XMLNodeHeaderData) + nameLength + 1, state);
				attrNode = (XMLNodeHeader) (state->tree + state->dstPos);
				attrNode->kind = XMLNODE_ATTRIBUTE;
				attrNode->flags = 0;
				state->dstPos += sizeof(XMLNodeHeaderData);
				attrName = state->tree + state->dstPos;
				memcpy(attrName, state->inputText + nameStart, nameLength);
				*(state->tree + state->dstPos + nameLength) = '\0';
				state->dstPos += nameLength + 1;

				/*
				 * Is the attribute name unique?
				 */
				stackItems = &(state->stack.items[stackInit]);
				for (i = 0; i < attributes; i++)
				{
					char	   *nameOld;
					XMLNodeHeader attrOld = (XMLNodeHeader) (state->tree + *stackItems);

					nameOld = (char *) attrOld + sizeof(XMLNodeHeaderData);
					stackItems++;
					if (strcmp(attrName, nameOld) == 0)
					{
						elog(ERROR, "Attribute '%s' of node '%s' is not unique.",
							 getContentToLog(state->inputText, nameStart, nameLength, 16),
							 getContentToLog(state->inputText, nodeInfo->cntSrc, nodeInfo->cntLength, 16));
					}
				}

				quotMark = *state->c;
				attrValue = readAttValue(state, true, &refsInValue);
				if (refsInValue)
				{
					attrNode->flags |= XNODE_ATTR_CONTAINS_REF;
				}
				if (quotMark == XNODE_CHAR_APOSTR)
				{
					attrNode->flags |= XNODE_ATTR_APOSTROPHE;
				}
				if (strlen(attrValue) > 0)
				{
					char	   *end;
					double		numValue;

					numValue = strtod(attrValue, &end);
					if (end == (attrValue + strlen(attrValue)))
					{
						attrNode->flags |= XNODE_ATTR_NUMBER;
					}
				}
				attributes++;
				nextChar(state, false);
			}
			else
			{
				mustEnd = true;
				continue;
			}
		}
	}
	/* keep the compiler silent */
	return 0;
}

/*
 * http://www.w3.org/TR/xml/#NT-XMLDecl
 */
static void
checkXMLDeclaration(XMLNodeHeader * declAttrs, unsigned int attCount, XMLDecl decl)
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
		XMLNodeHeader attr = declAttrs[i];

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

static unsigned int
readComment(XMLParserState state)
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
		nextChar(state, false);
	}

	if (prev == XNODE_CHAR_DASH)
	{
		elog(ERROR, "Comment must not end with %c%s", XNODE_CHAR_DASH,
			 specStrings[XNODE_STR_CMT_END]);
	}
	len = state->srcPos - startPos;

	/*
	 * The convention is to end up tag processing when '>' is the current
	 * character.
	 */
	for (i = 0; i < strlen(specStrings[XNODE_STR_CDATA_END]) - 1; i++)
	{
		nextChar(state, false);
	}
	return len;
}

static unsigned int
readPI(XMLParserState state)
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
		nextChar(state, false);
	}
	if (state->srcPos - startPos >= 3)
	{
		char	   *targNameRef = specStrings[XNODE_STR_XDECL_START] + 2;
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
		nextChar(state, false);
		while (*state->c != XNODE_CHAR_QUESTMARK)
		{
			if (!XNODE_VALID_CHAR(state->c))
			{
				elog(ERROR, "Invalid XML character at row %u, column %u",
					 state->srcRow, state->srcCol);
			}
			nextChar(state, false);
		}
	}
	len = state->srcPos - startPos;
	if (*state->c != XNODE_CHAR_QUESTMARK)
	{
		UNEXPECTED_CHARACTER;
	}
	nextChar(state, false);
	if (*state->c != XNODE_CHAR_RARROW)
	{
		UNEXPECTED_CHARACTER;
	}
	return len;
}

/*
 * TODO Reuse this code where possible
 */
static inline void
readWhitespace(XMLParserState state, bool optional)
{
	if (XNODE_WHITESPACE(state->c))
	{
		nextChar(state, false);
	}
	else if (!optional)
	{
		elog(ERROR, "whitespace expected at row %u, column %u", state->srcRow, state->srcCol);
	}
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-cp
 */
static void
readDTD_CP(XMLParserState state)
{
	if (XNODE_VALID_NAME_START(state->c))
	{
		readName(state, false);
	}
	else
	{
		readDTD_ChoiceOrSeq(state, false);
	}
	if (*state->c == XNODE_CHAR_QUESTMARK || *state->c == XNODE_CHAR_ASTERISK ||
		*state->c == XNODE_CHAR_PLUS)
	{
		nextChar(state, false);
	}
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-choice or
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-seq
 */
static void
readDTD_ChoiceOrSeq(XMLParserState state, bool started)
{
	unsigned int i = 0;
	char		delim = 0x0;

	if (!started)
	{
		if (*state->c != XNODE_CHAR_LBRKT_RND)
		{
			UNEXPECTED_CHARACTER;
		}
		nextChar(state, false);
		readWhitespace(state, true);
	}
	readDTD_CP(state);
	readWhitespace(state, true);
	while (*state->c != XNODE_CHAR_RBRKT_RND)
	{
		if (i == 0)
		{
			if (*state->c != XNODE_CHAR_PIPE && *state->c != XNODE_CHAR_COMMA)
			{
				elog(ERROR, "'%c' or '%c' expected at row %u, column %u", XNODE_CHAR_PIPE, XNODE_CHAR_COMMA,
					 state->srcRow, state->srcCol);
			}
			delim = *state->c;
		}
		else
		{
			if (*state->c != delim)
			{
				elog(ERROR, "'%c' or '%c' expected at row %u, column %u", delim, XNODE_CHAR_RBRKT_RND,
					 state->srcRow, state->srcCol);
			}
		}
		nextChar(state, false);
		readWhitespace(state, true);
		readDTD_CP(state);
		readWhitespace(state, true);
		i++;
	}
	nextChar(state, false);
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-ExternalID
 */
static void
readDTDExternalID(XMLParserState state)
{
	if (readSpecialString(specStringsDTD, XNODE_STR_DTD_SYSTEM, state))
	{
		if (!XNODE_WHITESPACE(state->c))
		{
			UNEXPECTED_CHARACTER;
		}
		nextChar(state, false);
		processLiteral(state, false);
		nextChar(state, false);
	}
	else if (readSpecialString(specStringsDTD, XNODE_STR_DTD_PUBLIC, state))
	{
		if (!XNODE_WHITESPACE(state->c))
		{
			UNEXPECTED_CHARACTER;
		}
		nextChar(state, false);
		processLiteral(state, true);
		nextChar(state, false);
		if (!XNODE_WHITESPACE(state->c))
		{
			UNEXPECTED_CHARACTER;
		}
		nextChar(state, false);
		processLiteral(state, false);
		nextChar(state, false);
	}
}


/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-EntityValue
 */
static void
readDTDEntityValue(XMLParserState state)
{
	char		qMark = *state->c;

	nextChar(state, false);
	while (*state->c != qMark)
	{
		if (*state->c == XNODE_CHAR_PCT)
		{
			/*
			 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-PERef erence
			 */
			nextChar(state, false);
			readName(state, false);
			if (*state->c != XNODE_CHAR_SEMICOLON)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (*state->c == XNODE_CHAR_AMPERSAND)
		{
			pg_wchar	value;

			readReference(state, &value);
		}
		nextChar(state, false);
	}
	nextChar(state, false);
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-AttValue
 *
 * returns atribute value (NULL-terminated)
 */
static char *
readAttValue(XMLParserState state, bool output, bool *refs)
{
	char		qMark;
	char	   *value = state->tree + state->dstPos;

	*refs = false;
	if (*state->c != XNODE_CHAR_APOSTR && *state->c != XNODE_CHAR_QUOTMARK)
	{
		elog(ERROR, "Quotation mark or apostrophe expected at row %u, column %u.",
			 state->srcRow, state->srcCol);
	}
	qMark = *state->c;
	do
	{
		nextChar(state, false);
		if (!XNODE_VALID_CHAR(state->c))
		{
			elog(ERROR, "Invalid XML character at row %u, column %u",
				 state->srcRow, state->srcCol);
		}
		if (*state->c == XNODE_CHAR_LBRACKET)
		{
			UNEXPECTED_CHARACTER;
		}
		else if (*state->c == XNODE_CHAR_AMPERSAND)
		{
			pg_wchar	value;
			char	   *refStart = state->c;
			unsigned int row = state->srcRow;
			unsigned int col = state->srcCol;

			*refs = true;
			if (readReference(state, &value))
			{
				char		utf8char[5];

				memset(utf8char, 0, 5);
				unicode_to_utf8(value, (unsigned char *) utf8char);
				if (!XNODE_VALID_CHAR(utf8char))
				{
					elog(ERROR, "Invalid XML character reference at row %u, column %u", row, col);
				}
				if (output)
				{
					unsigned int len = strlen(utf8char);

					ensureSpace(len, state);
					memcpy(state->tree + state->dstPos, utf8char, len);
					state->dstPos += len;
				}
			}
			else
			{
				char		predefValue;

				if (!isPredefinedEntity(refStart + 1, &predefValue))
				{
					elog(ERROR, "this parser version only supports character and predefined references");
				}
				if (output)
				{
					ensureSpace(1, state);
					*(state->tree + state->dstPos) = predefValue;
					state->dstPos++;
				}
			}
		}
		else if (output && *state->c != qMark)
		{
			ensureSpace(state->cWidth, state);
			memcpy(state->tree + state->dstPos, state->c, state->cWidth);
			state->dstPos += state->cWidth;
		}
	} while (*state->c != qMark);
	if (output)
	{
		ensureSpace(1, state);
		*(state->tree + state->dstPos) = '\0';
		state->dstPos++;
	}
	return value;
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Reference
 */
static bool
readReference(XMLParserState state, pg_wchar *value)
{
	bool		charRef = false;

	nextChar(state, false);
	if (*state->c == XNODE_CHAR_HASH)
	{
		/*
		 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-CharRef
		 */
		bool		hex = false;
		unsigned int digits = 0;
		char	   *valueStart;

		charRef = true;

		nextChar(state, false);
		if (*state->c == 'x')
		{
			hex = true;
			nextChar(state, false);
		}
		valueStart = state->c;
		while ((hex && isxdigit(*state->c)) || (!hex && isdigit(*state->c)))
		{
			nextChar(state, false);
			digits++;
		}
		if (digits == 0)
		{
			elog(ERROR, "decimal or hexadecimal value expected at row %u, column %u.",
				 state->srcRow, state->srcCol);
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
		readName(state, false);
	}
	if (*state->c != XNODE_CHAR_SEMICOLON)
	{
		elog(ERROR, "'%c' expected at row %u, column %u.", XNODE_CHAR_SEMICOLON,
			 state->srcRow, state->srcCol);
	}
	return charRef;
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

static void
readDTD(XMLParserState state)
{
	if (!XNODE_WHITESPACE(state->c))
	{
		UNEXPECTED_CHARACTER;
	}
	else
	{
		nextChar(state, false);
	}
	if (!XNODE_VALID_NAME_START(state->c))
	{
		UNEXPECTED_CHARACTER;
	}
	nextChar(state, false);
	while (XNODE_VALID_NAME_CHAR(state->c))
	{
		nextChar(state, false);
	}
	if (XNODE_WHITESPACE(state->c))
	{
		nextChar(state, false);
	}
	readDTDExternalID(state);

	if (XNODE_WHITESPACE(state->c))
	{
		nextChar(state, false);
	}
	if (*state->c == XNODE_CHAR_LBRACKET)
	{
		/*
		 * http://www.w3.org/TR/xml/#NT-intSubset Including the right bracket
		 * and optional white space
		 */
		nextChar(state, false);
		while (*state->c != XNODE_CHAR_RBRACKET)
		{
			if (*state->c == XNODE_CHAR_PCT)
			{
				/*
				 * http://www.w3.org/TR/2008/REC-xml-20081126/# NT-PEReference
				 */
				nextChar(state, false);
				if (!XNODE_VALID_NAME_START(state->c))
				{
					UNEXPECTED_CHARACTER;
				}
				nextChar(state, false);
				while (XNODE_VALID_NAME_CHAR(state->c))
				{
					nextChar(state, false);
				}
				if (*state->c != XNODE_CHAR_SEMICOLON)
				{
					UNEXPECTED_CHARACTER;
				}
			}
			else if (*state->c == XNODE_CHAR_LARROW)
			{
				processDTDNode(state);
			}
			else if (!XNODE_WHITESPACE(state->c))
			{
				UNEXPECTED_CHARACTER;
			}
			nextChar(state, false);
		}
		nextChar(state, false);
		if (XNODE_WHITESPACE(state->c))
		{
			nextChar(state, false);
		}
	}
	if (*state->c != XNODE_CHAR_RARROW)
	{
		UNEXPECTED_CHARACTER;
	}
}

static void
processDTDNode(XMLParserState state)
{
	nextChar(state, false);
	if (*state->c == XNODE_CHAR_QUESTMARK)
	{
		nextChar(state, false);
		readPI(state);
	}
	else if (*state->c == XNODE_CHAR_EXCLMARK)
	{
		nextChar(state, false);
		if (readSpecialStringPart(specStrings, XNODE_STR_CMT_START, state, 2))
		{
			readComment(state);
		}
		else if (readSpecialString(specStringsDTD, XNODE_STR_DTD_ELEMENT, state))
		{
			readWhitespace(state, false);
			readName(state, false);
			nextChar(state, false);
			if (!readSpecialString(specStringsDTD, XNODE_STR_DTD_EMPTY, state) &&
				!readSpecialString(specStringsDTD, XNODE_STR_DTD_ANY, state))
			{

				if (*state->c == XNODE_CHAR_LBRKT_RND)
				{
					nextChar(state, false);
					readWhitespace(state, true);
					if (readSpecialString(specStringsDTD, XNODE_STR_DTD_PCDATA, state))
					{
						/*
						 * http://www.w3.org/TR/2008/RE
						 * C-xml-20081126/#NT-Mixed
						 */
						unsigned int names = 0;

						readWhitespace(state, true);
						while (*state->c == XNODE_CHAR_PIPE)
						{
							readWhitespace(state, true);
							nextChar(state, false);
							readName(state, false);
							names++;
							readWhitespace(state, true);
						}
						if (*state->c != XNODE_CHAR_RBRKT_RND)
						{
							UNEXPECTED_CHARACTER;
						}
						nextChar(state, false);
						if (*state->c == XNODE_CHAR_ASTERISK)
						{
							if (names == 0)
							{
								UNEXPECTED_CHARACTER;
							}
							else
							{
								nextChar(state, false);
							}
						}
					}
					else
					{
						/*
						 * http://www.w3.org/TR/2008/RE
						 * C-xml-20081126/#NT-children
						 */
						readDTD_ChoiceOrSeq(state, true);
						if (*state->c == XNODE_CHAR_QUESTMARK || *state->c == XNODE_CHAR_ASTERISK ||
							*state->c == XNODE_CHAR_PLUS)
						{
							nextChar(state, false);
						}
					}
				}
				else
				{
					UNEXPECTED_CHARACTER;
				}

			}
			if (XNODE_WHITESPACE(state->c))
			{
				nextChar(state, false);
			}
			if (*state->c != XNODE_CHAR_RARROW)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (readSpecialString(specStringsDTD, XNODE_STR_DTD_ATTLIST, state))
		{
			if (!XNODE_WHITESPACE(state->c))
			{
				UNEXPECTED_CHARACTER;
			}
			readWhitespace(state, false);
			readName(state, false);
			while (XNODE_WHITESPACE(state->c))
			{
				nextChar(state, false);
				if (XNODE_VALID_NAME_START(state->c))
				{
					/*
					 * http://www.w3.org/TR/2008/REC-xml-20 081126/#NT-AttDef
					 */
					unsigned int i,
								j,
								len;
					char	   *type;
					bool		found = false;

					readName(state, true);
					for (i = 0; i < XNODE_DTD_ATTR_TYPES; i++)
					{
						type = dtdAttTypes[i];
						len = strlen(type);
						if (strncmp(type, state->c, len) == 0)
						{
							for (j = 0; i < len; j++)
							{
								nextChar(state, false);
							}
							found = true;
							break;
						}
					}
					if (!found)
					{
						if (readSpecialString(specStringsDTD, XNODE_STR_DTD_NOTATION, state))
						{
							/*
							 * http://www.w3.org/TR /2008/REC-xml-20081
							 * 126/#NT-NotationTyp e
							 */
							readWhitespace(state, false);
							if (*state->c != XNODE_CHAR_LBRKT_RND)
							{
								UNEXPECTED_CHARACTER;
							}
							nextChar(state, false);
							readWhitespace(state, true);
							readName(state, false);
							readWhitespace(state, true);
							while (*state->c != XNODE_CHAR_RBRKT_RND)
							{
								if (*state->c != XNODE_CHAR_PIPE)
								{
									UNEXPECTED_CHARACTER;
								}
								nextChar(state, false);
								readWhitespace(state, true);
								readName(state, false);
								readWhitespace(state, true);
							}
							nextChar(state, false);

						}
						else if (*state->c == XNODE_CHAR_LBRKT_RND)
						{
							/*
							 * http://www.w3.org/TR /2008/REC-xml-20081
							 * 126/#NT-Enumeration
							 */
							unsigned int cnt;

							nextChar(state, false);
							do
							{
								readWhitespace(state, true);
								cnt = 0;
								while (XNODE_VALID_NAME_CHAR(state->c))
								{
									nextChar(state, false);
									cnt++;
								}
								if (cnt == 0)
								{
									elog(ERROR, "name token expected at row %u, column %u", state->srcRow, state->srcCol);
								}
								readWhitespace(state, true);
							} while (*state->c != XNODE_CHAR_PIPE);
							if (*state->c != XNODE_CHAR_RBRKT_RND)
							{
								UNEXPECTED_CHARACTER;
							}
							nextChar(state, false);
						}
						else
						{
							UNEXPECTED_CHARACTER;
						}
					}
					readWhitespace(state, false);

					/*
					 * http://www.w3.org/TR/2008/REC-xml-20
					 * 081126/#NT-DefaultDecl
					 */
					if (!readSpecialString(specStringsDTD, XNODE_STR_DTD_REQUIRED, state) &&
						!readSpecialString(specStringsDTD, XNODE_STR_DTD_IMPLIED, state))
					{
						bool		refs;

						if (readSpecialString(specStringsDTD, XNODE_STR_DTD_FIXED, state))
						{
							readWhitespace(state, false);
						}
						readAttValue(state, false, &refs);
						nextChar(state, false);
					}
				}
			}
			if (*state->c != XNODE_CHAR_RARROW)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (readSpecialString(specStringsDTD, XNODE_STR_DTD_ENTITY, state))
		{
			readWhitespace(state, false);
			if (*state->c == XNODE_CHAR_PCT)
			{
				/*
				 * http://www.w3.org/TR/2008/REC-xml-20081126/# NT-PEDecl
				 */
				nextChar(state, false);
				readWhitespace(state, false);
				readName(state, true);
				if (*state->c == XNODE_CHAR_QUOTMARK || *state->c == XNODE_CHAR_APOSTR)
				{
					readDTDEntityValue(state);
				}
				else
				{
					unsigned int pos = state->srcPos;

					readDTDExternalID(state);
					if (pos == state->srcPos)
					{
						elog(ERROR, "failed to read entity value or external id at row %u, column %u.",
							 state->srcRow, state->srcCol);
					}
				}
			}
			else
			{
				readName(state, true);

				/*
				 * http://www.w3.org/TR/2008/REC-xml-20081126/# NT-EntityDef
				 */
				if (*state->c == XNODE_CHAR_QUOTMARK || *state->c == XNODE_CHAR_APOSTR)
				{
					readDTDEntityValue(state);
				}
				else
				{
					unsigned int pos = state->srcPos;

					readDTDExternalID(state);
					if (pos == state->srcPos)
					{
						elog(ERROR, "failed to read entity value or external id at row %u, column %u.",
							 state->srcRow, state->srcCol);
					}
					if (XNODE_WHITESPACE(state->c))
					{
						nextChar(state, false);
						if (readSpecialString(specStringsDTD, XNODE_STR_DTD_NDATA, state))
						{
							readWhitespace(state, false);
							readName(state, false);
						}
					}
				}
			}
			readWhitespace(state, true);
			if (*state->c != XNODE_CHAR_RARROW)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (readSpecialString(specStringsDTD, XNODE_STR_DTD_NOTATION, state))
		{
			readWhitespace(state, false);
			nextChar(state, false);
			readName(state, true);
			if (readSpecialString(specStringsDTD, XNODE_STR_DTD_PUBLIC, state))
			{
				readWhitespace(state, false);
				processLiteral(state, true);
				nextChar(state, false);
				if (!XNODE_WHITESPACE(state->c))
				{
					nextChar(state, false);
					if (*state->c == XNODE_CHAR_AMPERSAND || *state->c == XNODE_CHAR_QUOTMARK)
					{
						processLiteral(state, false);
						nextChar(state, false);
					}
				}
			}
			else if (readSpecialString(specStringsDTD, XNODE_STR_DTD_SYSTEM, state))
			{
				readWhitespace(state, false);
				processLiteral(state, false);
				nextChar(state, false);
			}
			else
			{
				UNEXPECTED_CHARACTER;
			}
			readWhitespace(state, true);
			if (*state->c != XNODE_CHAR_RARROW)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else
		{
			elog(ERROR, "unrecognized string (keyword) at row %u, column %u", state->srcRow, state->srcCol);
		}
	}
	else
	{
		UNEXPECTED_CHARACTER;
	}
}


/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-PubidLiteral or
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-SystemLiteral depending on
 * whether 'public' parameter is true or false
 */

static void
processLiteral(XMLParserState state, bool public)
{
	char		qMark;

	if (*state->c != XNODE_CHAR_APOSTR && *state->c != XNODE_CHAR_QUOTMARK)
	{
		UNEXPECTED_CHARACTER;
	}
	qMark = *state->c;
	do
	{
		nextChar(state, false);
		if (!XNODE_VALID_CHAR(state->c))
		{
			elog(ERROR, "invalid XML character at row %u, column %u",
				 state->srcRow, state->srcCol);
		}
		if ((*state->c != qMark) && public)
		{
			if (!isPubIdChar(*state->c))
			{
				UNEXPECTED_CHARACTER;
			}
		}
	} while (*state->c != qMark);
}

/*
 * 'offset' - how many characters of the (possible) special string have been
 * processed before this function was called.
 */
static bool
readSpecialStringPart(char specStrings[][XNODE_SPEC_STR_MAX_LEN],
				 XNodeSpecString strIndex, XMLParserState state, char offset)
{
	char	   *specString = specStrings[strIndex] + offset;
	unsigned int len = strlen(specString);

	if (state->srcPos + len <= state->sizeIn &&
		strncmp(state->c, specString, len) == 0)
	{

		unsigned int i;

		for (i = 0; i < len; i++)
		{
			nextChar(state, false);
		}
		return true;
	}
	else
	{
		return false;
	}
}

static bool
readSpecialString(char specStrings[][XNODE_SPEC_STR_MAX_LEN], XNodeSpecString strIndex,
				  XMLParserState state)
{
	return readSpecialStringPart(specStrings, strIndex, state, 0);
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

/*
 * The state variables are incremented by values retrieved during the
 * previous call. The reason is that sometimes we need to have the current
 * character width at hand (in order to check the next character).
 */
static void
nextChar(XMLParserState state, bool endAllowed)
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

static void
ensureSpace(unsigned int size, XMLParserState state)
{
	unsigned int chunks = 0;
	unsigned int orig = state->sizeOut;

	while (state->dstPos + size > state->sizeOut)
	{
		state->sizeOut += state->sizeOut >> XNODE_OUT_OVERHEAD_BITS;
		chunks++;
	}
	if (chunks > 0)
	{
		state->result = (char *) repalloc(state->result, state->sizeOut
										  + VARHDRSZ);
		state->tree = state->result + VARHDRSZ;
		elog(DEBUG1, "Output array expanded from %u to %u bytes.", orig, state->sizeOut);
	}
}

static void
saveNodeHeader(XMLParserState state, XMLNodeInternal nodeInfo, char flags)
{
	unsigned int hdrSize;
	XMLNodeHeader node;

	if (nodeInfo->tokenType & (TOKEN_ETAG | TOKEN_EMPTY_ELEMENT | TOKEN_XMLDECL))
	{
		hdrSize = sizeof(XMLElementHeaderData);
	}
	else
	{
		hdrSize = sizeof(XMLNodeHeaderData);
	}
	ensureSpace(hdrSize, state);
	node = (XMLNodeHeader) (state->tree + nodeInfo->nodeOut);
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
	state->dstPos += hdrSize;
	nodeInfo->headerSaved = true;
}

static void
saveContent(XMLParserState state, XMLNodeInternal nodeInfo)
{
	if (nodeInfo->tokenType & (TOKEN_ETAG | TOKEN_EMPTY_ELEMENT |
							   TOKEN_CDATA | TOKEN_COMMENT | TOKEN_DTD | TOKEN_PI | TOKEN_TEXT | TOKEN_REFERENCE))
	{
		ensureSpace(nodeInfo->cntLength + 1, state);
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
saveReferences(XMLParserState state, XMLNodeInternal nodeInfo,
			   XMLElementHeader element, unsigned short int children)
{
	/*
	 * Find out the range of reference values and the corresponding storage.
	 */
	XMLNodeOffset dist = nodeInfo->nodeOut -
	state->stack.items[state->stack.position - children];
	char		bwidth = getXMLNodeOffsetByteWidth(dist);
	unsigned int refsTotal = children * bwidth;
	char	   *childOffTarg;
	unsigned short int i;
	XMLNodeOffset elementOff = (char *) element - state->tree;

	ensureSpace(refsTotal, state);
	element = (XMLElementHeader) (state->tree + elementOff);
	XNODE_SET_REF_BWIDTH(element, bwidth);
	childOffTarg = XNODE_LAST_REF(element);
	for (i = 0; i < children; i++)
	{
		XMLNodeOffset childOffset = xmlnodePop(&state->stack);

		/*
		 * Distance between parent and child
		 */
		dist = nodeInfo->nodeOut - childOffset;

		writeXMLNodeOffset(dist, &childOffTarg, bwidth, false);
		childOffTarg = XNODE_PREV_REF(childOffTarg, element);
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
	XMLElementHeader rootNode;
	XMLNodeOffset *rootOffSrc;
	char	   *rootOffTarg;
	XMLNodeOffset rootNodeOff = state->dstPos;
	XMLNodeOffset *rootNodeOffPtr;
	unsigned short int childCount;
	unsigned int refsTotal;
	char		bwidth;

	rootHdrSz = sizeof(XMLElementHeaderData);

	/*
	 * If the initial call to processToken() did not fail, at least one root
	 * node must have been processed.
	 */
	Assert(state->stack.position >= 1);
	childCount = state->stack.position;
	rootOffSrc = state->stack.items;
	bwidth = getXMLNodeOffsetByteWidth(rootNodeOff - *rootOffSrc);
	refsTotal = childCount * bwidth;
	ensureSpace(rootHdrSz + refsTotal, state);
	state->dstPos += rootHdrSz;
	rootNode = (XMLElementHeader) (state->tree + rootNodeOff);
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
		XMLNodeOffset dist = rootNodeOff - *rootOffSrc;

		writeXMLNodeOffset(dist, &rootOffTarg, bwidth, true);
		rootOffSrc++;
	}
	state->dstPos += refsTotal;

	if (kind == XMLNODE_DOC && state->decl != NULL)
	{
		unsigned int declSize = sizeof(XMLDeclData);

		ensureSpace(declSize, state);

		/*
		 * re-initialize the 'rootNode', in case re-allocation took place
		 */
		rootNode = (XMLElementHeader) (state->tree + rootNodeOff);

		memcpy(state->tree + state->dstPos, state->decl, declSize);
		state->dstPos += declSize;
		rootNode->common.flags |= XNODE_DOC_XMLDECL;
	}
	ensureSpace(sizeof(XMLNodeOffset), state);
	rootNode = (XMLElementHeader) (state->tree + rootNodeOff);

	rootNodeOffPtr = (XMLNodeOffset *) (state->tree + state->dstPos);
	*rootNodeOffPtr = ((char *) rootNode - state->tree);
	state->dstPos += sizeof(XMLNodeOffset);
	SET_VARSIZE(state->result, state->dstPos + VARHDRSZ);
}

/*
 * TODO Estimate length of the output so that the exact length doesn't have
 * to be computed.
 */
void
xmlnodeDumpNode(char *input, XMLNodeOffset nodeOff, char **output, unsigned int *pos)
{
	char	   *content;
	unsigned int cntLen;
	unsigned int incr;
	XMLNodeHeader node = (XMLNodeHeader) (input + nodeOff);

	switch (node->kind)
	{
			unsigned short int i;
			char	   *childOffPtr,
					   *lastChild;

		case XMLNODE_ELEMENT:
		case XMLNODE_DOC:
			if (node->kind == XMLNODE_ELEMENT)
			{
				XMLElementHeader element = (XMLElementHeader) node;

				content = XNODE_ELEMENT_NAME(element);
				cntLen = strlen(content);

				/*
				 * STag
				 */
				if (*output != NULL)
				{
					**output = XNODE_CHAR_LARROW;
					(*output)++;

					/*
					 * Tag name
					 */
					memcpy(*output, content, cntLen);
					*output += cntLen;
				}
				*pos += cntLen + 1;
				/* '<' + Name */
			}
			childOffPtr = XNODE_FIRST_REF(((XMLElementHeader) node));
			if (node->kind == XMLNODE_ELEMENT)
			{
				XMLElementHeader eh = (XMLElementHeader) node;

				i = dumpAttributes(eh, input, output, pos);
				childOffPtr = childOffPtr + i * XNODE_GET_REF_BWIDTH(eh);

				if (node->flags & XNODE_ELEMENT_EMPTY)
				{
					/*
					 * EmptyElement
					 */
					if (*output != NULL)
					{
						**output = XNODE_CHAR_SLASH;
						(*output)++;
						**output = XNODE_CHAR_RARROW;
						(*output)++;
					}
					*pos += 2;
				}
				else
				{
					char	   *lastChild = XNODE_LAST_REF((XMLElementHeader) node);

					if (*output != NULL)
					{
						**output = XNODE_CHAR_RARROW;
						(*output)++;
					}
					(*pos)++;

					while (childOffPtr <= lastChild)
					{
						xmlnodeDumpNode(input, nodeOff - readXMLNodeOffset(&childOffPtr,
							   XNODE_GET_REF_BWIDTH(eh), true), output, pos);
					}

					/*
					 * Etag
					 */
					if (*output != NULL)
					{
						**output = XNODE_CHAR_LARROW;
						(*output)++;
						**output = XNODE_CHAR_SLASH;
						(*output)++;
						memcpy(*output, content, cntLen);
						(*output) += cntLen;
						**output = XNODE_CHAR_RARROW;
						(*output)++;
					}
					*pos += 3 + strlen(content);
					/* '</' + 'Name' + '>' */
				}
			}
			else
			{
				/*
				 * This is a document node.
				 */
				char	   *lastChild = XNODE_LAST_REF((XMLElementHeader) node);

				while (childOffPtr <= lastChild)
				{
					xmlnodeDumpNode(input, nodeOff -
									readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH((XMLElementHeader) node), true),
									output, pos);
				}
			}
			break;

		case XMLNODE_DTD:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);
			if (*output != NULL)
			{
				unsigned int incr;

				memcpy(*output, specStrings[XNODE_STR_DTD_START], incr
					   = strlen(specStrings[XNODE_STR_DTD_START]));
				(*output) += incr;
				memcpy(*output, content, cntLen);
				(*output) += cntLen;
				memcpy(*output, specStrings[XNODE_STR_DTD_END], incr
					   = strlen(specStrings[XNODE_STR_DTD_END]));
				(*output) += incr;
			}
			cntLen += strlen(specStrings[XNODE_STR_DTD_START]) + strlen(
											 specStrings[XNODE_STR_DTD_END]);
			*pos += cntLen;
			break;

		case XMLNODE_ATTRIBUTE:
			content = XNODE_CONTENT(node);

			/*
			 * Skip name
			 */
			content += strlen(content) + 1;
			cntLen = strlen(content);
			if (*output != NULL)
			{
				memcpy(*output, content, cntLen);
				(*output) += cntLen;
			}
			*pos += cntLen;
			break;

		case XMLNODE_COMMENT:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);
			if (*output != NULL)
			{
				unsigned int incr;

				memcpy(*output, specStrings[XNODE_STR_CMT_START], incr
					   = strlen(specStrings[XNODE_STR_CMT_START]));
				(*output) += incr;
				memcpy(*output, content, cntLen);
				(*output) += cntLen;
				memcpy(*output, specStrings[XNODE_STR_CMT_END], incr
					   = strlen(specStrings[XNODE_STR_CMT_END]));
				(*output) += incr;
			}
			cntLen += strlen(specStrings[XNODE_STR_CMT_START]) + strlen(
											 specStrings[XNODE_STR_CMT_END]);
			*pos += cntLen;
			break;

		case XMLNODE_PI:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);
			incr = strlen(specStrings[XNODE_STR_PI_START]);

			if (*output != NULL)
			{
				memcpy(*output, specStrings[XNODE_STR_PI_START], incr);
				(*output) += incr;
				memcpy(*output, content, cntLen);
				(*output) += cntLen;
			}
			*pos += cntLen + incr;

			if (node->flags & XNODE_PI_HAS_VALUE)
			{
				content += cntLen + 1;
				cntLen = strlen(content);
				if (*output != NULL)
				{
					**output = ' ';
					(*output)++;
					memcpy(*output, content, cntLen);
					(*output) += cntLen;
				}
				*pos += cntLen + 1;
			}
			incr = strlen(specStrings[XNODE_STR_PI_END]);
			if (*output != NULL)
			{
				memcpy(*output, specStrings[XNODE_STR_PI_END], incr);
				(*output) += incr;
			}
			*pos += incr;
			break;

		case XMLNODE_CDATA:
		case XMLNODE_TEXT:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);

			if (node->flags & XNODE_TEXT_SPEC_CHARS)
			{
				dumpContentEscaped(node->kind, output, content, cntLen, pos);
			}
			else
			{
				if (*output != NULL)
				{
					memcpy(*output, content, cntLen);
					(*output) += cntLen;
				}
				*pos += cntLen;
			}
			break;

		case XMLNODE_DOC_FRAGMENT:
			childOffPtr = XNODE_FIRST_REF(((XMLElementHeader) node));
			lastChild = XNODE_LAST_REF((XMLElementHeader) node);
			while (childOffPtr <= lastChild)
			{
				XMLElementHeader eh = (XMLElementHeader) node;

				xmlnodeDumpNode(input, nodeOff - readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH(eh), true),
								output, pos);
			}
			break;

		default:
			elog(ERROR, "unable to dump node: %u", node->kind);
			break;
	}
}

static unsigned int
dumpAttributes(XMLElementHeader element, char *input,
			   char **output, unsigned int *pos)
{

	unsigned int i = 0;
	char	   *childOffPtr = XNODE_FIRST_REF(element);
	char	   *lastChild = XNODE_LAST_REF(element);

	while (childOffPtr <= lastChild)
	{
		XMLNodeHeader attrNode = (XMLNodeHeader) ((char *) element
												  - readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH(element), true));
		char	   *attrName,
				   *attrValue;
		unsigned int attrNameLen,
					attrValueLen;
		char		qMark = (attrNode->flags & XNODE_ATTR_APOSTROPHE) ? XNODE_CHAR_APOSTR : XNODE_CHAR_QUOTMARK;

		if (attrNode->kind != XMLNODE_ATTRIBUTE)
		{
			break;
		}
		attrName = (char *) attrNode + sizeof(XMLNodeHeaderData);
		attrNameLen = strlen(attrName);
		if (*output != NULL)
		{
			**output = XNODE_CHAR_SPACE;
			(*output)++;
			memcpy(*output, attrName, attrNameLen);
			*output += attrNameLen;
			**output = XNODE_CHAR_EQ;
			(*output)++;
			**output = qMark;
			(*output)++;
		}
		*pos += attrNameLen + 3;

		attrValue = attrName + attrNameLen + 1;
		attrValueLen = strlen(attrValue);

		if (attrNode->flags & XNODE_ATTR_CONTAINS_REF)
		{
			dumpContentEscaped(XMLNODE_ATTRIBUTE, output, attrValue, attrValueLen, pos);
		}
		else
		{
			if (*output != NULL)
			{
				memcpy(*output, attrValue, attrValueLen);
				*output += attrValueLen;
			}
			*pos += attrValueLen;
		}

		if (*output != NULL)
		{
			**output = qMark;
			(*output)++;
		}
		(*pos)++;
		i++;
	}
	return i;
}

static bool
isPubIdChar(char c)
{
	if ((c >= 0x61 && c <= 0x7a) || (c >= 0x41 && c <= 0x5a) || (c >= 0x30 && c <= 0x39))
	{
		return true;
	}
	else
	{
		unsigned short int i;

		for (i = 0; i < sizeof(pubIdChars); i++)
		{
			if (c == pubIdChars[i])
			{
				return true;
			}
		}
		return false;
	}
}

static void
dumpContentEscaped(XMLNodeKind kind, char **output, char *input, unsigned int inputLen,
				   unsigned int *outPos)
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
					dumpSpecString(output, escaped, outPos, &incrInput);
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
				dumpSpecString(output, outStr, outPos, &incrInput);
			}
		}
		else
		{
			elog(ERROR, "unexpected node kind %u", kind);
		}

		if (!special)
		{
			incrInput = pg_utf_mblen((unsigned char *) input);
			if (*output != NULL)
			{
				memcpy(*output, input, incrInput);
				*output += incrInput;
			}
			*outPos += incrInput;
		}
		i += incrInput;
		input += incrInput;
	}
}

static void
dumpSpecString(char **output, char *outNew, unsigned int *outPos, unsigned int *incrInput)
{
	unsigned short len = strlen(outNew);

	if (*output != NULL)
	{
		**output = XNODE_CHAR_AMPERSAND;
		(*output)++;
		memcpy(*output, outNew, len);
		*output += len;
	}
	*outPos += len + 1;
	*incrInput = 1;
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
 */

void
writeXMLNodeOffset(XMLNodeOffset ref, char **outPtr, unsigned char bytes, bool step)
{
	unsigned int mask = 0xFF;
	unsigned short int shift = 0;
	unsigned char i;
	char	   *out = *outPtr;

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

	outDecl.maxlen = 16;
	outDecl.data = (char *) palloc(outDecl.maxlen);
	resetStringInfo(&outDecl);

	qMark = XMLDECL_GET_QUOT_MARK(decl, i);
	appendStringInfoString(&outDecl, specStrings[XNODE_STR_XDECL_START]);
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
	appendStringInfoString(&outDecl, specStrings[XNODE_STR_XDECL_END]);
	appendStringInfoChar(&outDecl, '\0');
	return outDecl.data;
}
