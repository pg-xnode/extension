/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#include "xml_parser.h"

static void parseDTDExternalID(XMLParserState state);
static void parseMarkupDecl(XMLParserState state);
static void parseLiteral(XMLParserState state, bool public);
static void parseDTDCP(XMLParserState state);
static void parseDTDChoiceOrSeq(XMLParserState state, bool started);
static void parseDTDEntityValue(XMLParserState state);

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-PubidChar (Except for
 * intervals)
 */
static char pubIdChars[] = {0x20, 0xd, 0xa, 0x2d, 0x27, 0x28, 0x29, 0x2b, 0x2c, 0x2e, 0x2f,
0x3a, 0x3d, 0x3f, 0x3b, 0x21, 0x2a, 0x23, 0x40, 0x24, 0x5f, 0x25};
static bool isPubIdChar(char c);

/*
 * Order of the DTD keywords must follow the order of
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

void
xmlnodeParseDTD(XMLParserState state)
{
	if (!XNODE_WHITESPACE(state->c))
	{
		UNEXPECTED_CHARACTER;
	}
	else
	{
		nextXMLChar(state, false);
	}
	if (!XNODE_VALID_NAME_START(state->c))
	{
		UNEXPECTED_CHARACTER;
	}
	nextXMLChar(state, false);
	while (XNODE_VALID_NAME_CHAR(state->c))
	{
		nextXMLChar(state, false);
	}
	if (XNODE_WHITESPACE(state->c))
	{
		nextXMLChar(state, false);
	}
	parseDTDExternalID(state);

	if (XNODE_WHITESPACE(state->c))
	{
		nextXMLChar(state, false);
	}
	if (*state->c == XNODE_CHAR_LBRACKET)
	{
		/*
		 * http://www.w3.org/TR/xml/#NT-intSubset Including the right bracket
		 * and optional white space
		 */
		nextXMLChar(state, false);
		while (*state->c != XNODE_CHAR_RBRACKET)
		{
			if (*state->c == XNODE_CHAR_PCT)
			{
				/*
				 * http://www.w3.org/TR/2008/REC-xml-20081126/# NT-PEReference
				 */
				nextXMLChar(state, false);
				if (!XNODE_VALID_NAME_START(state->c))
				{
					UNEXPECTED_CHARACTER;
				}
				nextXMLChar(state, false);
				while (XNODE_VALID_NAME_CHAR(state->c))
				{
					nextXMLChar(state, false);
				}
				if (*state->c != XNODE_CHAR_SEMICOLON)
				{
					UNEXPECTED_CHARACTER;
				}
			}
			else if (*state->c == XNODE_CHAR_LARROW)
			{
				parseMarkupDecl(state);
			}
			else if (!XNODE_WHITESPACE(state->c))
			{
				UNEXPECTED_CHARACTER;
			}
			nextXMLChar(state, false);
		}
		nextXMLChar(state, false);
		if (XNODE_WHITESPACE(state->c))
		{
			nextXMLChar(state, false);
		}
	}
	if (*state->c != XNODE_CHAR_RARROW)
	{
		UNEXPECTED_CHARACTER;
	}
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-ExternalID
 */
static void
parseDTDExternalID(XMLParserState state)
{
	if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_SYSTEM, state))
	{
		if (!XNODE_WHITESPACE(state->c))
		{
			UNEXPECTED_CHARACTER;
		}
		nextXMLChar(state, false);
		parseLiteral(state, false);
		nextXMLChar(state, false);
	}
	else if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_PUBLIC, state))
	{
		if (!XNODE_WHITESPACE(state->c))
		{
			UNEXPECTED_CHARACTER;
		}
		nextXMLChar(state, false);
		parseLiteral(state, true);
		nextXMLChar(state, false);
		if (!XNODE_WHITESPACE(state->c))
		{
			UNEXPECTED_CHARACTER;
		}
		nextXMLChar(state, false);
		parseLiteral(state, false);
		nextXMLChar(state, false);
	}
}

/*
 * http://www.w3.org/TR/xml/#NT-markupdecl
 */
static void
parseMarkupDecl(XMLParserState state)
{
	nextXMLChar(state, false);
	if (*state->c == XNODE_CHAR_QUESTMARK)
	{
		nextXMLChar(state, false);
		readXMLPI(state);
	}
	else if (*state->c == XNODE_CHAR_EXCLMARK)
	{
		nextXMLChar(state, false);
		if (readSpecialXMLStringPart(specXMLStrings, XNODE_STR_CMT_START, state, 2))
		{
			readXMLComment(state);
		}
		else if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_ELEMENT, state))
		{
			readXMLWhitespace(state, false);
			readXMLName(state, false, false, false, NULL);
			nextXMLChar(state, false);
			if (!readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_EMPTY, state) &&
			 !readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_ANY, state))
			{

				if (*state->c == XNODE_CHAR_LBRKT_RND)
				{
					nextXMLChar(state, false);
					readXMLWhitespace(state, true);
					if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_PCDATA, state))
					{
						/*
						 * http://www.w3.org/TR/2008/RE
						 * C-xml-20081126/#NT-Mixed
						 */
						unsigned int names = 0;

						readXMLWhitespace(state, true);
						while (*state->c == XNODE_CHAR_PIPE)
						{
							readXMLWhitespace(state, true);
							nextXMLChar(state, false);
							readXMLName(state, false, false, false, NULL);
							names++;
							readXMLWhitespace(state, true);
						}
						if (*state->c != XNODE_CHAR_RBRKT_RND)
						{
							UNEXPECTED_CHARACTER;
						}
						nextXMLChar(state, false);
						if (*state->c == XNODE_CHAR_ASTERISK)
						{
							if (names == 0)
							{
								UNEXPECTED_CHARACTER;
							}
							else
							{
								nextXMLChar(state, false);
							}
						}
					}
					else
					{
						/*
						 * http://www.w3.org/TR/2008/RE
						 * C-xml-20081126/#NT-children
						 */
						parseDTDChoiceOrSeq(state, true);
						if (*state->c == XNODE_CHAR_QUESTMARK || *state->c == XNODE_CHAR_ASTERISK ||
							*state->c == XNODE_CHAR_PLUS)
						{
							nextXMLChar(state, false);
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
				nextXMLChar(state, false);
			}
			if (*state->c != XNODE_CHAR_RARROW)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_ATTLIST, state))
		{
			if (!XNODE_WHITESPACE(state->c))
			{
				UNEXPECTED_CHARACTER;
			}
			readXMLWhitespace(state, false);
			readXMLName(state, false, false, false, NULL);
			while (XNODE_WHITESPACE(state->c))
			{
				nextXMLChar(state, false);
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

					readXMLName(state, true, false, false, NULL);
					for (i = 0; i < XNODE_DTD_ATTR_TYPES; i++)
					{
						type = dtdAttTypes[i];
						len = strlen(type);
						if (strncmp(type, state->c, len) == 0)
						{
							for (j = 0; i < len; j++)
							{
								nextXMLChar(state, false);
							}
							found = true;
							break;
						}
					}
					if (!found)
					{
						if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_NOTATION, state))
						{
							/*
							 * http://www.w3.org/TR /2008/REC-xml-20081
							 * 126/#NT-NotationTyp e
							 */
							readXMLWhitespace(state, false);
							if (*state->c != XNODE_CHAR_LBRKT_RND)
							{
								UNEXPECTED_CHARACTER;
							}
							nextXMLChar(state, false);
							readXMLWhitespace(state, true);
							readXMLName(state, false, false, false, NULL);
							readXMLWhitespace(state, true);
							while (*state->c != XNODE_CHAR_RBRKT_RND)
							{
								if (*state->c != XNODE_CHAR_PIPE)
								{
									UNEXPECTED_CHARACTER;
								}
								nextXMLChar(state, false);
								readXMLWhitespace(state, true);
								readXMLName(state, false, false, false, NULL);
								readXMLWhitespace(state, true);
							}
							nextXMLChar(state, false);

						}
						else if (*state->c == XNODE_CHAR_LBRKT_RND)
						{
							/*
							 * http://www.w3.org/TR /2008/REC-xml-20081
							 * 126/#NT-Enumeration
							 */
							unsigned int cnt;

							nextXMLChar(state, false);
							do
							{
								readXMLWhitespace(state, true);
								cnt = 0;
								while (XNODE_VALID_NAME_CHAR(state->c))
								{
									nextXMLChar(state, false);
									cnt++;
								}
								if (cnt == 0)
								{
									elog(ERROR, "name token expected at row %u, column %u", state->srcRow, state->srcCol);
								}
								readXMLWhitespace(state, true);
							} while (*state->c != XNODE_CHAR_PIPE);
							if (*state->c != XNODE_CHAR_RBRKT_RND)
							{
								UNEXPECTED_CHARACTER;
							}
							nextXMLChar(state, false);
						}
						else
						{
							UNEXPECTED_CHARACTER;
						}
					}
					readXMLWhitespace(state, false);

					/*
					 * http://www.w3.org/TR/2008/REC-xml-20
					 * 081126/#NT-DefaultDecl
					 */
					if (!readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_REQUIRED, state) &&
						!readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_IMPLIED, state))
					{
						bool		refs;

						if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_FIXED, state))
						{
							readXMLWhitespace(state, false);
						}
						readXMLAttValue(state, false, &refs);
						nextXMLChar(state, false);
					}
				}
			}
			if (*state->c != XNODE_CHAR_RARROW)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_ENTITY, state))
		{
			readXMLWhitespace(state, false);
			if (*state->c == XNODE_CHAR_PCT)
			{
				/*
				 * http://www.w3.org/TR/2008/REC-xml-20081126/# NT-PEDecl
				 */
				nextXMLChar(state, false);
				readXMLWhitespace(state, false);
				readXMLName(state, true, false, false, NULL);
				if (*state->c == XNODE_CHAR_QUOTMARK || *state->c == XNODE_CHAR_APOSTR)
				{
					parseDTDEntityValue(state);
				}
				else
				{
					unsigned int pos = state->srcPos;

					parseDTDExternalID(state);
					if (pos == state->srcPos)
					{
						elog(ERROR, "failed to read entity value or external id at row %u, column %u.",
							 state->srcRow, state->srcCol);
					}
				}
			}
			else
			{
				readXMLName(state, true, false, false, NULL);

				/*
				 * http://www.w3.org/TR/2008/REC-xml-20081126/# NT-EntityDef
				 */
				if (*state->c == XNODE_CHAR_QUOTMARK || *state->c == XNODE_CHAR_APOSTR)
				{
					parseDTDEntityValue(state);
				}
				else
				{
					unsigned int pos = state->srcPos;

					parseDTDExternalID(state);
					if (pos == state->srcPos)
					{
						elog(ERROR, "failed to read entity value or external id at row %u, column %u.",
							 state->srcRow, state->srcCol);
					}
					if (XNODE_WHITESPACE(state->c))
					{
						nextXMLChar(state, false);
						if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_NDATA, state))
						{
							readXMLWhitespace(state, false);
							readXMLName(state, false, false, false, NULL);
						}
					}
				}
			}
			readXMLWhitespace(state, true);
			if (*state->c != XNODE_CHAR_RARROW)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_NOTATION, state))
		{
			readXMLWhitespace(state, false);
			nextXMLChar(state, false);
			readXMLName(state, true, false, false, NULL);
			if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_PUBLIC, state))
			{
				readXMLWhitespace(state, false);
				parseLiteral(state, true);
				nextXMLChar(state, false);
				if (!XNODE_WHITESPACE(state->c))
				{
					nextXMLChar(state, false);
					if (*state->c == XNODE_CHAR_AMPERSAND || *state->c == XNODE_CHAR_QUOTMARK)
					{
						parseLiteral(state, false);
						nextXMLChar(state, false);
					}
				}
			}
			else if (readSpecialXMLString(specStringsDTD, XNODE_STR_DTD_SYSTEM, state))
			{
				readXMLWhitespace(state, false);
				parseLiteral(state, false);
				nextXMLChar(state, false);
			}
			else
			{
				UNEXPECTED_CHARACTER;
			}
			readXMLWhitespace(state, true);
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
parseLiteral(XMLParserState state, bool public)
{
	char		qMark;

	if (*state->c != XNODE_CHAR_APOSTR && *state->c != XNODE_CHAR_QUOTMARK)
	{
		UNEXPECTED_CHARACTER;
	}
	qMark = *state->c;
	do
	{
		nextXMLChar(state, false);
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
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-cp
 */
static void
parseDTDCP(XMLParserState state)
{
	if (XNODE_VALID_NAME_START(state->c))
	{
		readXMLName(state, false, false, false, NULL);
	}
	else
	{
		parseDTDChoiceOrSeq(state, false);
	}
	if (*state->c == XNODE_CHAR_QUESTMARK || *state->c == XNODE_CHAR_ASTERISK ||
		*state->c == XNODE_CHAR_PLUS)
	{
		nextXMLChar(state, false);
	}
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-choice or
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-seq
 */
static void
parseDTDChoiceOrSeq(XMLParserState state, bool started)
{
	unsigned int i = 0;
	char		delim = 0x0;

	if (!started)
	{
		if (*state->c != XNODE_CHAR_LBRKT_RND)
		{
			UNEXPECTED_CHARACTER;
		}
		nextXMLChar(state, false);
		readXMLWhitespace(state, true);
	}
	parseDTDCP(state);
	readXMLWhitespace(state, true);
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
		nextXMLChar(state, false);
		readXMLWhitespace(state, true);
		parseDTDCP(state);
		readXMLWhitespace(state, true);
		i++;
	}
	nextXMLChar(state, false);
}

/*
 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-EntityValue
 */
static void
parseDTDEntityValue(XMLParserState state)
{
	char		qMark = *state->c;

	nextXMLChar(state, false);
	while (*state->c != qMark)
	{
		if (*state->c == XNODE_CHAR_PCT)
		{
			/*
			 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-PERef erence
			 */
			nextXMLChar(state, false);
			readXMLName(state, false, false, false, NULL);
			if (*state->c != XNODE_CHAR_SEMICOLON)
			{
				UNEXPECTED_CHARACTER;
			}
		}
		else if (*state->c == XNODE_CHAR_AMPERSAND)
		{
			pg_wchar	value;

			readXMLReference(state, &value);
		}
		nextXMLChar(state, false);
	}
	nextXMLChar(state, false);
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
