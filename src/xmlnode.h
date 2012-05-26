/*
 * Copyright (C) 2012, Antonin Houska
 */

#ifndef XMLNODE_H
#define XMLNODE_H

#include <ctype.h>
#include "postgres.h"
#include "fmgr.h"
#include "utils/elog.h"

#define XNODE_CHAR_SPACE		0x20
#define XNODE_CHAR_EXCLMARK		0x21
#define XNODE_CHAR_QUOTMARK		0x22
#define XNODE_CHAR_HASH			0x23
#define XNODE_CHAR_PCT			0x25
#define XNODE_CHAR_AMPERSAND	0x26
#define XNODE_CHAR_APOSTR		0x27
#define XNODE_CHAR_LBRKT_RND	0x28
#define XNODE_CHAR_RBRKT_RND	0x29
#define XNODE_CHAR_ASTERISK		0x2a
#define XNODE_CHAR_PLUS			0x2b
#define XNODE_CHAR_COMMA		0x2c
#define XNODE_CHAR_DASH			0x2d
#define XNODE_CHAR_DOT			0x2e
#define XNODE_CHAR_SLASH		0x2f
#define XNODE_CHAR_COLON		0x3a
#define XNODE_CHAR_SEMICOLON	0x3b
#define XNODE_CHAR_LARROW		0x3c
#define XNODE_CHAR_EQ			0x3d
#define XNODE_CHAR_RARROW		0x3e
#define XNODE_CHAR_QUESTMARK	0x3f
#define XNODE_CHAR_AT			0x40
#define XNODE_CHAR_LBRACKET		0x5b
#define XNODE_CHAR_RBRACKET		0x5d
#define XNODE_CHAR_UNDERSCORE	0x5f
#define XNODE_CHAR_PIPE			0x7c

#define XNODE_NAMESPACE_DEF_PREFIX		"xmlns"

typedef enum XMLNodeKind
{
	XMLNODE_DOC = 0,
	XMLNODE_DTD,
	XMLNODE_ELEMENT,
	XMLNODE_ATTRIBUTE,
	XMLNODE_COMMENT,
	XMLNODE_CDATA,
	XMLNODE_PI,
	XMLNODE_TEXT,
	XMLNODE_DOC_FRAGMENT,
	XMLNODE_NODE
} XMLNodeKind;

typedef uint32 XMLNodeOffset;

typedef struct XMLNodeCommonData
{
	/*
	 * XMLNodeKind would be expected here, but char is used to save space.
	 */
	uint8		kind;
	uint8		flags;
} XMLNodeCommonData;

/*
 * For simple (non-compound) node types, content follows the structure immediately
 * (there are no references to children).
 */
typedef XMLNodeCommonData XMLNodeHdrData;

typedef XMLNodeHdrData *XMLNodeHdr;

/*
 * A compound node, i.e node containing other node(s).
 * Used for the following node types:
 * XMLNODE_ELEMENT, XMLNODE_DOC, XMLNODE_DOC_FRAGMENT
 *
 * The header is followed by array of references.
 * Reference means relative offset of a nested element, from the current element backwards (child
 * is always stored at lower addresses than parent).
 * Such a reference takes 1 to 4 bytes, while this 'byte width' must be equal for all references
 * that given compound node keeps. Thus the byte width is derived from the maximum offset the compound
 * node has (given the 'child first' ordering, the maximum offset is that of the first child).
 *
 * XNODE_GET_REF_BWIDTH() / XNODE_SET_REF_BWIDTH() should be used to set the byte width. Direct access
 * to the storage is discouraged.
 *
 * The array of references is followed either by element name (XMLNODE_ELEMENT) or by document header
 * (XMLNODE_DOC).
 */
typedef struct XMLCompNodeHdrData
{
	XMLNodeCommonData common;

	uint16		children;
} XMLCompNodeHdrData;

typedef XMLCompNodeHdrData *XMLCompNodeHdr;

typedef struct XMLDeclData
{
	uint8		flags;
	uint8		version;
	uint8		enc;
	bool		standalone;
}	XMLDeclData;

typedef struct XMLDeclData *XMLDecl;

#define XMLDECL_HAS_ENC		(1 << 0)
#define XMLDECL_HAS_SD_DECL (1 << 1)
/*
 * Bits 2, 3 and 4 are reserved for quotation mark type
 */
#define XMLDECL_GET_QUOT_MARK(decl, i) (decl->flags & (1 << (i + 2))) ? XNODE_CHAR_APOSTR :\
		XNODE_CHAR_QUOTMARK;

#define XMLNODE_MAX_CHILDREN	0xFFFF

#define XNODE_WHITESPACE(c) (*(c) == 0x9 || *(c) == 0xA || *(c) == 0xD || *(c) == 0x20)
#define XNODE_VALID_NAME_START(c)	(*(c) == XNODE_CHAR_COLON || *(c) == XNODE_CHAR_UNDERSCORE || \
		isXMLCharInInterval(c, nameStartCharIntervals, XNODE_NAME_START_CHAR_INTERVALS))
#define XNODE_VALID_NAME_CHAR(c)	(XNODE_VALID_NAME_START(c) || *(c) == XNODE_CHAR_DASH ||\
		*(c) == XNODE_CHAR_DOT ||	isXMLCharInInterval(c, nameCharIntervals, XNODE_NAME_CHAR_INTERVALS))
#define XNODE_VALID_CHAR(c) (XNODE_WHITESPACE(c) || isXMLCharInInterval(c, charIntervals, CHAR_INTERVALS))

#define XNODE_NAME_START_CHAR_INTERVALS 15
#define XNODE_NAME_CHAR_INTERVALS		4

#define XNODE_OFFSET(node, doc) ((XMLNodeOffset) ((char *) (node) - (char *) VARDATA(doc)))

extern char getXMLNodeOffsetByteWidth(XMLNodeOffset o);

#define XNODE_FIRST_REF(cnd) ((char *) (cnd) + sizeof(XMLCompNodeHdrData))
#define XNODE_LAST_REF(cnd) (XNODE_FIRST_REF(cnd) + ((cnd)->children - 1) * XNODE_GET_REF_BWIDTH(cnd))

/*
 * Only use this for simple nodes
 */
#define XNODE_CONTENT(nd) ((char *) ((XMLNodeHdr) (nd) + 1))

/*
 * TODO Check if the multiplication needs to be performed in alternative
 * (more efficient) way. The same for XNODE_LAST_REF() above
 */
#define XNODE_ELEMENT_NAME(el) (XNODE_FIRST_REF(el) + (el)->children * XNODE_GET_REF_BWIDTH(el))

#define XNODE_NEXT_REF(ptr, cnd) (ptr + XNODE_GET_REF_BWIDTH(cnd))
#define XNODE_PREV_REF(ptr, cnd) (ptr - XNODE_GET_REF_BWIDTH(cnd))
#define XNODE_HAS_CHILDREN(cnd) (cnd->children > 0)

typedef struct varlena xmlnodetype;
typedef xmlnodetype *xmlnode;

extern Datum xmlnode_in(PG_FUNCTION_ARGS);
extern Datum xmlnode_out(PG_FUNCTION_ARGS);
extern Datum xmlnode_kind(PG_FUNCTION_ARGS);
extern Datum xmlnode_debug_print(PG_FUNCTION_ARGS);

extern char *dumpXMLNode(char *data, XMLNodeOffset rootNdOff);

typedef struct varlena xmldoctype;
typedef xmldoctype *xmldoc;

extern Datum xmldoc_in(PG_FUNCTION_ARGS);
extern Datum xmldoc_out(PG_FUNCTION_ARGS);

extern Datum xmlnode_to_xmldoc(PG_FUNCTION_ARGS);
extern Datum xmldoc_to_xmlnode(PG_FUNCTION_ARGS);


/* Get a pointer to root node offset from varlena (xmlnode, xmldoc) value. */
#define XNODE_ROOT_OFFSET_PTR(raw)	((XMLNodeOffset *) ((char *) raw + VARSIZE(raw) - sizeof(XMLNodeOffset)))
/* ... and the root offset itself */
#define XNODE_ROOT_OFFSET(raw) (*XNODE_ROOT_OFFSET_PTR(raw))

/*
 * Get root node from varlena (xmlnode, xmldoc) value
 */
#define XNODE_ROOT(raw) ((XMLNodeHdr) ((char *) VARDATA(raw) + XNODE_ROOT_OFFSET(raw)))

extern XMLNodeOffset readXMLNodeOffset(char **input, unsigned char bytes, bool step);
extern void writeXMLNodeOffset(XMLNodeOffset ref, char **output, unsigned char bytes, bool step);

#define XNODE_CONTAINER_CHUNK	16

typedef enum XNodeListItemKind
{
	XNODE_LIST_ITEM_SINGLE_OFF,
	XNODE_LIST_ITEM_SINGLE_PTR,
	XNODE_LIST_ITEM_RANGE
} XNodeListItemKind;

typedef struct XNodeOffsetRange
{
	XMLNodeOffset lower;
	XMLNodeOffset upper;
} XNodeOffsetRange;

typedef struct XNodeListItem
{
	bool		valid;			/* Only applicable when the containing list is
								 * used as 'ignore list'. */
	XNodeListItemKind kind;

	union
	{
		XMLNodeOffset singleOff;
		void	   *singlePtr;
		XNodeOffsetRange range;
	}			value;
} XNodeListItem;

/*
 * A container to be used as a stack in most cases, but sometimes we used it
 * as FIFO.
 */

typedef struct XMLNodeContainerData
{
	unsigned int size;
	unsigned int position;
	XNodeListItem *content;
} XMLNodeContainerData;

typedef struct XMLNodeContainerData *XMLNodeContainer;


#define UTF_MAX_WIDTH		4

typedef struct UTF8Interval
{
	char		first[UTF_MAX_WIDTH];
	char		last[UTF_MAX_WIDTH];
} UTF8Interval;


extern UTF8Interval nameStartCharIntervals[XNODE_NAME_START_CHAR_INTERVALS];
extern UTF8Interval nameCharIntervals[XNODE_NAME_CHAR_INTERVALS];
extern bool isXMLCharInInterval(char *c, UTF8Interval *intervals, unsigned short int intCount);


/*
 * Flags that are supposed to be generic (i.e. used for 2 or more nodes)
 * must be at the highest positions.
 *
 * Kind-specific flags are expected at the lowest positions.
 */

/*
 * For text node set if it contains predefined reference(s)? (&amp; &lt;
 * ...). For CDATA node set if it contains	'<', '>' or '&'
 */
#define XNODE_TEXT_SPEC_CHARS		(1 << 7)
/* Namespace declaration or use. */
#define XNODE_NMSP_PREFIX			(1 << 6)

/* Bits 0 and 1 indicate maximum byte width of the distance between parent and child */
#define XNODE_REF_BWIDTH				0x03
#define XNODE_EMPTY						(1 << 2)
#define XNODE_DOC_XMLDECL				(1 << 3)

#define XNODE_ATTR_APOSTROPHE			(1 << 0)

/* Set if apostrophe is used as value delimiter. */
#define XNODE_ATTR_CONTAINS_REF			(1 << 1)
/*
 * This flag is set when value is stored as a text, but in fact represents a
 * number
 */
#define XNODE_ATTR_NUMBER				(1 << 2)

#define XNODE_PI_HAS_VALUE			(1 << 0)

#define XNODE_ATTR_IS_NUMBER(node)			(((node)->flags & XNODE_ATTR_NUMBER) != 0)

/*
 * Macros to read / write byte width of the maximum child's offset.
 * The value we store is 0-based, i.e. 0 in the 'flags' filed means 1 byte per reference.
 */
#define XNODE_GET_REF_BWIDTH(cnd)		(((cnd)->common.flags & XNODE_REF_BWIDTH) + 1)
/*
 * It's assumed that the bits we set have been reset.
 * If the existing value is to be changed, the caller is responsible for resetting the original value.
 */
#define XNODE_SET_REF_BWIDTH(cnd, bw)	((cnd)->common.flags |= (bw - 1))

/* Reset the byte-width if already set */
#define XNODE_RESET_REF_BWIDTH(cnd)		((cnd)->common.flags &= ~XNODE_REF_BWIDTH)

typedef enum XMLAddMode
{
	XMLADD_INVALID = 0,
	XMLADD_AFTER = 'a',
	XMLADD_BEFORE = 'b',
	XMLADD_INTO = 'i',
	XMLADD_REPLACE = 'r'
} XMLAddMode;

#define XMLADD_MODE(m) (tolower(m))

typedef enum XMLNodeAction
{
	XMLNODE_ACTION_ADD,
	XMLNODE_ACTION_REMOVE
} XMLNodeAction;

/*
 * DOM functions
 */
extern Datum xmlnode_children(PG_FUNCTION_ARGS);
extern Datum xmlelement(PG_FUNCTION_ARGS);


#endif   /* XMLNODE_H */
