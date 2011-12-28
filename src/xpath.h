#ifndef XPATH_H_
#define XPATH_H_

#include "xmlnode.h"

typedef struct varlena xpathtype;
typedef xpathtype *xpath;

#define XPATH_MAX_DEPTH					16

extern Datum xpath_in(PG_FUNCTION_ARGS);
extern Datum xpath_out(PG_FUNCTION_ARGS);
extern Datum xpath_debug_print(PG_FUNCTION_ARGS);

extern Datum xpath_single(PG_FUNCTION_ARGS);

#define XMLNODE_SET_MAX_COLS	16

extern Datum xpath_array(PG_FUNCTION_ARGS);

/*
 * The first is minimum amount of memory allocated for binary XPath and also
 * increment size. The second is maximum number of chunks.
 *
 * The maximum size of a single XPath (or a sub-path) in the binary form is
 * therefore (XPATH_PARSER_OUTPUT_CHUNK * XPATH_PARSER_OUTPUT_CHUNKS)
 *
 * This value *must not* exceed range of XPathOffset type.
 */
#define XPATH_PARSER_OUTPUT_CHUNK			1024
#define XPATH_PARSER_OUTPUT_CHUNKS			64

#define XPATH_EXPR_BUFFER_SIZE				512
#define XPATH_ELEMENT_MAX_LEN				0xFF
#define XPATH_EXPR_VAR_MAX					16

#define XPATH_LAST_LEVEL(xsc) ((xsc->currentDepth + 1) == (xsc->xpath->depth - xsc->xpathRoot))

/*
 * The primary purpose of 'xpath' type is to avoid repeated parsing of the
 * XPath expression when the same XPath is being used for many rows. A side
 * effect is that the XPath expressions can be stored in a table as a regular
 * type (and possibly used as something like a 'named query').
 */
typedef unsigned short XPathOffset;

typedef struct XPathData
{
	bool		relative;
	unsigned short depth;		/* 'Number of path elements (steps) */

	/*
	 * How many elements (steps) require search for descendants. It's useful
	 * to know that location path has 2 or more such elements. In such a case
	 * some nodes might be reached by multiple scan - subscan combinations and
	 * therefore uniqueness of nodes returned has to be checked.
	 */
	unsigned short descendants;
	unsigned short size;		/* Size of the whole XPath, including this
								 * structure */
	XMLNodeKind targNdKind;
	bool		allAttributes;
	bool		piTestValue;
	XPathOffset elements[1];
}	XPathData;

typedef struct XPathData *XPath;

#define XPATH_MAX_SUBPATHS	16

typedef struct XPathHeaderData
{
	unsigned short pathCount;
	XPathOffset paths[1];
}	XPathHeaderData;

#define XPATH_SET_MAX_PATHS		0xFF

typedef struct XPathHeaderData *XPathHeader;

#define XPATH_HDR_GET_PATH(header, i) ((XPath) (((char *)(header)) + (header)->paths[i]))

typedef enum XPathExpricateOperandType
{
	/* The following are all stored as XPathExprOperand */
	XPATH_OPERAND_LITERAL = 0,
	XPATH_OPERAND_ATTRIBUTE,
	XPATH_OPERAND_PATH,

	/* The following are both XPathExpression */
	XPATH_OPERAND_EXPR_TOP,
	XPATH_OPERAND_EXPR_SUB,

	/*
	 * While XPATH_OPERAND_FUNC is stored as XPathExprOperand,
	 * XPATH_OPERAND_FUNC_NOARG is a special kind of XPathExpression
	 */
	XPATH_OPERAND_FUNC,
	XPATH_OPERAND_FUNC_NOARG
}	XPathExprOperandType;

typedef enum XPathValueType
{
	XPATH_VAL_BOOLEAN = 0,
	XPATH_VAL_NUMBER,
	XPATH_VAL_STRING,
	XPATH_VAL_NODESET
}	XPathValueType;

typedef struct XPathValueData
{
	/*
	 * Even though 'char' is used for storage, 'XPathValueType' is the actual
	 * type.
	 */
	unsigned char type;
	union
	{
		/*
		 * Where the node (or document fragment in case multiple nodes are
		 * stored here) stargs.
		 */
		XMLNodeOffset nodeSetRoot;
		double		numVal;
		bool		booVal;
		char		strVal[1];
	}			v;
}	XPathValueData;

typedef struct XPathValueData *XPathValue;

typedef struct varlena xpathvaltype;
typedef xpathvaltype *xpathval;

extern Datum xpathval_in(PG_FUNCTION_ARGS);
extern Datum xpathval_out(PG_FUNCTION_ARGS);
extern Datum xpathval_to_xmlnode(PG_FUNCTION_ARGS);

/*
 * All nodes in the set are supposed to be of the same kind
 */
typedef struct XPathNodeSetData
{
	unsigned short count;
	bool		isDocument;
	union
	{
		XMLNodeHeader single;
		XMLNodeHeader *array;
	}			nodes;
}	XPathNodeSetData;

typedef struct XPathNodeSetData *XPathNodeSet;

typedef struct XPathStringData
{
	char	   *str;

	/*
	 * Sometimes 'str' just points to inside an existing node, sometimes it
	 * has been palloc'd separate.
	 */
	bool		mustFree;
}	XPathStringData;

typedef XPathStringData *XPathString;

#define XPATH_FUNC_NAME_MAX_LEN 16
#define XPATH_FUNC_MAX_ARGS		4


typedef enum XPathFunctionId
{
	XPATH_FUNC_POSITION = 0,
	XPATH_FUNC_CONTAINS,
	XPATH_FUNC_COUNT
}	XPathFunctionId;

typedef union XPathExprGenericValue
{
	bool		boolean;
	double		num;
	XPathStringData string;
	unsigned short path;
	XPathNodeSetData nodeSet;
	XPathFunctionId funcId;
}	XPathExprGenericValue;

typedef struct XPathExprOperandValueData
{
	unsigned char type;

	/*
	 * isNull - true when element being tested does contain node (attribute,
	 * text node) corresponding to this operand. At the beginning of each test
	 * we assume such node doesn't exist, so we set 'isNull' to true when
	 * parsing the path expression. Once the attribute / text node is found,
	 * 'isNull' is set to false.
	 */
	bool		isNull;
	bool		castToNumber;

	/*
	 * The union must be the last member of the structure
	 */
	XPathExprGenericValue v;
}	XPathExprOperandValueData;

typedef struct XPathExprOperandValueData *XPathExprOperandValue;

/*
 * The literal string (either string constant or attribute name) is stored
 * immediately after the operand structure
 */
#define XPATH_GEN_VALUE_STRING(value) (((char *) (value)) + sizeof(XPathExprOperandValueData))

typedef struct XPathExprOperandData
{
	unsigned char type;

	/*
	 * substituted - true if attribute name has already been substituted with
	 * attribute value that we found in the element being tested.
	 */
	bool		substituted;

	/*
	 * If the structure is followed by a string (literal or attribute name),
	 * its length (including terminating NULL) is includes here.
	 */
	unsigned short size;
	/* value must be the last attribute of this structure */
	XPathExprOperandValueData value;
}	XPathExprOperandData;

typedef struct XPathExprOperandData *XPathExprOperand;

#define XPATH_EXPR_OPERATOR_KINDS	8

/*
 * The order must be identical to that in 'opStrings' array, see xpath.c
 */
typedef enum XPathExprOperatorId
{
	XPATH_EXPR_OPERATOR_LTE,
	XPATH_EXPR_OPERATOR_LT,
	XPATH_EXPR_OPERATOR_GTE,
	XPATH_EXPR_OPERATOR_GT,
	XPATH_EXPR_OPERATOR_EQ,
	XPATH_EXPR_OPERATOR_NEQ,
	XPATH_EXPR_OPERATOR_AND,
	XPATH_EXPR_OPERATOR_OR
}	XPathExprOperatorId;

/*
 * Binary operators
 */
typedef struct XPathExprOperatorData
{
	XPathExprOperatorId id;
	unsigned char precedence;
	XPathValueType resType;
}	XPathExprOperatorData;

typedef struct XPathExprOperatorData *XPathExprOperator;

/*
 * This is to be used by parser
 */
typedef struct XPathExprOperatorTextData
{
	XPathExprOperatorData op;
	char	   *text;
}	XPathExprOperatorTextData;

typedef struct XPathExprOperatorTextData *XPathExprOperatorText;


typedef void (*XpathFuncImpl) (unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result);

typedef struct XPathFunctionData
{
	XPathFunctionId id;
	char		name[XPATH_FUNC_NAME_MAX_LEN];
	unsigned short nargs;		/* Number of arguments */
	XPathValueType argTypes[XPATH_FUNC_MAX_ARGS];
	XpathFuncImpl impl;
	XPathValueType resType;
	bool		predicateOnly;	/* May the function only appear within a
								 * predicate? */
}	XPathFunctionData;

typedef struct XPathFunctionData *XPathFunction;

/* Total number of XPath functions the parser can recognize. */
#define XPATH_FUNCTIONS			3

XPathFunctionData xpathFunctions[XPATH_FUNCTIONS];

/*
 * If type is XPATH_OPERAND_EXPR_TOP, the header is followed by array of
 * offsets pointing to variables (attributes or text nodes).
 * Operands/operators are stored then, regardless the expression type (top
 * level or subexpression).
 */
typedef struct XPathExpressionData
{
	unsigned char type;
	unsigned char flags;
	unsigned short size;
	unsigned short variables;
	unsigned short members;

	unsigned short npaths;

	/*
	 * 'true' if there's no relative path in the expression nor an attribute
	 * operand in the main expression. Set only for the main expression.
	 */
	bool		mainExprAbs;

	unsigned short nfuncs;
	bool		hasNodesets;

	/*
	 * Identifier of a function to be applied when the expression (argument
	 * list) is done. Only set if type is XPATH_OPERAND_FUNC
	 */
	XPathFunctionId funcId;

	XPathValueType valType;
}	XPathExpressionData;

#define XPATH_SUBEXPRESSION_EXPLICIT		(1 << 0)

typedef struct XPathExpressionData *XPathExpression;

extern XPath getSingleXPath(XPathExpression expr, XPathHeader xpHdr);

typedef struct XPathElementData
{
	/*
	 * If the location path element has a predicate, it's located immediately
	 * after the name.
	 */
	bool		hasPredicate;
	bool		descendant;
	char		name[1];
}	XPathElementData;

typedef struct XPathElementData *XPathElement;

typedef struct XPathParserStateData
{
	/*
	 * Human-readable position, i.e. starts at 1 Increased always by one, even
	 * if multi-byte character has been processed.
	 */
	unsigned short pos;
	unsigned int elementPos;
	char	   *c;
	unsigned short cWidth;
	char	   *result,
			   *output;
	unsigned short outChunks;

	/*
	 * Strictly, this attribute is redundant. The purpose is to eliminate need
	 * for evaluating (outChunks * XPATH_PARSER_OUTPUT_CHUNK) each time the
	 * free space in 'result' is being checked.
	 */
	unsigned int outSize;
}	XPathParserStateData;

typedef struct XPathParserStateData *XPathParserState;

#define XPATH_NODE_TYPE_MAX_LEN 22		/* 'processing-instruction' */
#define XPATH_NODE_TYPES_COUNT	4

typedef enum XPathNodeType
{
	XPATH_NODE_TYPE_COMMENT = 0,
	XPATH_NODE_TYPE_TEXT,
	XPATH_NODE_TYPE_NODE,
	XPATH_NODE_TYPE_PI
}	XPathNodeType;


extern		XPath parseLocationPath(XPath * subpaths, bool isSubPath, unsigned short *subpathCnt, char **xpathPtr,
				  unsigned short *pos);
extern		XPathExprOperator parseXPathExpression(XPathExpression exprCurrent, XPathParserState state,
					 char term, XPathExprOperator firstOperator, char *output, unsigned short *outPos, bool isSubExpr,
  bool argList, XPath * subpaths, unsigned short *subpathCnt, bool mainExpr);

/*
 * Status of XML tree scan at given level of the tree.
 */
typedef struct XMLScanOneLevelData
{
	XMLElementHeader parent;
	char	   *nodeRef;
	unsigned short int siblingsLeft;
	unsigned short matches;
	struct XMLScanOneLevelData *up;
}	XMLScanOneLevelData;

typedef struct XMLScanOneLevelData *XMLScanOneLevel;

typedef struct XMLScanData
{
	XPath		xpath;

	/*
	 * Even if the current 'xpath' expression is known, we need the XPathHdr
	 * sometimes to get sub-path(s).
	 */
	XPathHeader xpathHeader;
	unsigned short xpathRoot;
	XMLScanOneLevel scanState;
	unsigned short int currentDepth;

	/*
	 * 'true' indicates that we only need to proceed to the next node instead
	 * of checking the current one. Set this to 'true' when 1. finished
	 * subtree scan (whether the subtree was scanned in the current scan or in
	 * a 'subscan'), 2. returning a matching node. When the scan afterwards
	 * resumes, this flag indicates that the matching node shouldn't be
	 * checked again.
	 */
	bool		skip;
	/* 'true' if just returned from subtree scan. */
	bool		subtreeDone;

	bool		descsProcessed;
	bool done;

	/* The document is needed for absolute sub-paths. */
	xmldoc		document;

	/*
	 * Intermediate resulting node-set. This is used in special cases where
	 * the scan algorithm might return some nodes multiple times. If this scan
	 * has sub-scan(s), they all share an instance of the container.
	 */
	XMLNodeContainer resTmp;
	struct XMLScanData *subScan;
	struct XMLScanData *parent;
}	XMLScanData;

typedef struct XMLScanData *XMLScan;

#define XMLSCAN_CURRENT_LEVEL(xscan) ((xscan)->scanState + (xscan)->currentDepth)

typedef struct XMLScanContextData
{
	XMLScan		baseScan;
	unsigned int columns;
	xpath	   *colPaths;
	Datum	   *colResults;
	bool	   *colResNulls;
	Oid			outArrayType,
				outElmType;
	int			outElmLen;
	bool		outElmByVal;
	char		outElmalign;
}	XMLScanContextData;

typedef struct XMLScanContextData *XMLScanContext;

extern		XPathExpression prepareXPathExpression(XPathExpression exprOrig, XMLElementHeader ctxElem,
					   xmldoc document, XPathHeader xpHdr, XMLScan xscan);
extern void evaluateXPathExpression(XPathExpression expr, XMLScanOneLevel scan,
						XMLElementHeader element, unsigned short recursionLevel, XPathExprOperandValue result);

extern void xpathValCastToBool(XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst);
extern void xpathValCastToNum(XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst);
extern void xpathValCastToStr(XPathExprOperandValue valueSrc, XPathExprOperandValue valueDst);
extern void xpathStrFree(XPathExprOperandValue strValue);

extern void xpathCount(unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result);
extern void xpathContains(unsigned short nargs, XPathExprOperandValue args, XPathExprOperandValue result);

#endif   /* XPATH_H_ */
