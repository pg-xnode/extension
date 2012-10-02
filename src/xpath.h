/*
 * Copyright (C) 2012, Antonin Houska
 */

#ifndef XPATH_H_
#define XPATH_H_

#include "postgres.h"
#include "lib/stringinfo.h"

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

/*
 * Flags to represent terminating characters for given context.
 */
#define XPATH_TERM_NULL						(1 << 0)
#define XPATH_TERM_RBRKT					(1 << 1)
#define XPATH_TERM_RBRKT_RND				(1 << 2)
#define XPATH_TERM_RBRKT_CRL				(1 << 3)
#define XPATH_TERM_COMMA					(1 << 4)

extern bool validXPathTermChar(char c, unsigned char flags);

#define XPATH_LAST_LEVEL(xsc) ((xsc->depth + 1) == (xsc->xpath->depth - xsc->xpathRoot))
#define XPATH_CURRENT_LEVEL(xsc)  ((XPathElement) ((char *) xsc->xpath + xsc->xpath->elements[xsc->xpathRoot + xsc->depth]))

/*
 * The primary purpose of 'xpath' type is to avoid repeated parsing of the
 * XPath expression when the same XPath is being used for many rows. A side
 * effect is that the XPath expressions can be stored in a table as a regular
 * type (and possibly used as something like a 'named query').
 */
typedef uint16 XPathOffset;

/*
 * This in fact means 'location path' as opposed to the full XPath expression
 * (see XPathExpressionData structure). It shouldn't be a problem as location
 * path is often considered XPath by users. Specific name like 'XLocPathData'
 * wouldn't look nice.
 *
 * Similarly, XPathHeaderData represents storage for *location paths*.
 */
typedef struct XPathData
{
	bool		relative;
	uint8		depth;			/* 'Number of path elements (steps) */

	/*
	 * How many elements (steps) require search for descendants. It's useful
	 * to know that location path has 2 or more such elements. In such a case
	 * some nodes might be reached by multiple scan - subscan combinations and
	 * therefore uniqueness of nodes returned has to be checked.
	 */
	uint8		descendants;

	uint32		size;			/* Size of the whole XPath, including this
								 * structure */
	uint8		targNdKind;		/* 'uint8' just for storage, values are listed
								 * in 'enum XMLNodeKind' */

	/*
	 * 'true' if the path ends with '@*' Parser only sets the appropriate
	 * value if 'targNdKind' is XMLNODE_ATTRIBUTE. Otherwise 'allAttributes'
	 * is not defined.
	 */
	bool		allAttributes;
	bool		piTestValue;
	XPathOffset elements[1];
}	XPathData;

typedef struct XPathData *XPath;

#define XPATH_MAX_SUBPATHS	16


typedef struct XPathHeaderData
{
	uint8		pathCount;
	uint8		paramCount;
	XPathOffset paramFirst;		/* How far the first parameter name starts. */
	XPathOffset paths[1];
}	XPathHeaderData;

typedef struct XPathHeaderData *XPathHeader;


#define XPATH_SET_MAX_PATHS		0xFF

#define XPATH_HDR_GET_PATH(header, i) ((XPath) (((char *)(header)) + (header)->paths[i]))

typedef enum XPathExprOperandType
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
	XPATH_OPERAND_FUNC_NOARG,

	XPATH_OPERAND_PARAMETER
}	XPathExprOperandType;

typedef enum XPathValueType
{
	XPATH_VAL_BOOLEAN = 0,
	XPATH_VAL_NUMBER,
	XPATH_VAL_STRING,
	XPATH_VAL_NODESET,

	/*
	 * XPathValue type is never set to this this. The only purpose is to
	 * declare that function argument can be of any type.
	 */
	XPATH_VAL_OBJECT
} XPathValueType;

extern char *xpathValueTypes[];

typedef struct XPathValueData
{
	/*
	 * Even though 'char' is used for storage, 'XPathValueType' is the actual
	 * type.
	 */
	uint8		type;
	union
	{
		/*
		 * Where the node (or document fragment in case multiple nodes are
		 * stored here) starts.
		 */
		XMLNodeOffset nodeSetRoot;
		float8		numVal;
		bool		booVal;
		char		strVal[1];
	}			v;
}	XPathValueData;

typedef struct XPathValueData *XPathValue;

typedef struct varlena xpathvaltype;
typedef xpathvaltype *xpathval;

extern Datum xpathval_in(PG_FUNCTION_ARGS);
extern Datum xpathval_out(PG_FUNCTION_ARGS);
extern Datum xpathval_to_bool(PG_FUNCTION_ARGS);
extern Datum xpathval_to_float8(PG_FUNCTION_ARGS);
extern Datum xpathval_to_numeric(PG_FUNCTION_ARGS);
extern Datum xpathval_to_int4(PG_FUNCTION_ARGS);
extern Datum xpathval_to_xmlnode(PG_FUNCTION_ARGS);


/*
 * For internal purposes only.
 * Used to convert variable value to id and back.
 */
typedef enum XPathExprVar
{
	XPATH_VAR_STRING,
	XPATH_VAR_NODE_SINGLE,
	XPATH_VAR_NODE_ARRAY
} XPathExprVar;

/*
 * All nodes in the set are supposed to be of the same kind
 */
typedef struct XPathNodeSetData
{
	uint32		count;
	bool		isDocument;
	union
	{
		/*
		 * The following fields both reference subscripts in the corresponding
		 * arrays allocated temporarily in 'XPathExprStateData'.
		 *
		 * The reason for not using pointers is that XPathNodeSetData is used
		 * for storage. It doesn't matter that this union is transient (values
		 * are substituted at 'prepare time'.)
		 *
		 * XPATH_VAR_NODE_SINGLE and XPATH_VAR_NODE_ARRAY (see XPathExprVar
		 * enumeration above) are used to convert the ids to values and vice
		 * versa.
		 */
		unsigned short nodeId;
		unsigned short arrayId;
	}			nodes;
} XPathNodeSetData;

typedef struct XPathNodeSetData *XPathNodeSet;


#define XPATH_FUNC_NAME_MAX_LEN		16
/* Maximum number of regular arguments */
#define XPATH_FUNC_MAX_ARGS_REG		4
/* Total number of function arguments */
#define XPATH_FUNC_MAX_ARGS			32

typedef enum XPathFunctionId
{
	XPATH_FUNC_TRUE = 0,
	XPATH_FUNC_FALSE,
	XPATH_FUNC_POSITION,
	XPATH_FUNC_LAST,
	XPATH_FUNC_NAME_NOARG,
	XPATH_FUNC_LOCAL_NAME_NOARG,
	XPATH_FUNC_CONTAINS,

	XPATH_FUNC_BOOLEAN,
	XPATH_FUNC_NUMBER,
	XPATH_FUNC_STRING,
	XPATH_FUNC_NAME,
	XPATH_FUNC_LOCAL_NAME,
	XPATH_FUNC_STARTS_WITH,

	XPATH_FUNC_COUNT,
	XPATH_FUNC_CONCAT
} XPathFunctionId;

#define XPATH_PARAM_NAME_MAX_LEN	16	/* Excluding the terminating NULL */
#define XPATH_PARAM_MAX_COUNT		32

/*
 * TODO
 * perhaps divide into 2 structures: one for 'boolean', 'num', 'stringId',
 * 'nodeSet' (i.e. those corresponding to XPathValueType) and the other.
 */
typedef union XPathExprGenericValue
{
	bool		boolean;
	float8		num;

	/*
	 * 0-based index in an array that gets palloc'd right before expression
	 * evaluation. This is a transient variable and we set it before
	 * expression gets evaluated.
	 *
	 * (char *) could be used instead, however that would make size of the
	 * operand platform (compiler) dependent. The current concept is that the
	 * XPath expression is retrieved from storage, copied and used. (No
	 * conversion to a special type used for evaluation only.)
	 *
	 * 'stringId' can represent either string literal or attribute value. In
	 * either case XPATH_VAR_STRING value of XPathExprVar enumeration is used.
	 */
	uint16		stringId;

	uint16		path;
	XPathNodeSetData nodeSet;
	uint8		funcId;
	uint8		paramId;
} XPathExprGenericValue;

typedef struct XPathExprOperandValueData
{
	uint8		type;			/* Which member of the 'union
								 * XPathExprGenericValue'. */

	/*
	 * On evaluation: It makes lot of sense for castXPathExprOperandToNum() to
	 * produce either positive or negative float value where we no longer have
	 * to check the 'negative' attribute. However that brings a requirement
	 * not to flip the sign back and forth by repeated cast.
	 *
	 * Therefore the convention is not to apply the sign to operands or
	 * subexpressions. Instead, it just gets propagated to the top-level
	 * expression. The sign may only be evaluated at the top level.
	 *
	 * evaluateBinaryOperator() is special case in that it does apply the sign
	 * to its arguments. This is o.k. because these 'binary operands' are
	 * replaced by a new value (i.e. nothing like repeated cast can happen to
	 * them afterwards).
	 */
	bool		negative;

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
} XPathExprOperandValueData;

typedef struct XPathExprOperandValueData *XPathExprOperandValue;

/*
 * The literal string (either string constant or attribute name) is stored
 * immediately after the operand structure
 */
#define XPATH_STRING_LITERAL(value) (((char *) (value)) + sizeof(XPathExprOperandValueData))

/*
 * 'XPathExpressionData' is actually a special kind of 'XPathExprOperandData',
 * but we're not always interested in that. That's why it's useful to have
 * a special type to access subset of attributes regardless the actual kind.
 */
typedef struct XPathExprOperandCommonData
{
	uint8		type;

	/*
	 * When used in XPathExprOperandData: if the XPathExprOperandData
	 * structure is followed by a string (literal or attribute name), its
	 * length (including terminating NULL) is included here.
	 */
	uint16		size;
} XPathExprOperandCommonData;

typedef struct XPathExprOperandCommonData *XPathExprOperandCommon;

typedef struct XPathExprOperandData
{
	XPathExprOperandCommonData common;

	/*
	 * substituted - true if attribute name has already been substituted with
	 * attribute value that we found in the element being tested.
	 */
	bool		substituted;

	/* Value must be the last attribute of this structure */
	XPathExprOperandValueData value;
}	XPathExprOperandData;

typedef struct XPathExprOperandData *XPathExprOperand;



#define XPATH_EXPR_OPERATOR_KINDS	14

/*
 * The order must be identical to that in 'xpathOperators' array, see xpath.c
 */
typedef enum XPathExprOperatorId
{
	XPATH_EXPR_OPERATOR_UNION,
	XPATH_EXPR_OPERATOR_MULTIPLY,
	XPATH_EXPR_OPERATOR_DIV,
	XPATH_EXPR_OPERATOR_MOD,
	XPATH_EXPR_OPERATOR_PLUS,
	XPATH_EXPR_OPERATOR_MINUS,
	XPATH_EXPR_OPERATOR_LTE,
	XPATH_EXPR_OPERATOR_LT,
	XPATH_EXPR_OPERATOR_GTE,
	XPATH_EXPR_OPERATOR_GT,
	XPATH_EXPR_OPERATOR_EQ,
	XPATH_EXPR_OPERATOR_NEQ,
	XPATH_EXPR_OPERATOR_AND,
	XPATH_EXPR_OPERATOR_OR
} XPathExprOperatorId;

typedef struct XPathExprOperatorStorageData
{
	/* XPathExprOperatorId is stored in a single byte. */
	uint8		id;
}	XPathExprOperatorStorageData;

typedef struct XPathExprOperatorStorageData *XPathExprOperatorStorage;

/*
 * Binary operators
 */
typedef struct XPathExprOperatorData
{
	XPathExprOperatorId id;
	uint8		precedence;
	uint8		resType;
} XPathExprOperatorData;

typedef struct XPathExprOperatorData *XPathExprOperator;

/*
 * This is to be used by parser
 */
typedef struct XPathExprOperatorTextData
{
	XPathExprOperatorData op;
	char	   *text;
} XPathExprOperatorTextData;

typedef struct XPathExprOperatorTextData *XPathExprOperatorText;

XPathExprOperatorTextData xpathOperators[XPATH_EXPR_OPERATOR_KINDS];

/*
 * Get XPathExprOperator from the storage.
 * 'idPtr' is pointer to the operator's id in the XPath storage.
 */
#define XPATH_EXPR_OPERATOR(idPtr) (((idPtr) != NULL) ?\
	(&((xpathOperators + ((XPathExprOperatorStorage) (idPtr))->id)->op)) : NULL)

/*
 * If type is XPATH_OPERAND_EXPR_TOP, the header is followed by array of
 * offsets pointing to variables (attributes or text nodes).
 * Operands/operators are stored then, regardless the expression type (top
 * level or subexpression).
 */
typedef struct XPathExpressionData
{
	XPathExprOperandCommonData common;

	uint8		flags;

	bool		negative;

	/* Only defined where type == XPATH_OPERAND_EXPR_TOP. */
	uint16		variables;

	uint16		members;

	uint16		nlits;			/* Number of string literals. */

	uint16		npaths;

	/*
	 * 'true' if there's no relative path in the expression nor an attribute
	 * operand in the main expression. Set only for the main expression.
	 */
	bool		mainExprAbs;

	uint16		nfuncs;

	/*
	 * Identifier of a function to be applied when the expression (argument
	 * list) is evaluated. Only set if type is XPATH_OPERAND_FUNC
	 */
	uint8		funcId;

	uint8		valType;
}	XPathExpressionData;

typedef struct XPathExpressionData *XPathExpression;

#define XPATH_SUBEXPRESSION_EXPLICIT		(1 << 0)


extern void dumpXPathExpression(XPathExpression expr, XPathHeader xpathHdr, StringInfo output, bool main,
					char **paramNames, bool debug);
extern void dumpLocationPath(XPathHeader xpathHdr, unsigned short pathNr, StringInfo output, char **paramNames,
				 bool debug);

extern XPath getSingleXPath(XPathExpression expr, XPathHeader xpHdr);

/*
 * Element of location path, i.e. location step.
 */
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

} XPathParserStateData;

typedef struct XPathParserStateData *XPathParserState;

typedef struct LocationPathOutput
{
	char	   *data,
			   *cursor;
	unsigned short chunks;

	/*
	 * Strictly, this attribute is redundant. The purpose is to eliminate need
	 * for evaluating (chunks * XPATH_PARSER_OUTPUT_CHUNK) each time the free
	 * space in 'data' is being checked.
	 */
	unsigned int size;
} LocationPathOutput;


#define XPATH_NODE_TYPE_MAX_LEN 22		/* 'processing-instruction' */
#define XPATH_NODE_TYPES_COUNT	4

typedef enum XPathNodeType
{
	XPATH_NODE_TYPE_COMMENT = 0,
	XPATH_NODE_TYPE_TEXT,
	XPATH_NODE_TYPE_NODE,
	XPATH_NODE_TYPE_PI
}	XPathNodeType;


/*
 * Alignment information is summarized at one place so that
 * dependencies are obvious.
 */

/* XPathOffset */
#define XPATH_ALIGNOF_OFFSET	ALIGNOF_SHORT

/*
 * XPathHeaderData
 *
 * The following structures (XPathData) don't affect this alignment, because
 * their position is not derived from the preceding structures. Instead,
 * XPathHeaderData contains offset of each XPathData.
 */
#define XPATH_ALIGNOF_PATH_HDR	ALIGNOF_SHORT

/*
 * XPathExprOperandData
 *
 * 'XPathExprGenericValue.num' member causes such a high alignment
 */
#define XPATH_ALIGNOF_OPERAND	ALIGNOF_DOUBLE

/*
 * XPathExpressionData
 *
 * Expression is a special type of operand.
 * Alignment must be XPATH_ALIGNOF_OPERAND even though the members
 * don't demand such. The point is that we use TYPEALIGN() macro to
 * derive position of each next operand, while we don't know yet if
 * will actually be a XPathExpressionData or XPathExprOperandData.
 */
#define XPATH_ALIGNOF_EXPR	XPATH_ALIGNOF_OPERAND

/*
 * XPathElementData
 *
 * Alignment must be identical to that of expression because position of node
 * test predicate expression is derived from position of the owning
 * XPathElementData.
 */
#define XPATH_ALIGNOF_LOC_STEP	XPATH_ALIGNOF_EXPR

/*
 * XPathData
 *
 * The structure itself would not require such a high alignment, but
 * the path contains expressions (see comment for XPATH_ALIGNOF_EXPR)
 */
#define XPATH_ALIGNOF_LOC_PATH	MAXIMUM_ALIGNOF

/*
 * XPathValueData
 *
 * Contains float8.
 */
#define XNODE_ALIGNOF_XPATHVAL	ALIGNOF_DOUBLE


extern XPathExprOperatorStorage parseXPathExpression(XPathExpression exprCurrent, XPathParserState state,
					 unsigned char termFlags, XPathExprOperatorStorage firstOperatorStorage, char *output,
		  unsigned short *outPos, bool isSubExpr, bool argList, XPath *paths,
	 unsigned short *subpathCnt, bool mainExpr, XMLNodeContainer paramNames);

extern void parseLocationPath(XPath *paths, bool isSubPath, unsigned short *pathCount, char **xpathSrc,
				  unsigned short *pos, XMLNodeContainer paramNames);

/*
 * Status of XML tree scan at given level of the tree.
 */
typedef struct XMLScanOneLevelData
{
	XMLCompNodeHdr parent;

	/* Where the next (multi-byte) reference will be read from. */
	char	   *nodeRefPtr;

	unsigned short int siblingsLeft;

	unsigned short contextPosition;

	unsigned short contextSize;
	bool		contextSizeKnown;

	struct XMLScanOneLevelData *up;
}	XMLScanOneLevelData;

typedef struct XMLScanOneLevelData *XMLScanOneLevel;

typedef struct XMLScanData
{
	XPath xpath;

	/*
	 * Even if the current 'xpath' expression is known, we need the
	 * XPathHeader sometimes to get sub-path(s).
	 */
	XPathHeader xpathHeader;
	unsigned short xpathRoot;
	XMLScanOneLevel state;
	/* Current depth */
	unsigned short int depth;

	/*
	 * 'true' indicates that we only need to proceed to the next node instead
	 * of checking the current one. Set this to 'true' when 1. finished
	 * subtree scan (whether the subtree was scanned in the current scan or in
	 * a 'subscan'), 2. returning a matching node. When the scan afterwards
	 * resumes, this flag indicates that the matching node shouldn't be
	 * checked again.
	 */
	bool		skip;
	bool		done;

	/* The document is needed for absolute sub-paths. */
	xmldoc		document;

	/*
	 * Intermediate resulting node-set. This is used in special cases where
	 * the scan algorithm might return some nodes multiple times.
	 *
	 * New nodes are also added to the the ignore list in order to prevent
	 * infinite recursions during the addition.
	 *
	 * If the current scan has sub-scan(s), they all share an instance of the
	 * container.
	 */
	XMLNodeContainer ignoreList;

	/* Direct child in the scan hierarchy. */
	struct XMLScanData *subScan;

	/* Direct parent in the scan hierarchy. */
	struct XMLScanData *parent;
} XMLScanData;

typedef struct XMLScanData *XMLScan;

#define XMLSCAN_CURRENT_LEVEL(xscan) ((xscan)->state + (xscan)->depth)

typedef struct XMLScanContextData
{
	XMLScan		baseScan;
	unsigned int columns;

	/*
	 * Even though the expression can be derived from the header, both are
	 * stored for better performance.
	 */
	XPathHeader *colHeaders;
	XPathExpression *colExpressions;

	Datum	   *colResults;
	bool	   *colResNulls;
	Oid			outArrayType,
				outElmType;
	int16		outElmLen;
	bool		outElmByVal;
	char		outElmalign;
}	XMLScanContextData;

typedef struct XMLScanContextData *XMLScanContext;

extern void initXMLScan(XMLScan xscan, XMLScan parent, XPath xpath, XPathHeader xpHdr, XMLCompNodeHdr scanRoot,
			xmldoc document, bool checkUniqueness);
extern void finalizeXMLScan(XMLScan xscan);

extern void initScanForTextNodes(XMLScan xscan, XMLCompNodeHdr root);
extern void finalizeScanForTextNodes(XMLScan xscan);

/*
 * Expression evaluation state.
 */
typedef struct XPathExprStateData
{
	XPathExpression expr;

	unsigned short count[3];
	unsigned short countMax[3];

	char	  **strings;
	XMLNodeHdr *nodes;
	XMLNodeHdr **nodeSets;
}	XPathExprStateData;

typedef struct XPathExprStateData *XPathExprState;

#define XPATH_VAR_CACHE_DEF_SIZE	16

extern XPathExprState prepareXPathExpression(XPathExpression exprOrig, XMLCompNodeHdr ctxElem,
					   xmldoc document, XPathHeader xpHdr, XMLScan xscan);
extern XPathExprState createXPathVarCache(unsigned int size);
extern void allocXPathExpressionVarCache(XPathExprState state, XPathExprVar varKind, unsigned int increment, bool init);
extern void evaluateXPathExpression(XPathExprState exprState, XPathExpression expr,
				unsigned short recursionLevel, XPathExprOperandValue result);

extern void freeExpressionState(XPathExprState state);

extern void castXPathExprOperandToBool(XPathExprState exprState, XPathExprOperandValue valueSrc,
						   XPathExprOperandValue valueDst);
extern void castXPathExprOperandToNum(XPathExprState exprState, XPathExprOperandValue valueSrc,
						  XPathExprOperandValue valueDst, bool raiseError);
extern void castXPathExprOperandToStr(XPathExprState exprState, XPathExprOperandValue valueSrc,
						  XPathExprOperandValue valueDst);

extern bool castXPathValToBool(XPathValue src);
extern float8 castXPathValToNum(XPathValue src);
extern char *castXPathValToStr(XPathValue src);

extern unsigned short getXPathOperandId(XPathExprState exprState, void *value, XPathExprVar varKind);
extern void *getXPathOperandValue(XPathExprState exprState, unsigned short id, XPathExprVar varKind);
extern XPathHeader getXPathHeader(xpath xpathValue);
extern XPathExpression getXPathExpressionFromStorage(XPathHeader xpathHeader);
extern char **getXPathParameterArray(XPathHeader xpathHeader);

typedef void (*XpathFuncImpl) (XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
										   XPathExprOperandValue result);
typedef void (*XpathFuncImplNoArgs) (XMLScan scan, XPathExprState exprState, XPathExprOperandValue result);

typedef struct XPathFunctionData
{
	XPathFunctionId id;
	char		name[XPATH_FUNC_NAME_MAX_LEN];

	/* Regular arguments */
	unsigned short nargs;
	XPathValueType argTypes[XPATH_FUNC_MAX_ARGS_REG];

	/*
	 * Some function may have additional arguments, all having the same type
	 * as last item of 'argTypes'.
	 */
	bool		nargsSoftLimit;

	union
	{
		XpathFuncImpl args;
		XpathFuncImplNoArgs noargs;
	}			impl;

	XPathValueType resType;
	bool		predicateOnly;	/* May the function only appear within a
								 * predicate? */
} XPathFunctionData;

typedef struct XPathFunctionData *XPathFunction;

/* Total number of XPath functions the parser can recognize. */
#define XPATH_FUNCTIONS			15

XPathFunctionData xpathFunctions[XPATH_FUNCTIONS];

/*
 * First, functions with no arguments.
 */
extern void xpathTrue(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result);
extern void xpathFalse(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result);
extern void xpathPosition(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result);
extern void xpathLast(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result);
extern void xpathNameNoArgs(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result);
extern void xpathLocalNameNoArgs(XMLScan xscan, XPathExprState exprState, XPathExprOperandValue result);

/*
 * And then those with non-empty argument list.
 */
extern void xpathBoolean(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			 XPathExprOperandValue result);
extern void xpathNumber(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			XPathExprOperandValue result);
extern void xpathString(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			XPathExprOperandValue result);
extern void xpathName(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
		  XPathExprOperandValue result);
extern void xpathLocalName(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			   XPathExprOperandValue result);
extern void xpathStartsWith(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
				XPathExprOperandValue result);

extern void xpathCount(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
		   XPathExprOperandValue result);
extern void xpathContains(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			  XPathExprOperandValue result);
extern void xpathConcat(XPathExprState exprState, unsigned short nargs, XPathExprOperandValue args,
			XPathExprOperandValue result);

#endif   /* XPATH_H_ */
