/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "access/htup.h"
#include "access/tupdesc.h"
#include "catalog/pg_cast.h"
#include "catalog/pg_proc.h"
#include "catalog/pg_type.h"
#include "utils/array.h"
#include "utils/lsyscache.h"
#include "utils/builtins.h"
#include "utils/syscache.h"
#include "utils/typcache.h"

#ifndef XNT_H_
#define XNT_H_

#define XNTNODE_NAMESPACE_PREFIX		"xnt"
#define XNTNODE_NAMESPACE_VALUE			"http://www.pg-xnode.org/xnt"

#define XNT_TEMPLATE			"template"

#define XNT_COPY_OF				"copy-of"
#define XNT_COPY_OF_EXPR		0

#define XNT_ELEMENT				"element"
#define XNT_ELEMENT_NAME		0

#define XNT_ATTRIBUTE			"attribute"
#define XNT_ATTRIBUTE_NAME		0
#define XNT_ATTRIBUTE_VALUE		1


/*
 * Information on special attribute names defined for each XNT tag.
 */

#define XNT_SPECIAL_ATTRS_MAX	4

typedef struct XNTAttrNames
{
	/* Number of attributes. */
	unsigned short number;

	/*
	 * The order of names determines the order used to store the attributes.
	 * With such a defined order we don't need to store attribute names.
	 */
	char	   *names[XNT_SPECIAL_ATTRS_MAX];
	bool		required[XNT_SPECIAL_ATTRS_MAX];
} XNTAttrNames;

/*
 * If attribute value contains xpath expressions, it's understood as a sequence of
 * tokens where token is either a NULL-terminated string or XPathExpressionData.
 */
#define XNT_ATTR_VALUE_MAX_TOKENS		16

typedef struct XNTHeaderData
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
} XNTHeaderData;

typedef struct XNTHeaderData *XNTHeader;

typedef struct XNTParamNameSorted
{
	unsigned short order;
	char	   *name;
} XNTParamNameSorted;

/*
 * In-memory structure to represent a single node of the tree.
 */
typedef struct XNodeInternalData *XNodeInternal;

typedef struct XNodeInternalData
{
	/* Pointer to node or subtree of the storage tree. */
	XMLNodeHdr	node;

	/*
	 * 'true' if 'node' points to a palloc'd copy instead of storage. It
	 * indicates that 'node' has to be freed when the template is no longer
	 * needed.
	 */
	bool		copy;

	XMLNodeContainerData children;
} XNodeInternalData;

typedef struct varlena xnttype;
typedef xnttype *xnt;

extern Datum xnode_template_in(PG_FUNCTION_ARGS);
extern Datum xnode_template_out(PG_FUNCTION_ARGS);

extern XMLNodeKind getXNTNodeKind(char *name);
extern char *getXNTNodeName(XMLNodeKind kind);
extern char *getXNTAttributeName(XMLNodeKind kind, unsigned short attrNr);
extern void validateXNTTree(XMLNodeHdr root);

extern char *preprocessXNTAttributes(XNodeListItem *attrOffsets, unsigned short attrCount, char *parserOutput,
						XMLNodeKind specNodeKind, bool *offsetsValid, unsigned int *specAttrCount, unsigned int *outSize,
						unsigned int *outCount, XMLNodeContainer paramNames);
extern char *preprocessXNTAttrValues(XNodeListItem *attrOffsets, unsigned short attrCount, char *parserOutput, unsigned int *outSize,
				   XMLNodeContainer paramNames, XMLNodeContainer substNodes);
extern char *dumpBinaryAttrValue(char *binValue, char **paramNames, XPathExprOperandValue paramValues,
					unsigned short *paramMap, XPathExprState exprState);

extern Datum xnode_from_template(PG_FUNCTION_ARGS);

#endif   /* XNT_H_ */
