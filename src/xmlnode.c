/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"
#include "funcapi.h"
#include "catalog/pg_proc.h"
#include "mb/pg_wchar.h"
#include "utils/array.h"
#include "utils/builtins.h"
#include "utils/lsyscache.h"
#include "utils/palloc.h"
#include "utils/syscache.h"

#include "xmlnode.h"
#include "xmlnode_util.h"
#include "xml_parser.h"
#include "xnt.h"

/*
 * TODO error handling: use 'ereport()' and define numeric error codes.
 */

/*
 * Intervals defined in http://www.w3.org/TR/xml/#NT-NameStartChar converted
 * to UTF-8.
 */
UTF8Interval nameStartCharIntervals[XNODE_NAME_START_CHAR_INTERVALS] =
{
	{{0x41, 0x00, 0x00, 0x00}, {0x5a, 0x00, 0x00, 0x00}},
	{{0x61, 0x00, 0x00, 0x00}, {0x7a, 0x00, 0x00, 0x00}},
	{{0xc3, 0x80, 0x00, 0x00}, {0xc3, 0x96, 0x00, 0x00}},
	{{0xc3, 0x80, 0x00, 0x00}, {0xc3, 0x96, 0x00, 0x00}},
	{{0xc3, 0x98, 0x00, 0x00}, {0xc3, 0xb6, 0x00, 0x00}},
	{{0xc3, 0xb8, 0x00, 0x00}, {0xcb, 0xbf, 0x00, 0x00}},
	{{0xcd, 0xb0, 0x00, 0x00}, {0xcd, 0xbd, 0x00, 0x00}},
	{{0xcd, 0xbf, 0x00, 0x00}, {0xe1, 0xbf, 0xbf, 0x00}},
	{{0xe2, 0x80, 0x8c, 0x00}, {0xe2, 0x80, 0x8d, 0x00}},
	{{0xe2, 0x81, 0xb0, 0x00}, {0xe2, 0x86, 0x8f, 0x00}},
	{{0xe2, 0xb0, 0x80, 0x00}, {0xe2, 0xbf, 0xaf, 0x00}},
	{{0xe3, 0x80, 0x81, 0x00}, {0xed, 0x9f, 0xbf, 0x00}},
	{{0xef, 0xa4, 0x80, 0x00}, {0xef, 0xb7, 0x8f, 0x00}},
	{{0xef, 0xb7, 0xb0, 0x00}, {0xef, 0xbf, 0xbd, 0x00}},
	{{0xf0, 0x90, 0x80, 0x80}, {0xf3, 0xaf, 0xbf, 0xbf}}
};

/*
 * http://www.w3.org/TR/xml/#NT-NameChar
 */

UTF8Interval nameCharIntervals[XNODE_NAME_CHAR_INTERVALS] =
{
	{{0x30, 0x00, 0x00, 0x00}, {0x39, 0x00, 0x00, 0x00}},
	{{0xc2, 0xb7, 0x00, 0x00}, {0xc2, 0xb7, 0x00, 0x00}},
	{{0xcc, 0x80, 0x00, 0x00}, {0xcd, 0xaf, 0x00, 0x00}},
	{{0xe2, 0x80, 0xbf, 0x00}, {0xe2, 0x81, 0x80, 0x00}},
};

static void getNodeInfo(XMLNodeHdr node, unsigned int *size, unsigned short *count);
static int	attrNameComparator(const void *left, const void *right);

typedef struct TypeInfo
{
	Oid			oid;
	int16		elmlen;
	bool		elmbyval;
	char		elmalign;
} TypeInfo;

static void
initXNodeTypeInfo(Oid fnOid, int argNr, TypeInfo *ti)
{
	HeapTuple	tup;
	Form_pg_proc procStruct;

	tup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fnOid));
	Assert(HeapTupleIsValid(tup));
	procStruct = (Form_pg_proc) GETSTRUCT(tup);
	ti->oid = procStruct->proargtypes.values[argNr];
	ReleaseSysCache(tup);
	get_typlenbyvalalign(ti->oid, &ti->elmlen, &ti->elmbyval, &ti->elmalign);
}


PG_MODULE_MAGIC;

PG_FUNCTION_INFO_V1(xmlnode_in);

Datum
xmlnode_in(PG_FUNCTION_ARGS)
{
	pg_enc		dbEnc;
	XMLNodeParserStateData parserState;
	char	   *input = PG_GETARG_CSTRING(0);

	if (strlen(input) == 0)
	{
		elog(ERROR, "zero length input string");
	}
	dbEnc = GetDatabaseEncoding();
	initXMLParserState(&parserState, input, XMLNODE_NODE, NULL);
	xmlnodeParseNode(&parserState);
	finalizeXMLParserState(&parserState);
	PG_RETURN_POINTER(parserState.result);
}


PG_FUNCTION_INFO_V1(xmlnode_out);

Datum
xmlnode_out(PG_FUNCTION_ARGS)
{

	xmlnode		node = (xmlnode) PG_GETARG_VARLENA_P(0);
	char	   *data = (char *) VARDATA(node);
	XMLNodeOffset rootNdOff = XNODE_ROOT_OFFSET(node);

	PG_RETURN_CSTRING(dumpXMLNode(data, rootNdOff));
}

PG_FUNCTION_INFO_V1(xmlnode_kind);

Datum
xmlnode_kind(PG_FUNCTION_ARGS)
{
	xmlnode		nodeRaw = (xmlnode) PG_GETARG_VARLENA_P(0);
	XMLNodeHdr	node = XNODE_ROOT(nodeRaw);
	char	   *kindStr = getXMLNodeKindStr(node->kind);

	PG_RETURN_TEXT_P(cstring_to_text(kindStr));
}


PG_FUNCTION_INFO_V1(xmlnode_debug_print);

Datum
xmlnode_debug_print(PG_FUNCTION_ARGS)
{
	xmlnode		nodeRaw = (xmlnode) PG_GETARG_VARLENA_P(0);
	char	   *data = (char *) VARDATA(nodeRaw);
	StringInfo	output = makeStringInfo();

	dumpXMLNodeDebug(output, data, XNODE_ROOT_OFFSET(nodeRaw));
	PG_RETURN_TEXT_P(cstring_to_text(output->data));
}

PG_FUNCTION_INFO_V1(xmldoc_in);

Datum
xmldoc_in(PG_FUNCTION_ARGS)
{
	pg_enc		dbEnc;
	XMLNodeParserStateData parserState;
	char	   *input = PG_GETARG_CSTRING(0);

	if (strlen(input) == 0)
	{
		elog(ERROR, "zero length input string");
	}
	dbEnc = GetDatabaseEncoding();
	if (dbEnc != PG_UTF8)
	{
		elog(ERROR, "The current version of xmlnode requires both database encoding to be UTF-8.");
	}
	initXMLParserState(&parserState, input, XMLNODE_DOC, NULL);
	xmlnodeParseDoc(&parserState);
	finalizeXMLParserState(&parserState);
	PG_RETURN_POINTER(parserState.result);
}

PG_FUNCTION_INFO_V1(xmldoc_out);

Datum
xmldoc_out(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);
	char	   *data = (char *) VARDATA(doc);
	XMLNodeOffset rootNdOff = XNODE_ROOT_OFFSET(doc);

	PG_RETURN_CSTRING(dumpXMLNode(data, rootNdOff));
}


PG_FUNCTION_INFO_V1(xmlnode_to_xmldoc);

Datum
xmlnode_to_xmldoc(PG_FUNCTION_ARGS)
{
	XMLCompNodeHdr rootNode,
				rootDoc;
	unsigned int sizeNewMax;
	xmlnode		node = (xmlnode) PG_GETARG_VARLENA_P(0);
	xmldoc		document = NULL;
	char	   *docData;
	unsigned int sizeOrig = VARSIZE(node);
	unsigned int dataSizeOrig = sizeOrig - VARHDRSZ;
	char	   *nodeData = (char *) VARDATA(node);

	XMLNodeOffset sizeToCopy = dataSizeOrig - sizeof(XMLNodeOffset);
	XMLNodeOffset rootOffsetOrig = XNODE_ROOT_OFFSET(node);

	rootNode = (XMLCompNodeHdr) (nodeData + rootOffsetOrig);

	if (rootNode->common.kind == XMLNODE_ELEMENT || rootNode->common.kind == XMLNODE_DOC)
	{
		unsigned int unresolvedNmspcCount;
		char	  **unresolvedNamespaces = getUnresolvedXMLNamespaces((XMLNodeHdr) rootNode, &unresolvedNmspcCount);

		if (unresolvedNmspcCount > 0)
		{
			/*
			 * Only the first unresolved namespace is printed out. This may be
			 * changed if users appear to prefer complete list.
			 */
			elog(ERROR, "xml element uses unbound namespace '%s' so it can't be converted to xml document",
				 unresolvedNamespaces[0]);
		}
	}

	if (rootNode->common.kind == XMLNODE_ELEMENT)
	{
		/*
		 * If document should contain only one node, it must be element. See
		 * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-document
		 */
		char	   *refTargPtr;
		XMLNodeOffset *rootOffPtrNew;
		XMLNodeOffset dist;
		char		bwidth;

		/*
		 * Add maximum possible padding of document node, the document node
		 * itself, maximum space for 1 reference and maximum padding for the
		 * root node offset (the original document already contains the offset
		 * itself, but it gets moved now so additional padding may be needed.)
		 */
		sizeNewMax = sizeOrig + MAX_PADDING(XNODE_ALIGNOF_COMPNODE) +
			sizeof(XMLCompNodeHdrData) + sizeof(XMLNodeOffset) +
			MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET);

		document = (xmldoc) palloc(sizeNewMax);
		docData = (char *) VARDATA(document);
		memcpy(docData, nodeData, sizeToCopy);

		/*
		 * The document root node will be added as close to the copied chunk
		 * as correct alignment allows.
		 */
		rootDoc = (XMLCompNodeHdr) (docData + TYPEALIGN(XNODE_ALIGNOF_COMPNODE, sizeToCopy));

		rootDoc->common.kind = XMLNODE_DOC;
		rootDoc->common.flags = 0;
		rootDoc->children = 1;

		/*
		 * Compute relative reference of the 'old root' that the document
		 * ('new root') will contain.
		 */
		dist = (char *) rootDoc - docData - rootOffsetOrig;
		bwidth = getXMLNodeOffsetByteWidth(dist);
		XNODE_SET_REF_BWIDTH(rootDoc, bwidth);

		refTargPtr = (char *) rootDoc + sizeof(XMLCompNodeHdrData);
		writeXMLNodeOffset(dist, &refTargPtr, bwidth, true);

		/*
		 * 'refTargPtr' ended up right after the finished document root. We
		 * just need to find the aligned address for the root offset and write
		 * the offset there.
		 */
		rootOffPtrNew = (XMLNodeOffset *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, refTargPtr);
		*rootOffPtrNew = (char *) rootDoc - docData;

		SET_VARSIZE(document, (char *) rootOffPtrNew - (char *) document + sizeof(XMLNodeOffset));
	}
	else if (rootNode->common.kind == XMLNODE_DOC_FRAGMENT)
	{
		checkXMLWellFormedness(rootNode);

		/*
		 * The document root's role is the same like that of the document
		 * fragment: it's a parent of the actual nodes. So we just need to
		 * copy the whole tree and change the root node kind.
		 */
		document = (xmldoc) palloc(sizeOrig);
		docData = (char *) VARDATA(document);
		memcpy(document, node, sizeOrig);

		rootDoc = (XMLCompNodeHdr) (docData + rootOffsetOrig);
		rootDoc->common.kind = XMLNODE_DOC;
		rootDoc->common.flags = 0;
		SET_VARSIZE(document, sizeOrig);
	}
	else
	{
		elog(ERROR, "%s can't be cast to XML document", getXMLNodeKindStr(rootNode->common.kind));
	}
	PG_RETURN_POINTER(document);
}

PG_FUNCTION_INFO_V1(xmldoc_to_xmlnode);

Datum
xmldoc_to_xmlnode(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);
	char	   *docData = VARDATA(doc);
	xmlnode		node;

	XMLCompNodeHdr root = (XMLCompNodeHdr) XNODE_ROOT(doc);

	Assert(root->common.kind == XMLNODE_DOC);

	if (root->children == 1)
	{
		/* The single child (i.e. root element) will be the result of the cast */
		char	   *refPtr = XNODE_FIRST_REF(root);
		unsigned char bwidth = XNODE_GET_REF_BWIDTH(root);
		XMLNodeOffset rootOff = (char *) root - docData;
		XMLNodeOffset childOff = rootOff - readXMLNodeOffset(&refPtr, bwidth, false);
		XMLNodeHdr	child = (XMLNodeHdr) (docData + childOff);

		node = (xmlnode) copyXMLNode(child, NULL, true, NULL);
	}
	else
	{
		/*
		 * Just change type to document fragment and ignore XMLDeclData if one
		 * exists
		 */
		char	   *nodeData,
				   *ptrUnaligned;
		XMLCompNodeHdr rootNew;
		XMLNodeOffset rootOffNew;
		XMLNodeOffset *rootOffPtrNew;

		node = (xmlnode) copyXMLNode((XMLNodeHdr) root, NULL, true, &rootOffNew);
		nodeData = VARDATA(node);
		rootNew = (XMLCompNodeHdr) (nodeData + rootOffNew);
		rootNew->common.kind = XMLNODE_DOC_FRAGMENT;
		rootNew->common.flags = 0;

		/*
		 * The root offset will be stored (aligned) right after the document
		 * fragment header.
		 */
		ptrUnaligned = (char *) XNODE_ELEMENT_NAME(rootNew);
		rootOffPtrNew = (XMLNodeOffset *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, ptrUnaligned);
		*rootOffPtrNew = rootOffNew;

		/*
		 * If the document contained XMLDeclData, the size is going to be
		 * decreased.
		 */
		SET_VARSIZE(node, (char *) rootOffPtrNew - (char *) node + sizeof(XMLNodeOffset));
	}

	PG_RETURN_POINTER(node);
}

char *
dumpXMLNode(char *data, XMLNodeOffset rootNdOff)
{
	unsigned int resultPos;
	char	   *result,
			   *resultTmp;
	XMLNodeHdr	root = (XMLNodeHdr) (data + rootNdOff);
	char	   *declStr = NULL;
	unsigned short declSize = 0;
	char	   *srcCursor = NULL;
	char	  **paramNames = NULL;

	if (root->kind == XMLNODE_DOC_FRAGMENT)
	{
		XMLCompNodeHdr fragment = (XMLCompNodeHdr) root;

		if (checkFragmentForAttributes(fragment))
		{
			elog(ERROR, "document fragment having attributes as direct children can't be dumped.");
		}
	}
	else if (root->kind == XMLNODE_DOC && (root->flags & XNODE_DOC_XMLDECL))
	{
		XMLCompNodeHdr doc = (XMLCompNodeHdr) root;
		XMLDecl		decl = (XMLDecl) XNODE_ELEMENT_NAME(doc);

		declStr = dumpXMLDecl(decl);
		declSize = strlen(declStr);
		srcCursor = (char *) decl + sizeof(XMLDeclData);
	}
	else if (root->kind == XNTNODE_ROOT)
	{
		XNTHeader	xntHdr;
		XMLCompNodeHdr template = (XMLCompNodeHdr) root;

		srcCursor = XNODE_ELEMENT_NAME(template);
		srcCursor = (char *) TYPEALIGN(XNODE_ALIGNOF_XNT_HDR, srcCursor);
		xntHdr = (XNTHeader) srcCursor;

		if (xntHdr->paramCount > 0)
		{
			srcCursor += sizeof(XNTHeaderData);
			paramNames = getXPathParameterArray(&srcCursor, xntHdr->paramCount);
		}
	}

	resultTmp = NULL;
	resultPos = 0;
	xmlnodeDumpNode(data, rootNdOff, &resultTmp, &resultPos, paramNames);

	result = (char *) palloc(declSize + resultPos + 1);
	if (declSize > 0)
	{
		memcpy(result, declStr, declSize);
		pfree(declStr);
	}
	resultPos = declSize;
	resultTmp = result + resultPos;
	xmlnodeDumpNode(data, rootNdOff, &resultTmp, &resultPos, paramNames);
	if (paramNames != NULL)
	{
		pfree(paramNames);
	}
	result[resultPos] = '\0';
	return result;
}

/*
 * Write 'node' and it's descendants to storage, starting at '*output'.
 * When finished, '*output' points to the first byte following the storage
 * and 'root' contains offset of node/subtree root, counting from the
 * original value of '*output'.
 *
 * If used to write document (XMLNODE_DOC), the caller is responsible
 * for adding XML declaration to the root node.
 */
void
writeXMLNodeInternal(XNodeInternal node, char **output, XMLNodeOffset *root)
{
	XMLNodeOffset *offs = NULL;
	char	   *start = *output;
	unsigned short i;
	unsigned int childCount = 0;
	XNodeListItem *childItem = NULL;
	XNodeInternal *childrenTmp = NULL;
	bool		mustReorder = false;
	bool		hasNonAttr = false;
	XMLNodeKind nodeKind = node->node->kind;

	if ((nodeKind == XMLNODE_DOC || nodeKind == XMLNODE_ELEMENT || nodeKind == XNTNODE_TEMPLATE) &&
		node->children.content != NULL)
	{
		childCount = node->children.position;
	}

	if (childCount > 0)
	{
		Assert(nodeKind == XMLNODE_ELEMENT || nodeKind == XMLNODE_DOC ||
			   nodeKind == XNTNODE_TEMPLATE);

		offs = (XMLNodeOffset *) palloc(childCount * sizeof(XMLNodeOffset));

		if (nodeKind == XMLNODE_ELEMENT)
		{
			childrenTmp = (XNodeInternal *) palloc(childCount * sizeof(XNodeInternal));
			childItem = (XNodeListItem *) (&node->children)->content;

			for (i = 0; i < childCount; i++)
			{
				XNodeInternal child = (XNodeInternal) childItem->value.singlePtr;

				if (child->node->kind != XMLNODE_ATTRIBUTE)
				{
					hasNonAttr = true;
				}

				if (!mustReorder && hasNonAttr && child->node->kind == XMLNODE_ATTRIBUTE)
				{
					mustReorder = true;
				}

				childrenTmp[i] = child;
				childItem++;
			}
		}
	}

	if (mustReorder)
	{
		unsigned short i,
					nonAttr;

		/*
		 * Ensure that attributes are at the lowest positions in the reference
		 * list.
		 */

		/* Start with finding the first non-attribute node. */
		for (i = 0; i < childCount; i++)
		{
			if (childrenTmp[i]->node->kind != XMLNODE_ATTRIBUTE)
			{
				break;
			}
		}
		nonAttr = i++;

		/*
		 * If attribute is found anywhere after that, move it to position
		 * immediately following the last correctly located (i.e. near the
		 * reference array beginning) attribute.
		 */
		for (; i < childCount; i++)
		{
			if (childrenTmp[i]->node->kind == XMLNODE_ATTRIBUTE)
			{
				XNodeInternal attr;
				XNodeInternal *src,
						   *dst;
				unsigned int size = (i - nonAttr) * sizeof(XNodeInternal);

				attr = childrenTmp[i];
				src = childrenTmp + nonAttr;
				dst = src + 1;
				memmove(dst, src, size);
				childrenTmp[nonAttr] = attr;
				nonAttr++;
			}
		}
	}

	if (nodeKind != XMLNODE_ELEMENT)
	{
		/*
		 * 'childrenTmp' is not initialized, we're iterating the children
		 * first time.
		 */
		childItem = (XNodeListItem *) (&node->children)->content;
	}

	/* Make sure that attributes are unique. */
	if (nodeKind == XMLNODE_ELEMENT && childCount > 0)
	{
		unsigned int attrCount = 0;
		char	  **attrNames = (char **) palloc(childCount * sizeof(char *));

		for (i = 0; i < childCount; i++)
		{
			if (childrenTmp[i]->node->kind == XMLNODE_ATTRIBUTE)
			{
				XMLNodeHdr	attrNode = childrenTmp[i]->node;

				attrNames[attrCount++] = XNODE_CONTENT(attrNode);
			}
			else
			{
				break;
			}
		}

		if (attrCount > 1)
		{
			qsort(attrNames, attrCount, sizeof(char *), attrNameComparator);

			for (i = 0; i < (attrCount - 1); i++)
			{
				if (strcmp(attrNames[i], attrNames[i + 1]) == 0)
				{
					elog(ERROR, "attribute name '%s' is not unique", attrNames[i]);
				}
			}
		}
		pfree(attrNames);
	}

	for (i = 0; i < childCount; i++)
	{
		XNodeInternal child;
		XMLNodeOffset root = 0;

		if (childrenTmp != NULL)
		{
			child = childrenTmp[i];
		}
		else
		{
			child = (XNodeInternal) childItem->value.singlePtr;
			childItem++;
		}

		offs[i] = *output - start;
		writeXMLNodeInternal(child, output, &root);
		offs[i] += root;
	}

	if (childrenTmp != NULL)
	{
		pfree(childrenTmp);
	}

	/*
	 * For element, 'children.content' has to be checked instead of
	 * 'childCount'. 'childCount' might have become zero due to node removal.
	 * Even in such a case we still need to construct the new node out of
	 * (possibly missing) children, instead of copying the original subtree.
	 */
	if ((nodeKind == XMLNODE_ELEMENT && node->children.content != NULL) || nodeKind == XNTNODE_TEMPLATE ||
		nodeKind == XMLNODE_DOC)
	{
		XMLCompNodeHdr compNode;
		char	   *name;

		*output = (char *) TYPEALIGN(XNODE_ALIGNOF_COMPNODE, *output);
		/* The header */
		compNode = (XMLCompNodeHdr) *output;
		compNode->common.flags = 0;

		switch (nodeKind)
		{
			case XMLNODE_ELEMENT:
				compNode->common.kind = XMLNODE_ELEMENT;
				if (!hasNonAttr)
				{
					compNode->common.flags |= XNODE_EMPTY;
				}
				break;

			case XNTNODE_TEMPLATE:
				compNode->common.kind = XMLNODE_DOC_FRAGMENT;
				break;

			default:
				compNode->common.kind = nodeKind;
				break;

		}

		compNode->children = childCount;
		*output += sizeof(XMLCompNodeHdrData);

		/* References */
		if (childCount > 0)
		{
			XMLNodeOffset relOffMax = (char *) compNode - (start + offs[0]);
			char		bwidth = getXMLNodeOffsetByteWidth(relOffMax);

			for (i = 0; i < childCount; i++)
			{
				XMLNodeOffset offRel = (char *) compNode - (start + offs[i]);

				writeXMLNodeOffset(offRel, output, bwidth, true);
			}
			XNODE_SET_REF_BWIDTH(compNode, bwidth);
		}

		if (nodeKind == XMLNODE_ELEMENT)
		{
			XMLCompNodeHdr elNode = (XMLCompNodeHdr) node->node;

			/* Element name */
			name = XNODE_ELEMENT_NAME(elNode);
			strcpy(*output, name);
			*output += strlen(name) + 1;
		}

		*root = (char *) compNode - start;
	}
	else
	{
		XMLNodeOffset nodeRoot;
		char	   *copyRootPtr;

		/*
		 * This is either a simple node or element that we didn't have to
		 * deconstruct.
		 */
		copyXMLNode(node->node, *output, false, &nodeRoot);
		copyRootPtr = *output + nodeRoot;
		*root = copyRootPtr - start;
		*output = copyRootPtr + getXMLNodeSize((XMLNodeHdr) copyRootPtr, false);
	}

	if (offs != NULL)
	{
		pfree(offs);
	}
}

/* How many bytes do we need to store the offset? */
char
getXMLNodeOffsetByteWidth(XMLNodeOffset o)
{
	char		i = 1;

	while (o > 0xFF)
	{
		o >>= 8;
		i++;
	};
	return i;
}

/*
 * TODO xmlnode_send(), xmlnode_receive() functions.
 */



PG_FUNCTION_INFO_V1(xmlnode_children);

Datum
xmlnode_children(PG_FUNCTION_ARGS)
{
	xmlnode		nodeRaw = (xmlnode) PG_GETARG_VARLENA_P(0);
	char	   *data = (char *) VARDATA(nodeRaw);
	XMLNodeOffset rootNdOff = XNODE_ROOT_OFFSET(nodeRaw);
	XMLNodeHdr	node = (XMLNodeHdr) (data + rootNdOff);
	TypeInfo	nodeType;
	ArrayType  *result;

	/*
	 * Unfortunately the type info has to be retrieved every time again. If
	 * the backend used global variable to remember the values, all backends
	 * would have to invalidate the values whenever the extension gets dropped
	 * / created.
	 */

	Assert(fcinfo->flinfo != NULL);

	initXNodeTypeInfo(fcinfo->flinfo->fn_oid, 0, &nodeType);

	if (node->kind == XMLNODE_DOC || node->kind == XMLNODE_ELEMENT || node->kind == XMLNODE_DOC_FRAGMENT)
	{
		XMLCompNodeHdr root = (XMLCompNodeHdr) node;
		unsigned short children = root->children;
		Datum	   *elems;
		unsigned short i = 0;
		XMLNodeIteratorData iterator;
		XMLNodeHdr	childNode;

		if (children == 0)
		{
			result = construct_empty_array(nodeType.oid);
			PG_RETURN_POINTER(result);
		}

		elems = (Datum *) palloc(children * sizeof(Datum));

		initXMLNodeIterator(&iterator, root, true);
		while ((childNode = getNextXMLNodeChild(&iterator)) != NULL)
		{
			char	   *childNodeCopy = copyXMLNode(childNode, NULL, true, NULL);

			elems[i++] = PointerGetDatum(childNodeCopy);
		}

		result = construct_array(elems, children, nodeType.oid, nodeType.elmlen, nodeType.elmbyval,
								 nodeType.elmalign);
		PG_RETURN_POINTER(result);
	}
	else
	{
		result = construct_empty_array(nodeType.oid);
		PG_RETURN_POINTER(result);
	}
}


PG_FUNCTION_INFO_V1(xmlelement);

Datum
xmlelement(PG_FUNCTION_ARGS)
{
	Datum		nameText;
	ArrayType  *attrs = NULL;
	char	   *elName;
	unsigned int nameLen,
				resSizeMax;
	unsigned int childSize = 0;
	char	   *result,
			   *resData,
			   *resCursor,
			   *nameDst;
	XMLCompNodeHdr element;
	XMLNodeOffset *rootOffPtr;
	char	  **attrNames = NULL;
	char	  **attrValues = NULL;
	char	   *attrValFlags = NULL;
	XMLNodeHdr *attrNodes = NULL;
	XMLNodeHdr	child = NULL;
	char	  **newNds = NULL;
	char	   *newNd = NULL;
	unsigned int attrCount = 0;
	unsigned int attrsSizeTotal = 0;
	unsigned short childCount = 0;
	XMLNodeParserStateData namePState;
	unsigned int firstColPos = 0;

	if (PG_ARGISNULL(0))
	{
		elog(ERROR, "invalid element name");
	}
	nameText = PG_GETARG_DATUM(0);
	elName = TextDatumGetCString(nameText);

	nameLen = strlen(elName);
	if (nameLen == 0)
	{
		elog(ERROR, "invalid element name");
	}

	if (!PG_ARGISNULL(1))
	{
		int		   *dims;
		Oid			elType,
					arrType;
		int16		arrLen,
					elLen;
		bool		elByVal,
					elIsNull;
		char		elAlign;
		unsigned int i;

		attrs = PG_GETARG_ARRAYTYPE_P(1);
		if (ARR_NDIM(attrs) != 2)
		{
			elog(ERROR, "attributes must be passed in 2 dimensional array");
		}
		dims = ARR_DIMS(attrs);
		if (dims[1] != 2)
		{
			elog(ERROR, "the second dimension of attribute array must be 2");
		}

		attrCount = dims[0];
		Assert(attrCount > 0);

		elType = attrs->elemtype;
		arrType = get_array_type(elType);
		Assert(arrType != InvalidOid);
		arrLen = get_typlen(arrType);
		get_typlenbyvalalign(elType, &elLen, &elByVal, &elAlign);
		attrNames = (char **) palloc(attrCount * sizeof(char *));
		attrValues = (char **) palloc(attrCount * sizeof(char *));
		attrValFlags = (bool *) palloc(attrCount * sizeof(char));

		for (i = 1; i <= attrCount; i++)
		{
			int			subscrName[] = {i, 1};
			int			subscrValue[] = {i, 2};
			Datum		elDatum;
			char	   *nameStr,
					   *valueStr;
			bool		valueHasRefs = false;

			elDatum = array_ref(attrs, 2, subscrName, arrLen, elLen, elByVal, elAlign, &elIsNull);
			if (elIsNull)
			{
				elog(ERROR, "attribute name must not be null");
			}
			nameStr = text_to_cstring(DatumGetTextP(elDatum));

			initXMLParserState(&namePState, nameStr, XMLNODE_ELEMENT, NULL);
			readXMLName(&namePState, false, true, true, &firstColPos);

			/*
			 * No need to finalize the state when XMLNODE_ELEMENT is the
			 * target type.
			 */

			if (*namePState.c != '\0')
			{
				elog(ERROR, "unexpected char in attribute name: '%c'", *namePState.c);
			}
			if (firstColPos == 0)
			{
				if (*nameStr == XNODE_CHAR_COLON && strlen(nameStr) > 1)
				{				/* Igonre the leading colon. */
					char	   *tmp = nameStr;

					nameStr = (char *) palloc(strlen(nameStr));
					strcpy(nameStr, tmp + 1);
					pfree(tmp);
				}
			}


			/* Check uniqueness of the attribute name. */
			if (i > 1)
			{
				unsigned short j;

				for (j = 0; j < (i - 1); j++)
				{
					if (strcmp(nameStr, attrNames[j]) == 0)
					{
						elog(ERROR, "attribute name '%s' is not unique", nameStr);
					}
				}
			}

			elDatum = array_ref(attrs, 2, subscrValue, arrLen, elLen, elByVal, elAlign, &elIsNull);
			if (elIsNull)
			{
				elog(ERROR, "attribute value must not be null");
			}
			valueStr = text_to_cstring(DatumGetTextP(elDatum));

			attrValFlags[i - 1] = 0;

			if (strlen(valueStr) > 0)
			{
				XMLNodeParserStateData state;
				char	   *valueStrOrig = valueStr;

				/* Parse the value and check validity. */
				initXMLParserState(&state, valueStr, XMLNODE_ATTRIBUTE, NULL);
				valueStr = readXMLAttValue(&state, true, &valueHasRefs);

				/*
				 * If the value contains quotation mark, then apostrophe is
				 * the delimiter.
				 */
				if (strchr(valueStr, XNODE_CHAR_QUOTMARK) != NULL)
				{
					attrValFlags[i - 1] |= XNODE_ATTR_APOSTROPHE;
				}
				finalizeXMLParserState(&state);
				pfree(valueStrOrig);
			}

			attrNames[i - 1] = nameStr;
			attrValues[i - 1] = valueStr;
			if (valueHasRefs)
			{
				attrValFlags[i - 1] |= XNODE_ATTR_CONTAINS_REF;
			}
			attrsSizeTotal += sizeof(XMLNodeHdrData) + strlen(nameStr) +strlen(valueStr) + 2;
		}
	}

	if (!PG_ARGISNULL(2))
	{
		Datum		childNodeDatum = PG_GETARG_DATUM(2);
		xmlnode		childRaw = (xmlnode) PG_DETOAST_DATUM(childNodeDatum);

		child = XNODE_ROOT(childRaw);
		if (child->kind == XMLNODE_DOC_FRAGMENT)
		{
			childSize = getXMLNodeSize(child, true) - getXMLNodeSize(child, false);
		}
		else
		{
			childSize = getXMLNodeSize(child, true);
		}
	}

	/*
	 * Make sure the element name is valid.
	 *
	 * If initialized for XMLNODE_ELEMENT, the parser state has no memory
	 * allocated so we don't have to call finalizeXMLParserState().
	 */
	initXMLParserState(&namePState, elName, XMLNODE_ELEMENT, NULL);
	readXMLName(&namePState, false, true, true, &firstColPos);

	if (*namePState.c != '\0')
	{
		elog(ERROR, "unexpected char in element name: '%c'", *namePState.c);
	}
	if (firstColPos == 0)
	{
		if (*elName == XNODE_CHAR_COLON && strlen(elName) > 1)
		{
			elName++;
		}
	}

	if (child != NULL)
	{
		if (child->kind == XMLNODE_DOC_FRAGMENT)
		{
			childCount = ((XMLCompNodeHdr) child)->children;
		}
		else
		{
			childCount = 1;
		}
	}

	/*
	 * It's hard to determine the byte width of references until the copying
	 * has finished. Therefore we assume the worst case: 4 bytes per
	 * reference.
	 */
	resSizeMax = VARHDRSZ + attrsSizeTotal + childSize + (attrCount + childCount) * 4 +
		sizeof(XMLCompNodeHdrData) + nameLen + 1 + sizeof(XMLNodeOffset);

	/*
	 * As for padding, the worst case is that the child node is a document
	 * fragment only containing compound nodes. In addition, the new element
	 * node needs to be taken into account, as well as the root node offset.
	 * On the other hand no padding is required for attributes.
	 */
	resSizeMax += MAX_PADDING(XNODE_ALIGNOF_COMPNODE) * (childCount + 1) +
		MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET);

	result = (char *) palloc(resSizeMax);
	resCursor = resData = VARDATA(result);

	if (attrCount > 0)
	{							/* Copy attributes. */
		unsigned short i;

		Assert(attrNames != NULL && attrValues != NULL && attrValFlags != NULL);

		attrNodes = (XMLNodeHdr *) palloc(attrCount * sizeof(XMLNodeHdr));
		for (i = 0; i < attrCount; i++)
		{
			XMLNodeHdr	attrNode = (XMLNodeHdr) resCursor;
			char	   *name = attrNames[i];
			char	   *value = attrValues[i];

			attrNodes[i] = attrNode;
			attrNode->kind = XMLNODE_ATTRIBUTE;
			attrNode->flags = attrValFlags[i];

			if (xmlAttrValueIsNumber(value))
			{
				attrNode->flags |= XNODE_ATTR_NUMBER;
			}

			resCursor = XNODE_CONTENT(attrNode);
			strcpy(resCursor, name);
			resCursor += strlen(name) + 1;
			pfree(name);

			strcpy(resCursor, value);
			resCursor += strlen(value) + 1;
			pfree(value);
		}
		pfree(attrNames);
		pfree(attrValues);
		pfree(attrValFlags);
	}

	if (child != NULL)
	{
		XMLNodeKind k = child->kind;

		/*
		 * Check if the node to be inserted is of a valid kind. If the node is
		 * document fragment, its assumed that invalid node kinds are never
		 * added. Otherwise we'd have to check the node fragment (recursively)
		 * not only here.
		 */
		if (k != XMLNODE_DOC_FRAGMENT)
		{
			if (k == XMLNODE_DOC || k == XMLNODE_DTD || k == XMLNODE_ATTRIBUTE)
			{
				elog(ERROR, "the nested node must not be %s", getXMLNodeKindStr(k));
			}
		}
		copyXMLNodeOrDocFragment(child, &resCursor, &newNd, &newNds);
	}

	resCursor = (char *) TYPEALIGN(XNODE_ALIGNOF_COMPNODE, resCursor);
	element = (XMLCompNodeHdr) resCursor;
	element->common.kind = XMLNODE_ELEMENT;
	element->common.flags = (child == NULL) ? XNODE_EMPTY : 0;
	element->children = attrCount + childCount;

	if (childCount > 0 || attrCount > 0)
	{
		XMLNodeOffset childOff,
					childOffMax;
		char		bwidth;
		char	   *refPtr;

		/* Save relative offset(s) of the child node(s). */

		if (attrCount > 0)
		{
			childOffMax = (char *) element - resData;
		}
		else if (childCount > 0)
		{
			if (child->kind == XMLNODE_DOC_FRAGMENT)
			{
				Assert(newNds != NULL);
				childOffMax = (char *) element - newNds[0];
			}
			else
			{
				childOffMax = (char *) element - newNd;
			}
		}
		else
		{
			childOffMax = 0;
		}
		bwidth = getXMLNodeOffsetByteWidth(childOffMax);
		XNODE_SET_REF_BWIDTH(element, bwidth);

		refPtr = XNODE_FIRST_REF(element);

		if (attrCount > 0)
		{
			unsigned short i;

			/* The attribute references first... */
			for (i = 0; i < attrCount; i++)
			{
				XMLNodeHdr	node = attrNodes[i];

				childOff = (char *) element - (char *) node;
				writeXMLNodeOffset(childOff, &refPtr, bwidth, true);
			}
			pfree(attrNodes);
		}


		if (childCount > 0)
		{
			/* ...followed by those of the other children. */
			if (child->kind == XMLNODE_DOC_FRAGMENT)
			{
				unsigned short i;

				for (i = 0; i < childCount; i++)
				{
					childOff = (char *) element - newNds[i];
					writeXMLNodeOffset(childOff, &refPtr, bwidth, true);
				}
				pfree(newNds);
			}
			else
			{
				childOff = (char *) element - newNd;
				writeXMLNodeOffset(childOff, &refPtr, bwidth, true);
			}
		}
	}

	/* And finally set the element name. */
	nameDst = XNODE_ELEMENT_NAME(element);
	strcpy(nameDst, elName);
	resCursor = nameDst + strlen(elName) + 1;

	resCursor = (char *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, resCursor);
	SET_VARSIZE(result, (char *) resCursor - result + sizeof(XMLNodeOffset));
	rootOffPtr = XNODE_ROOT_OFFSET_PTR(result);
	*rootOffPtr = (char *) element - resData;
	PG_RETURN_POINTER(result);
}

/*
 * Collect namespace declarations that the node contains.
 *
 * If 'declsOnly' is true, then only declarations are collected. Otherwise 'attrsPrefixed'
 * receives array containing all prefixed attributes of 'currentNode' (except for the
 * declarations) and 'attrsPrefixedCount' receives number of such attributes.
 */
void
collectXMLNamespaceDeclarations(XMLCompNodeHdr currentNode, unsigned int *attrCount, unsigned int *nmspDeclCount,
								XMLNodeContainer declarations, bool declsOnly, XMLNodeHdr **attrsPrefixed, unsigned int *attrsPrefixedCount)
{

	unsigned int defNmspLen = strlen(XNODE_NAMESPACE_DEF_PREFIX);
	XMLNodeIteratorData iterator;
	XMLNodeHdr	childNode;

	if (!declsOnly)
	{
		*attrCount = 0;
		*attrsPrefixedCount = 0;
	}

	initXMLNodeIterator(&iterator, currentNode, true);

	while ((childNode = getNextXMLNodeChild(&iterator)) != NULL)
	{

		if (childNode->kind != XMLNODE_ATTRIBUTE)
		{
			break;
		}

		/*
		 * The sizes might be exaggerated sometimes but the exact number of
		 * attribute names is not at hand now.
		 */
		if (!declsOnly && *attrCount == 0)
		{
			*attrsPrefixed = (XMLNodeHdr *) palloc(currentNode->children * sizeof(XMLNodeHdr));
		}

		if (!declsOnly)
		{
			(*attrCount)++;
		}

		if (childNode->flags & XNODE_NMSP_PREFIX)
		{
			char	   *attrName = XNODE_CONTENT(childNode);

			if ((strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX, defNmspLen) == 0) &&
				attrName[defNmspLen] == XNODE_CHAR_COLON)
			{

				/* Namespace declaration. */

				xmlnodePushSinglePtr(declarations, (void *) childNode);
				if (nmspDeclCount != NULL)
				{
					(*nmspDeclCount)++;
				}
			}
			else
			{
				/* Namespace usage. */
				if (!declsOnly)
				{
					(*attrsPrefixed)[(*attrsPrefixedCount)++] = childNode;
				}
			}
		}
	}
}


PG_FUNCTION_INFO_V1(xmlfragment);

/*
 * Aggregate function to turn group of nodes into a single document fragment (node).
 *
 * Instead of constructing a new document fragment per each state transition it might be
 * better to collect pointers to the group members and only create one fragment when
 * at the end of the group. However there seems to be no appropriate type for
 * the state.
 */
Datum
xmlfragment(PG_FUNCTION_ARGS)
{
	xmlnode		nodeVar,
				newNodeVar;
	unsigned short nodeCount,
				nodeCountNew;
	unsigned int nodeCountTotal;
	char	   *resData,
			   *resCursor,
			   *result = NULL;
	unsigned int nodeSize,
				newNodeSize;
	unsigned int resultSize = 0;
	XMLNodeHdr	newNode,
				node = NULL;
	XMLNodeOffset *rootsAbs;
	XMLNodeOffset rootOff,
				offRelMax,
				root = 0;
	XMLCompNodeHdr resFragment;
	XMLNodeOffset *rootOffPtr;
	unsigned int sizeFinal;
	unsigned short i;
	char	  **roots;
	char		bwidth;

	nodeVar = PG_GETARG_VARLENA_P(0);
	newNodeVar = PG_GETARG_VARLENA_P(1);
	node = XNODE_ROOT(nodeVar);
	newNode = XNODE_ROOT(newNodeVar);
	nodeCount = 0;

	if (newNode->kind == XMLNODE_DOC || newNode->kind >= XNTNODE_ROOT)
	{
		elog(ERROR, "node of kind %u can't be concatenated to anything", newNode->kind);
	}

	/* Evaluate the 'state node'... */
	getNodeInfo(node, &nodeSize, &nodeCount);
	resultSize += nodeSize;

	/* ... as well as the new one. */
	getNodeInfo(newNode, &newNodeSize, &nodeCountNew);
	resultSize += newNodeSize;

	nodeCountTotal = nodeCount + nodeCountNew;

	Assert(nodeCountTotal > 1);
	if (nodeCountTotal > XMLNODE_MAX_CHILDREN)
	{
		elog(ERROR, "maximum number of child nodes is %u", XMLNODE_MAX_CHILDREN);
	}

	/*
	 * getXMLNodeSize() does account for references, but not always the
	 * maximum byte width. Let's assume the worst: all references are going to
	 * grow from 1 to 4 bytes.
	 */
	resultSize += nodeCountTotal * (sizeof(XMLNodeOffset) - 1);
	resultSize += VARHDRSZ + sizeof(XMLCompNodeHdrData) + sizeof(XMLNodeOffset);

	/*
	 * As for padding, the worst case is that the original node, the new one
	 * and also the new root need some. And the root offset too.
	 */
	resultSize += 3 * MAX_PADDING(XNODE_ALIGNOF_COMPNODE) + MAX_PADDING(XNODE_ALIGNOF_NODE_OFFSET);

	result = (char *) palloc(resultSize);
	resData = resCursor = VARDATA(result);

	rootsAbs = (XMLNodeOffset *) palloc(nodeCountTotal * sizeof(XMLNodeOffset));

	/*
	 * Copy the existing node.
	 *
	 * copyXMLNodeOrDocFragment() is not easy to use here because the existing
	 * (status) node and the new one can each be either a single node or
	 * fragment. The combinations of both would make initialization of
	 * 'rootsAbs' uneasy.
	 */
	if (nodeCount == 1)
	{
		copyXMLNode(node, resCursor, false, &root);
		resCursor += nodeSize;
		rootsAbs[0] = root;
	}
	else
	{
		roots = copyXMLDocFragment((XMLCompNodeHdr) node, &resCursor);
		for (i = 0; i < nodeCount; i++)
		{
			XMLNodeOffset rootAbs;

			rootAbs = roots[i] - resData;
			rootsAbs[i] = rootAbs;
		}
		pfree(roots);
	}

	/* And append the new one(s). */
	if (nodeCountNew == 1)
	{
		copyXMLNode(newNode, resCursor, false, &root);
		resCursor += root;
		rootsAbs[nodeCount] = resCursor - resData;

		/*
		 * Move the cursor to address immediately following the copied new
		 * node.
		 */
		resCursor += getXMLNodeSize(newNode, false);
	}
	else
	{
		XMLNodeOffset *dst;

		roots = copyXMLDocFragment((XMLCompNodeHdr) newNode, &resCursor);

		dst = rootsAbs + nodeCount;
		for (i = 0; i < nodeCountNew; i++)
		{
			dst[i] = roots[i] - resData;
		}
		pfree(roots);
	}

	/*
	 * At least 2 nodes must exist in the result now, so the resulting node
	 * must be a document fragment.
	 */
	resCursor = (char *) TYPEALIGN(XNODE_ALIGNOF_COMPNODE, resCursor);
	resFragment = (XMLCompNodeHdr) resCursor;
	resFragment->common.kind = XMLNODE_DOC_FRAGMENT;
	resFragment->common.flags = 0;
	resFragment->children = nodeCountTotal;

	offRelMax = resCursor - (resData + rootsAbs[0]);
	bwidth = getXMLNodeOffsetByteWidth(offRelMax);
	XNODE_SET_REF_BWIDTH(resFragment, bwidth);

	resCursor += sizeof(XMLCompNodeHdrData);

	for (i = 0; i < nodeCountTotal; i++)
	{
		XMLNodeOffset offRel;
		XMLNodeHdr	resFragChild;

		/*
		 * There should be no way for such a fragment to arise but the check
		 * doesn't hurt.
		 */
		resFragChild = (XMLNodeHdr) (resData + rootsAbs[i]);
		if (resFragChild->kind == XMLNODE_ATTRIBUTE)
		{
			elog(ERROR, "resulting document fragment must not contain attribute nodes");
		}
		offRel = (char *) resFragment - (char *) resFragChild;
		writeXMLNodeOffset(offRel, &resCursor, bwidth, true);
	}

	resCursor = (char *) TYPEALIGN(XNODE_ALIGNOF_NODE_OFFSET, resCursor);
	rootOff = (char *) resFragment - resData;
	rootOffPtr = (XMLNodeOffset *) resCursor;
	*rootOffPtr = rootOff;
	sizeFinal = resCursor - result + sizeof(XMLNodeOffset);
	SET_VARSIZE(result, sizeFinal);
	PG_RETURN_POINTER(result);
}

static void
getNodeInfo(XMLNodeHdr node, unsigned int *size, unsigned short *count)
{
	/* Evaluate the 'state node'... */
	*size = getXMLNodeSize(node, true);

	if (node->kind == XMLNODE_DOC_FRAGMENT)
	{
		XMLCompNodeHdr fragment = (XMLCompNodeHdr) node;

		*count = fragment->children;
		Assert(*count > 1);

		/*
		 * The fragment node (header) itself won't be copied. Possible padding
		 * of the fragment (root) is not subtracted, even though it may
		 * disappear in the resulting tree. This can save mere 3 bytes in
		 * extreme case, so the effort does not seem to be adequate.
		 */
		*size -= getXMLNodeSize(node, false);
	}
	else
	{
		*count = 1;
	}
}

static int
attrNameComparator(const void *left, const void *right)
{
	return strcmp((char *) left, (char *) right);
}
