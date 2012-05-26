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
	initXMLParserState(&parserState, input, XMLNODE_NODE);
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
	initXMLParserState(&parserState, input, XMLNODE_DOC);
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
	unsigned int sizeNew,
				dataSizeNew;
	xmlnode		node = (xmlnode) PG_GETARG_VARLENA_P(0);
	xmldoc		document = NULL;
	char	   *docData;
	unsigned int sizeOrig = VARSIZE(node);
	unsigned int dataSizeOrig = sizeOrig - VARHDRSZ;
	char	   *nodeData = (char *) VARDATA(node);

	/*
	 * The new root will start where last value of the array (i.e. offset of
	 * the current root) was so far
	 */
	XMLNodeOffset rootOffsetNew = dataSizeOrig - sizeof(XMLNodeOffset);

	/* Find that 'old last (root offset) value' ... */
	XMLNodeOffset *rootOffPtrOrig = (XMLNodeOffset *) (nodeData + rootOffsetNew);

	/* ... and read it */
	XMLNodeOffset rootOffsetOrig = *rootOffPtrOrig;

	/*
	 * Compute 'relative reference' of the 'old root' that the document ('new
	 * root') will remember
	 */
	XMLNodeOffset dist = rootOffsetNew - rootOffsetOrig;
	XMLNodeOffset *rootOffPtrNew;

	char		bwidth = getXMLNodeOffsetByteWidth(dist);

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

		sizeNew = sizeOrig + sizeof(XMLCompNodeHdrData) + bwidth;
		dataSizeNew = sizeNew - VARHDRSZ;
		document = (xmldoc) palloc(sizeNew);
		docData = (char *) VARDATA(document);
		memcpy(docData, nodeData, rootOffsetNew);
		rootDoc = (XMLCompNodeHdr) (docData + rootOffsetNew);
		rootDoc->common.kind = XMLNODE_DOC;
		rootDoc->common.flags = 0;
		XNODE_SET_REF_BWIDTH(rootDoc, bwidth);
		rootDoc->children = 1;
		refTargPtr = (char *) rootDoc + sizeof(XMLCompNodeHdrData);
		writeXMLNodeOffset(dist, &refTargPtr, bwidth, false);
		rootOffPtrNew = (XMLNodeOffset *) (docData + dataSizeNew - sizeof(XMLNodeOffset));
		*rootOffPtrNew = rootOffsetNew;
		SET_VARSIZE(document, sizeNew);
	}
	else if (rootNode->common.kind == XMLNODE_DOC_FRAGMENT)
	{
		checkXMLWellFormedness(rootNode);
		document = (xmldoc) palloc(sizeOrig);
		docData = (char *) VARDATA(document);
		memcpy(document, node, sizeOrig);
		rootDoc = (XMLCompNodeHdr) (docData + rootOffsetOrig);
		rootDoc->common.kind = XMLNODE_DOC;
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
	XMLNodeOffset rootOff,
				rootOffNew;
	XMLNodeOffset *rootOffPtrNew;

	XMLCompNodeHdr root = (XMLCompNodeHdr) XNODE_ROOT(doc);

	Assert(root->common.kind == XMLNODE_DOC);

	rootOff = (char *) root - docData;

	if (root->children == 1)
	{
		/* The single child (i.e. root element) will be the result of the cast */
		char	   *refPtr = XNODE_FIRST_REF(root);
		unsigned char bwidth = XNODE_GET_REF_BWIDTH(root);
		XMLNodeOffset childOff = rootOff - readXMLNodeOffset(&refPtr, bwidth, false);
		XMLNodeHdr	child = (XMLNodeHdr) (docData + childOff);

		node = (xmlnode) copyXMLNode(child, NULL, true, &rootOffNew);
		rootOffPtrNew = XNODE_ROOT_OFFSET_PTR(node);
		*rootOffPtrNew = rootOffNew;
	}
	else
	{
		/* Just change type to document fragment and ignore head if one exists */
		char	   *nodeData;
		XMLCompNodeHdr rootNew;

		node = (xmlnode) copyXMLNode((XMLNodeHdr) root, NULL, true, &rootOffNew);
		nodeData = VARDATA(node);
		rootNew = (XMLCompNodeHdr) (nodeData + rootOffNew);
		rootNew->common.kind = XMLNODE_DOC_FRAGMENT;
		rootNew->common.flags = 0;

		/*
		 * The root offset will be stored right after the document fragment
		 * header. If we used XNODE_ROOT_OFFSET_PTR() at this place, it could
		 * be wrong because original document (that we have just coppied)
		 * could have contained XMLDeclData.
		 */
		rootOffPtrNew = (XMLNodeOffset *) XNODE_ELEMENT_NAME(rootNew);
		*rootOffPtrNew = rootOffNew;
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

	resultTmp = NULL;
	resultPos = 0;

	if (root->kind == XMLNODE_DOC_FRAGMENT)
	{
		XMLCompNodeHdr fragment = (XMLCompNodeHdr) root;

		if (checkFragmentForAttributes(fragment))
		{
			elog(ERROR, "document fragment having attributes as direct children can't be dumped.");
		}
	}

	xmlnodeDumpNode(data, rootNdOff, &resultTmp, &resultPos);
	if (root->kind == XMLNODE_DOC && (root->flags & XNODE_DOC_XMLDECL))
	{
		XMLCompNodeHdr doc = (XMLCompNodeHdr) root;
		XMLDecl		decl = (XMLDecl) XNODE_ELEMENT_NAME(doc);

		declStr = dumpXMLDecl(decl);
		declSize = strlen(declStr);
	}
	result = (char *) palloc(declSize + resultPos + 1);
	if (declSize > 0)
	{
		memcpy(result, declStr, declSize);
		pfree(declStr);
	}
	resultPos = declSize;
	resultTmp = result + resultPos;
	xmlnodeDumpNode(data, rootNdOff, &resultTmp, &resultPos);
	result[resultPos] = '\0';
	return result;
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
		char	   *childOffPtr;
		unsigned short i;

		if (children == 0)
		{
			result = construct_empty_array(nodeType.oid);
			PG_RETURN_POINTER(result);
		}

		elems = (Datum *) palloc(children * sizeof(Datum));

		childOffPtr = XNODE_FIRST_REF(root);
		for (i = 0; i < children; i++)
		{
			XMLNodeOffset childOff = readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH(root), true);
			XMLNodeHdr	childNode = (XMLNodeHdr) (data + rootNdOff - childOff);
			char	   *childNodeCopy = copyXMLNode(childNode, NULL, true, NULL);

			elems[i] = PointerGetDatum(childNodeCopy);
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
		arrLen = get_typlen(arrType);
		Assert(arrType != InvalidOid);
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

			initXMLParserState(&namePState, nameStr, XMLNODE_ELEMENT);
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
				initXMLParserState(&state, valueStr, XMLNODE_ATTRIBUTE);
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
	initXMLParserState(&namePState, elName, XMLNODE_ELEMENT);
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
		copyXMLNodeOrDocFragment(child, childSize, &resCursor, &newNd, &newNds);
	}

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
	unsigned int i;
	char	   *childOffPtr = XNODE_FIRST_REF(currentNode);
	char		bwidth = XNODE_GET_REF_BWIDTH(currentNode);

	if (!declsOnly)
	{
		*attrCount = 0;
		*attrsPrefixedCount = 0;
	}

	for (i = 0; i < currentNode->children; i++)
	{
		XMLNodeOffset childOff = readXMLNodeOffset(&childOffPtr, bwidth, true);
		XMLNodeHdr	childNode = (XMLNodeHdr) ((char *) currentNode - childOff);

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
				char	   *nmspName = attrName + defNmspLen + 1;

				xmlnodePushSinglePtr(declarations, (void *) nmspName);
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
