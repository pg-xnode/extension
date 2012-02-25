/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"
#include "funcapi.h"
#include "mb/pg_wchar.h"
#include "utils/builtins.h"
#include "utils/palloc.h"

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
	initXMLParser(&parserState, input);
	xmlnodeParseNode(&parserState);
	finalizeXMLParser(&parserState);
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
	initXMLParser(&parserState, input);
	xmlnodeParseDoc(&parserState);
	finalizeXMLParser(&parserState);
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
/*
 * TODO Operators
 */
