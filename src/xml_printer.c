/*
 * Functions to convert binary XML data to text.
 *
 * Copyright (C) 2012, Antonin Houska
 */

#include "template.h"
#include "xml_parser.h"
#include "xmlnode_util.h"
#include "xnt.h"

static void xmlnodeDumpNode(char *input, char *nmspPrefix, char *nmspURI,
				XMLNodeOffset nodeOff, StringInfo output, char **paramNames,
				bool terminate, GetSpecialXNodNameFunc specNodeName);
static unsigned int dumpAttributes(XMLCompNodeHdr element, StringInfo output,
			   char **paramNames);
static void dumpContentEscaped(XMLNodeKind kind, StringInfo output, char *input, unsigned int inputLen);
static void dumpSpecString(StringInfo output, char *outNew, unsigned int *incrInput);
static char *ensureSpace(StringInfo output, unsigned int toWrite);
static void visitXMLNodeForDump(XMLNodeHdr *stack, unsigned int depth, void *userData);
static char *getValidPrefix(XMLCompNodeHdr element, char *namespaceURI, char *currentPrefix);


struct XMLNodeDumpInfo
{
	StringInfo	output;
	char	   *start;			/* where the tree storage starts */
};

char *
dumpXMLNode(char *data, XMLNodeOffset rootNdOff, unsigned int binarySize,
			char *nmspURI, GetSpecialXNodNameFunc specNodeName)
{
	XMLNodeHdr	root = (XMLNodeHdr) (data + rootNdOff);
	char	   *declStr = NULL;
	unsigned short declSize = 0;
	char	   *srcCursor = NULL;
	char	  **paramNames = NULL;
	StringInfo	output;
	unsigned int outSizeEst;

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
	else if (root->kind == XMLTEMPLATE_ROOT)
	{
		XMLTemplateHeader templHdr;
		XMLCompNodeHdr template = (XMLCompNodeHdr) root;

		srcCursor = XNODE_ELEMENT_NAME(template);
		srcCursor = (char *) TYPEALIGN(XNODE_ALIGNOF_TEMPL_HDR, srcCursor);
		templHdr = (XMLTemplateHeader) srcCursor;

		if (templHdr->paramCount > 0)
		{
			unsigned short i;

			srcCursor += sizeof(XMLTemplateHeaderData);
			paramNames = (char **) palloc(templHdr->paramCount * sizeof(char *));
			for (i = 0; i < templHdr->paramCount; i++)
			{
				paramNames[i] = srcCursor;
				srcCursor += strlen(srcCursor) + 1;
			}
		}
	}

	/* This estimate may need improvement to avoid too frequent reallocations. */
	outSizeEst = (binarySize <= 1024) ? 1024 : binarySize;
	outSizeEst += declSize;

	/*
	 * StringInfoData structure is used here, but we manage reallocations in a
	 * custom (less) aggressive way than the in-core functions. (It's not good
	 * to double the output memory if the document very large.)
	 */
	output = makeStringInfo();
	xnodeInitStringInfo(output, outSizeEst);

	if (declSize > 0)
	{
		memcpy(output->data, declStr, declSize);
		pfree(declStr);
		output->len = declSize;
	}
	xmlnodeDumpNode(data, NULL, nmspURI, rootNdOff, output, paramNames, true,
					specNodeName);

	if (paramNames != NULL)
	{
		pfree(paramNames);
	}
	return output->data;
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

void
dumpXMLNodeDebug(StringInfo output, char *data, XMLNodeOffset off)
{
	XMLNodeHdr	root = (XMLNodeHdr) (data + off);
	struct XMLNodeDumpInfo userData;

	userData.output = output;
	userData.start = data;
	walkThroughXMLTree(root, visitXMLNodeForDump, true, (void *) &userData);
}

/*
 * 'nmspPrefix' - for 'ordinary' XML documents there's no need to distinguish
 * prefix and the name itself. However special nodes (e.g. XNT) are stored as
 * integer (to make processing simpler) and namespace prefix is thrown away
 * as soon as parser sees it's bound to the correct namespace URI. That's
 * why the prefix is treated separate here.
 */
static void
xmlnodeDumpNode(char *input, char *nmspPrefix, char *nmspURI, XMLNodeOffset nodeOff,
				StringInfo output, char **paramNames, bool terminate,
				GetSpecialXNodNameFunc specNodeName)
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

		case XMLTEMPLATE_ROOT:

		case XNTNODE_TEMPLATE:
		case XNTNODE_COPY_OF:
		case XNTNODE_ELEMENT:
		case XNTNODE_ATTRIBUTE:
			if (node->kind == XMLNODE_ELEMENT || (node->flags & XNODE_EL_SPECIAL))
			{
				XMLCompNodeHdr element = (XMLCompNodeHdr) node;

				if (node->flags & XNODE_EL_SPECIAL)
				{
					Assert(nmspURI != NULL);
					nmspPrefix = getValidPrefix(element, nmspURI, nmspPrefix);

					Assert(specNodeName != NULL);
					content = specNodeName(node->kind, nmspPrefix);
				}
				else
				{
					content = XNODE_ELEMENT_NAME(element);
				}
				cntLen = strlen(content);
				cursor = ensureSpace(output, cntLen + 1);

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
					cursor = ensureSpace(output, 2);
					*cursor++ = XNODE_CHAR_SLASH;
					*cursor = XNODE_CHAR_RARROW;
				}
				else
				{
					char	   *lastChild = XNODE_LAST_REF((XMLCompNodeHdr) node);

					cursor = ensureSpace(output, 1);
					*cursor = XNODE_CHAR_RARROW;

					while (childOffPtr <= lastChild)
					{
						xmlnodeDumpNode(input, nmspPrefix, nmspURI,
									nodeOff - readXMLNodeOffset(&childOffPtr,
									 XNODE_GET_REF_BWIDTH(eh), true), output,
										paramNames, false, specNodeName);
					}

					/*
					 * Etag
					 */
					cursor = ensureSpace(output, cntLen + 3);
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
				 * This root node of a document (possibly that of special one,
				 * e.g. template).
				 */
				char	   *lastChild = XNODE_LAST_REF((XMLCompNodeHdr) node);

				while (childOffPtr <= lastChild)
				{
					xmlnodeDumpNode(input, nmspPrefix, nmspURI,
									nodeOff - readXMLNodeOffset(&childOffPtr, XNODE_GET_REF_BWIDTH((XMLCompNodeHdr) node), true),
									output, paramNames, false, specNodeName);
				}
			}
			break;

		case XMLNODE_DTD:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);

			cursor = ensureSpace(output, incr = strlen(specXMLStrings[XNODE_STR_DTD_START]));
			memcpy(cursor, specXMLStrings[XNODE_STR_DTD_START], incr);

			cursor = ensureSpace(output, cntLen);
			memcpy(cursor, content, cntLen);

			cursor = ensureSpace(output, incr = strlen(specXMLStrings[XNODE_STR_DTD_END]));
			memcpy(cursor, specXMLStrings[XNODE_STR_DTD_END], incr);
			break;

		case XMLNODE_ATTRIBUTE:
			content = XNODE_CONTENT(node);

			/* Skip the name. */
			content += strlen(content) + 1;

			cntLen = strlen(content);
			cursor = ensureSpace(output, cntLen);
			memcpy(cursor, content, cntLen);
			break;

		case XMLNODE_COMMENT:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);
			cursor = ensureSpace(output, incr = strlen(specXMLStrings[XNODE_STR_CMT_START]));
			memcpy(cursor, specXMLStrings[XNODE_STR_CMT_START], incr);

			cursor = ensureSpace(output, cntLen);
			memcpy(cursor, content, cntLen);

			cursor = ensureSpace(output, incr = strlen(specXMLStrings[XNODE_STR_CMT_END]));
			memcpy(cursor, specXMLStrings[XNODE_STR_CMT_END], incr);
			break;

		case XMLNODE_PI:
			content = XNODE_CONTENT(node);
			cntLen = strlen(content);
			incr = strlen(specXMLStrings[XNODE_STR_PI_START]);

			cursor = ensureSpace(output, incr);
			memcpy(cursor, specXMLStrings[XNODE_STR_PI_START], incr);

			cursor = ensureSpace(output, cntLen);
			memcpy(cursor, content, cntLen);

			if (node->flags & XNODE_PI_HAS_VALUE)
			{
				content += cntLen + 1;
				cntLen = strlen(content);
				cursor = ensureSpace(output, cntLen + 1);
				*cursor++ = ' ';
				memcpy(cursor, content, cntLen);
			}
			incr = strlen(specXMLStrings[XNODE_STR_PI_END]);
			cursor = ensureSpace(output, incr);
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
				cursor = ensureSpace(output, cntLen);
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

				xmlnodeDumpNode(input, nmspPrefix, nmspURI, nodeOff - offRel, output,
								paramNames, false, specNodeName);
			}
			break;

		default:
			elog(ERROR, "unable to dump node: %u", node->kind);
			break;
	}

	if (terminate)
	{
		cursor = ensureSpace(output, 1);
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
			XNodeSpecAttributes *attrInfo = getXNodeAttrInfo(element->common.kind);

			Assert(attrInfo != NULL);

			if (i < attrInfo->number)
			{
				attrName = attrInfo->names[i];
				isSpecialAttr = true;
			}
		}
		if (!isSpecialAttr)
		{
			attrName = XNODE_CONTENT(attrNode);
		}

		qMark = (attrNode->flags & XNODE_ATTR_APOSTROPHE) ? XNODE_CHAR_APOSTR : XNODE_CHAR_QUOTMARK;

		attrNameLen = strlen(attrName);

		cursor = ensureSpace(output, attrNameLen + 3);
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
				XMLNodeKind kind = element->common.kind;

				if (kind == XNTNODE_COPY_OF && i == XNT_COPY_OF_EXPR)
				{
					XPathHeader xpHdr;
					XPathExpression xpExpr;
					StringInfoData output;

					xpHdr = (XPathHeader) attrValue;
					xpExpr = getXPathExpressionFromStorage(xpHdr);

					xnodeInitStringInfo(&output, 32);
					dumpXPathExpression(xpExpr, xpHdr, &output, true, paramNames, false);
					attrValue = output.data;
					valueCopy = true;
				}
				else if ((element->common.kind == XNTNODE_ELEMENT && i == XNT_ELEMENT_NAME) ||
						 (element->common.kind == XNTNODE_ATTRIBUTE &&
					  (i == XNT_ATTRIBUTE_NAME || i == XNT_ATTRIBUTE_VALUE)))
				{
					attrValue = dumpXMLAttrBinaryValue(attrValue, paramNames, NULL, NULL, NULL);
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
				attrValue = dumpXMLAttrBinaryValue(attrValue, paramNames, NULL, NULL, NULL);
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
			cursor = ensureSpace(output, attrValueLen);
			memcpy(cursor, attrValue, attrValueLen);
		}

		if (valueCopy)
		{
			pfree(attrValue);
		}
		cursor = ensureSpace(output, 1);
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
			cursor = ensureSpace(output, incrInput);
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
	char	   *cursor = ensureSpace(output, len + 1);

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
ensureSpace(StringInfo output, unsigned int toWrite)
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

static void
visitXMLNodeForDump(XMLNodeHdr *stack, unsigned depth, void *userData)
{
	XMLNodeHdr	node = (XMLNodeHdr) stack[depth];
	struct XMLNodeDumpInfo *ud = (struct XMLNodeDumpInfo *) userData;
	StringInfo	output = ud->output;
	char	   *str;
	unsigned int size;
	XMLNodeOffset offAbs = (char *) node - ud->start;
	XMLNodeOffset offRel = 0;

	appendStringInfoSpaces(output, depth);

	if (depth > 0)
	{
		XMLNodeHdr	parent = (XMLNodeHdr) stack[depth - 1];

		offRel = (char *) parent - (char *) node;
	}

	switch (node->kind)
	{
		case XMLNODE_ELEMENT:
			size = getXMLNodeSize(node, true);
			appendStringInfo(output, "%s (abs: %u , rel: %u , size: %u)\n",
			XNODE_ELEMENT_NAME((XMLCompNodeHdr) node), offAbs, offRel, size);
			break;

		case XMLNODE_ATTRIBUTE:
			str = XNODE_CONTENT(node);
			break;

		case XMLNODE_COMMENT:
			str = "<comment>";
			break;

		case XMLNODE_CDATA:
			str = "CDATA";
			break;

		case XMLNODE_PI:
			str = "PI";
			break;

		case XMLNODE_TEXT:
			str = "<text>";
			break;

		case XMLNODE_DOC_FRAGMENT:
			str = "<fragment>";
			break;

		default:
			elog(ERROR, "unrecognized node kind %u", node->kind);
			break;

	}

	if (node->kind != XMLNODE_ELEMENT)
	{
		size = getXMLNodeSize(node, true);
		if (node->kind == XMLNODE_ATTRIBUTE)
		{
			appendStringInfoChar(output, '@');
		}
		appendStringInfo(output, "%s (abs: %u , rel: %u , size: %u)\n", str, offAbs, offRel, size);
	}
}

/*
 * Tries to find out if 'element' contains a namespace declaration of a new prefix
 * for 'namespaceURI'that overrides another one defined above. If not found,
 * 'currentPrefix' is returned.
 */
static char *
getValidPrefix(XMLCompNodeHdr element, char *namespaceURI, char *currentPrefix)
{
	XMLNodeIteratorData iterator;
	XMLNodeHdr	attrNode;
	XMLNodeKind kind = element->common.kind;

	/*
	 * Some nodes don't have attributes and therefore can't declare
	 * namespaces.
	 */
	if (kind == XMLNODE_DOC || kind == XMLNODE_DOC_FRAGMENT ||
		kind == XMLTEMPLATE_ROOT)
		return NULL;

	/*
	 * Whether the namespace prefix is passed or not, we need to check whether
	 * different one is not used at the current level of the tree.
	 */
	initXMLNodeIteratorSpecial(&iterator, element, false);

	while ((attrNode = getNextXMLNodeChild(&iterator)) != NULL)
	{
		char	   *attrName,
				   *attrValue,
				   *cursor;

		if (attrNode->kind != XMLNODE_ATTRIBUTE)
			break;

		Assert((attrNode->flags & XNODE_ATTR_VALUE_BINARY) == 0);

		attrName = XNODE_CONTENT(attrNode);
		if (strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX,
					strlen(XNODE_NAMESPACE_DEF_PREFIX)) == 0)
		{
			char	   *prefixNew = NULL;

			cursor = attrName + strlen(XNODE_NAMESPACE_DEF_PREFIX);
			if (*cursor == '\0')
			{
				/* Default namespace - no prefix. */
				prefixNew = cursor;
			}
			else if (*cursor == XNODE_CHAR_COLON)
			{
				/* Non-default namespace: prefix follows the colon. */
				prefixNew = cursor + 1;
			}
			else
			{
				/* Not a namespace declaration. */
				continue;
			}

			attrValue = attrName + strlen(attrName) + 1;
			if (strcmp(attrValue, namespaceURI) == 0)
			{
				return prefixNew;
			}
		}
	}

	/*
	 * The current prefix is not overridden here, keep using the one defined
	 * above.
	 */
	return currentPrefix;
}
