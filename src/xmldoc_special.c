/*
 * Support for special XML documents, such as XSD, XSLT, etc.
 *
 * Copyright (C) 2012, Antonin Houska
 */

#include "postgres.h"

#include "template.h"
#include "xnt.h"
#include "xpath.h"

static char *getAttributeLocalName(char *prefix, char *attrNamePrefixed,
					  XMLNodeContainer nmspDecls, char *parserOutput);

/*
 * Ensure that:
 *
 * 1. Reserved attribute are stored at defined positions.
 * 2. Name is not stored for the reserved attributes (the position determines
 * the attribute itself).
 * 3. Where appropriate, the attribute value is stored in special binary format
 * as opposed to NULL-terminated string.
 *
 * 'prefix' - namespace prefix of the owning node of the attributes.
 * The namespace should have been validated when passed here.
 *
 * 'nmspDecls' - namespace declarations valid in the current context.
 *
 * 'attrOffsets' - both input and output. It's expected to provide offsets of
 * attributes to be checked. Offsets of the processed attributes are returned.
 * Therefore there has to be sufficient space for slots for optional attributes
 * that the input document does not contain.
 * 'attrCount + XNT_SPECIAL_ATTRS_MAX' is always the safe size.
 *
 * 'attrCount' - the actual number of attributes that the template element contains.
 *
 * 'offsetsValid' - if given attribute is optional and the input element does not
 * contain it, then the corresponding position in 'attrOfsets' will be marked
 * as invalid. Size must be equal to that of 'attrOffsets'.
 *
 */
char *
preprocessSpecialXMLAttributes(char *prefix, XMLNodeContainer nmspDecls,
	XNodeListItem *attrOffsets, unsigned short attrCount, char *parserOutput,
 XMLNodeKind specNodeKind, XNodeSpecAttributes *attrInfo, bool *offsetsValid,
  unsigned int *specAttrCount, unsigned int *outSize, unsigned int *outCount,
			XMLNodeContainer paramNames, GetSpecialXNodNameFunc specNodeName)
{
	unsigned short i;
	XMLNodeHdr *attrsSorted = NULL;

	/*
	 * Space for the reserved attributes must always be there, even if all
	 * attributes of the given element are optional and user does not use any
	 * (namespace declarations are always allowed so the corner case is that
	 * user only specifies namespace declarations and nothing else).
	 */
	unsigned int outAttrsMax = attrCount + attrInfo->number;
	bool	   *attrValsToFree = NULL;
	char	   *resTmp,
			   *result = NULL;
	unsigned int *attrSizes = NULL;
	unsigned int indGen;
	unsigned int outSizeMax = 0;

	*outCount = 0;
	*specAttrCount = 0;

	if (outAttrsMax > 0)
	{
		unsigned int size;

		size = outAttrsMax * sizeof(bool);;
		attrValsToFree = (bool *) palloc(size);
		memset(attrValsToFree, false, size);

		size = outAttrsMax * sizeof(unsigned int);
		attrSizes = (unsigned int *) palloc(size);
		memset(attrSizes, 0, size);

		size = outAttrsMax * sizeof(XMLNodeHdr);
		attrsSorted = (XMLNodeHdr *) palloc(size);
		memset(attrsSorted, 0, size);
	}

	/*
	 * 'attrsSorted' will start with the special (reserved) attrs, while those
	 * 'generic' will follow.
	 */
	indGen = attrInfo->number;

	/*
	 * Sort the attributes, i.e. put the reserved into the slots at the start
	 * of 'attrsSorted'.
	 */
	for (i = 0; i < attrCount; i++)
	{
		unsigned short j;
		XNodeListItem *attrItem = attrOffsets + i;
		XMLNodeHdr	attr = (XMLNodeHdr) (parserOutput + attrItem->value.singleOff);
		char	   *attrName,
				   *attrValue;
		bool		found = false;

		attrName = XNODE_CONTENT(attr);

		if (attr->flags & XNODE_NMSP_PREFIX)
			attrName = getAttributeLocalName(prefix, attrName, nmspDecls, parserOutput);

		attrValue = attrName + strlen(attrName) + 1;

		/* Is this a reserved attribute? */
		for (j = 0; j < attrInfo->number; j++)
		{
			if (strcmp(attrName, attrInfo->names[j]) == 0)
			{
				XMLNodeHdr	specNode;
				char	   *specNodeValue;
				unsigned int valueSize,
							newAttrSize;
				XPathExpression expr = NULL;
				char	   *attrValueTokenized = NULL;

				if (specNodeKind == XNTNODE_COPY_OF &&
					strcmp(attrName, attrInfo->names[XNT_COPY_OF_EXPR]) == 0)
				{
					expr = getXPathExpressionForXMLTemplate(attrValue, XPATH_TERM_NULL, paramNames, NULL);
					valueSize = expr->common.size;
				}
				else if (strlen(attrValue) > 0 && strchr(attrValue, XNODE_CHAR_LBRKT_CUR) != NULL)
				{
					attrValueTokenized = getAttrValueForXMLTemplate(attrValue, &valueSize, paramNames);
				}
				else
				{
					/* Plain string or empty attribute. */
					valueSize = strlen(attrValue) + 1;
				}

				newAttrSize = sizeof(XMLNodeHdrData) + valueSize;
				specNode = (XMLNodeHdr) palloc(newAttrSize);
				specNode->kind = attr->kind;
				specNode->flags = attr->flags;
				/* There's no need to store special attribute's name. */
				specNodeValue = XNODE_CONTENT(specNode);

				if (expr != NULL)
				{
					/*
					 * 'expr' is now treated as as binary stream (no access to
					 * the structures) so we can forget about alignment for a
					 * while. All we need to know is where the data start in
					 * the new node. This position will control the alignment
					 * in the resulting document.
					 */
					memcpy(specNodeValue, expr, valueSize);
					pfree(expr);
					specNode->flags |= XNODE_ATTR_VALUE_BINARY;
				}
				else if (attrValueTokenized != NULL)
				{
					/* Likewise, see the comment above. */
					memcpy(specNodeValue, attrValueTokenized, valueSize);
					pfree(attrValueTokenized);
					specNode->flags |= XNODE_ATTR_VALUE_BINARY;
				}
				else
				{
					strcpy(specNodeValue, attrValue);
				}

				attrsSorted[j] = specNode;
				outSizeMax += newAttrSize;
				if (expr != NULL || attrValueTokenized != NULL)
				{
					outSizeMax += MAX_PADDING(XPATH_ALIGNOF_EXPR);
				}
				attrSizes[j] = newAttrSize;
				attrValsToFree[j] = true;
				found = true;
				(*specAttrCount)++;
				break;
			}
		}

		if (!found)
		{
			/* Namespace declaration is the only generic attribute allowed. */
			/* Either "xmlns:<namespace>"=... */
			if (((attr->flags & XNODE_NMSP_PREFIX) &&
				 XNODE_IS_NAMESPACE_DECL(attrName))
			/* Or "xmlns"=... */
				|| XNODE_IS_DEF_NAMESPACE_DECL(attrName))
			{

				attrValue = attrName + strlen(attrName) + 1;
				/* The whole attribute is stored in this case. */
				attrsSorted[indGen] = attr;
				attrSizes[indGen] = sizeof(XMLNodeHdrData) + strlen(attrName) +strlen(attrValue) + 2;
				outSizeMax += attrSizes[indGen];
				indGen++;
			}
			else
			{
				elog(ERROR, "element '%s' does not accept attribute '%s'",
					 specNodeName(specNodeKind, NULL), attrName);
			}
		}
	}

	/* Check for missing required attributes */
	for (i = 0; i < attrInfo->number; i++)
	{
		if (attrInfo->required[i] && (i >= attrCount || attrsSorted[i] == NULL))
		{
			elog(ERROR, "required attribute '%s' missing in element '%s'", attrInfo->names[i],
				 specNodeName(specNodeKind, NULL));
		}
	}

	if (outAttrsMax > 0)
	{
		XNodeListItem *attrItem = attrOffsets;

		/*
		 * The first offset does not change so we can use it as initial new
		 * value.
		 */
		XMLNodeOffset offNew = attrItem->value.singleOff;

		result = resTmp = (char *) palloc(outSizeMax);

		/*
		 * Construct the new sequence of attributes and adjust parent's
		 * offsets.
		 */
		for (i = 0; i < indGen; i++)
		{
			unsigned int currentSize = attrSizes[i];

			if (currentSize > 0)
			{
				char	   *ptr,
						   *ptrAligned;
				unsigned int padding = 0;
				XMLNodeHdr	attribute;

				Assert(attrsSorted[i] != NULL);
				attribute = attrsSorted[i];

				if (attribute->flags & XNODE_ATTR_VALUE_BINARY)
				{
					/*
					 * If the next node is located immediately after the
					 * previous, then 'ptr' is the position inside the new
					 * node controlling the alignment (typically XPath
					 * expression). As the 'binary nodes' have the
					 * alignment-sensitive data right after the header, the
					 * header size is used to derive the position.
					 */
					ptr = resTmp + sizeof(XMLNodeHdrData);

					/*
					 * If that address is not aligned, padding must be
					 * prepended.
					 */
					ptrAligned = (char *) TYPEALIGN(XPATH_ALIGNOF_EXPR, ptr);
					padding = ptrAligned - ptr;
				}

				resTmp += padding;
				memcpy(resTmp, attrsSorted[i], currentSize);
				resTmp += currentSize;
				offNew += padding;
				offsetsValid[i] = true;
			}
			else
			{
				Assert(attrsSorted[i] == NULL);
				offsetsValid[i] = false;
			}

			attrItem->value.singleOff = offNew;
			offNew += currentSize;
			attrItem++;
		}

		*outCount = indGen;
		*outSize = resTmp - result;

		for (i = 0; i < outAttrsMax; i++)
		{
			if (attrValsToFree[i])
			{
				pfree(attrsSorted[i]);
			}
		}
		pfree(attrValsToFree);
		pfree(attrSizes);
		pfree(attrsSorted);
	}
	return result;
}

/*
 * If the attribute has the correct prefix, let's ignore the prefix for
 * simplicity. If the prefix is not correct (e.g. bound to wrong URI), return
 * the original (prefixed) name, so it fails to match to any attribute
 * of given special element.
 */
static char *
getAttributeLocalName(char *prefix, char *attrNamePrefixed,
					  XMLNodeContainer nmspDecls, char *parserOutput)
{
	unsigned int prefLenEl = strlen(prefix);
	char	   *result = attrNamePrefixed;

	if (strncmp(prefix, attrNamePrefixed, prefLenEl) == 0 &&
		attrNamePrefixed[prefLenEl] == XNODE_CHAR_COLON)
	{
		/* Skip both the prefix and colon. */
		result += prefLenEl + 1;
	}
	else
	{
		char	   *colon,
				   *nmspURIAttr,
				   *nmspURIElement,
				   *prefAttr;
		unsigned int prefAttrLen;

		/*
		 * In rare cases the attribute can have different prefix from that of
		 * the element, but eventually both might be bound to the same URI.
		 */

		/*
		 * The *last* colon because namespace prefix may contain multiple
		 * colons.
		 */
		colon = strrchr(attrNamePrefixed, XNODE_CHAR_COLON);
		Assert(colon != NULL);
		prefAttrLen = colon - attrNamePrefixed;
		prefAttr = pnstrdup(attrNamePrefixed, prefAttrLen);

		if (XNODE_IS_DEF_NAMESPACE_DECL(prefAttr))
		{
			/* Not interested in namespace declarations. */
			pfree(prefAttr);
			return result;
		}

		nmspURIAttr = getXMLNamespaceURI(prefAttr, nmspDecls, parserOutput);
		nmspURIElement = getXMLNamespaceURI(
				   *prefix != '\0' ? prefix : NULL, nmspDecls, parserOutput);
		if (nmspURIAttr != NULL && nmspURIElement != NULL &&
			strcmp(nmspURIAttr, nmspURIElement) == 0)
		{
			/* Skip both the prefix and colon. */
			result += prefAttrLen + 1;
		}
		pfree(prefAttr);
	}
	return result;
}
