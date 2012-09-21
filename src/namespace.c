/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "xmlnode.h"
#include "xmlnode_util.h"

typedef struct XMLNamespaceCheckState
{
	XMLNodeContainerData declarations;

	/* How many (non-unique) declarations each level of the stack adds. */
	unsigned int counts[XMLTREE_WALKER_MAX_DEPTH];

	/*
	 * Array of unbound namespaces (without 'xmlns:' prefix). Each element of
	 * this array is 'unique'.
	 */

	/*
	 * TODO Enhance the container so that value can be added at any position.
	 * Then keep 'result' sorted and use binary search when checking for
	 * duplicate values.
	 */
	XMLNodeContainerData result;
} XMLNamespaceCheckState;

static void checkNodeNamespaces(XMLNodeHdr *stack, unsigned int depth, void *userData);
static char *getNamespaceName(char *name, char *colon);
static void addUniqueNamespace(XMLNodeContainer result, char *nmspName);

/*
 * Returns vector of (unique) namespaces used by 'node' and its descendants but not declared
 * at the appropriate level or above.
 *
 * 'count' receives length of the array.
 */
char	  **
getUnresolvedXMLNamespaces(XMLNodeHdr node, unsigned int *count)
{
	XMLNodeKind nodeKind = node->kind;
	XMLCompNodeHdr element;
	XMLNamespaceCheckState stateData;
	char	  **result = NULL;
	unsigned int resultSize;
	unsigned int i;

	*count = 0;

	if (nodeKind == XMLNODE_ATTRIBUTE && (node->flags & XNODE_NMSP_PREFIX))
	{
		char	   *attrName = XNODE_CONTENT(node);
		unsigned int nmspPrefLen = strlen(XNODE_NAMESPACE_DEF_PREFIX);

		if ((strncmp(attrName, XNODE_NAMESPACE_DEF_PREFIX, nmspPrefLen) == 0) &&
			attrName[nmspPrefLen] == XNODE_CHAR_COLON)
		{
			/* 'xmlns:...' does not depend on any namespace declaration. */
			return NULL;
		}

		/*
		 * If the prefix is not 'xmlns' then it represents an unbound
		 * namespace.
		 *
		 * This is a special case when we're for example adding (prefixed)
		 * attribute to element. Attributes already pertaining to the element
		 * are checked elsewhere (i.e. while checking the owning element
		 * itself).
		 */
		result = (char **) palloc(sizeof(char *));
		result[0] = strtok(attrName, ":");
		*count = 1;
		return result;
	}

	element = (XMLCompNodeHdr) node;

	if (nodeKind != XMLNODE_DOC && nodeKind != XMLNODE_ELEMENT &&
		nodeKind != XMLNODE_DOC_FRAGMENT)
	{
		return NULL;
	}

	xmlnodeContainerInit(&stateData.declarations);
	xmlnodeContainerInit(&stateData.result);
	walkThroughXMLTree((XMLNodeHdr) node, checkNodeNamespaces, false, (void *) &stateData);
	resultSize = stateData.result.position;
	xmlnodeContainerFree(&stateData.declarations);

	if (resultSize > 0)
	{
		XNodeListItem *item;

		result = (char **) palloc(resultSize * sizeof(char *));
		item = stateData.result.content;
		for (i = 0; i < resultSize; i++)
		{
			char	   *nmspName;

			Assert(item->kind == XNODE_LIST_ITEM_SINGLE_PTR);
			nmspName = (char *) item->value.singlePtr;
			result[i] = nmspName;
			item++;
		}
	}
	xmlnodeContainerFree(&stateData.result);
	*count = resultSize;
	return result;
}

void
resolveXMLNamespaces(XMLNodeContainer declarations, unsigned int declsActive, char *elNmspName,
					 bool *elNmspNameResolved, XMLNodeHdr *attrsPrefixed, unsigned int attrsPrefixedCount, bool *attrFlags,
					 unsigned short *attrsUnresolved, char *specNmspName, char *specNmspValue, bool *elNmspIsSpecial)
{

	unsigned int i;
	XNodeListItem *decls = declarations->content;

	/*
	 * Search the stack bottom-up for the first appropriate declaration.
	 */
	for (i = declsActive; i > 0; i--)
	{
		char	   *declNmspName;
		XNodeListItem *item = decls + i - 1;
		unsigned int declNmspLength;
		XMLNodeHdr	declNode;

		Assert(item->kind == XNODE_LIST_ITEM_SINGLE_PTR);
		declNode = (XMLNodeHdr) item->value.singlePtr;
		declNmspName = (char *) XNODE_CONTENT(declNode) + strlen(XNODE_NAMESPACE_DEF_PREFIX) + 1;
		declNmspLength = strlen(declNmspName);

		if (!(*elNmspNameResolved))
		{
			if (strncmp(elNmspName, declNmspName, declNmspLength) == 0 &&
				elNmspName[declNmspLength] == XNODE_CHAR_COLON)
			{
				*elNmspNameResolved = true;

				/*
				 * If seems to be a special namespace (e.g. 'xnt'), check
				 * whether the namespace value matches. If it does not, it
				 * must be considered an 'ordinary namespace'.
				 */
				if (specNmspName != NULL && specNmspValue != NULL && elNmspIsSpecial != NULL)
				{
					char	   *declNmspValue = (char *) declNmspName + declNmspLength + 1;

					*elNmspIsSpecial = (strcmp(declNmspName, specNmspName) == 0 &&
								  strcmp(declNmspValue, specNmspValue) == 0);
				}
			}
		}

		if (*attrsUnresolved > 0)
		{
			unsigned int j;

			/*
			 * Try to match any unresolved (prefixed) attribute to the current
			 * namespace declaration.
			 */
			for (j = 0; j < attrsPrefixedCount; j++)
			{
				if (!attrFlags[j])
				{
					XMLNodeHdr	attrNode = attrsPrefixed[j];
					char	   *attrNmspName = XNODE_CONTENT(attrNode);

					Assert(attrNode->flags & XNODE_NMSP_PREFIX);

					if (strncmp(attrNmspName, declNmspName, declNmspLength) == 0 &&
						attrNmspName[declNmspLength] == XNODE_CHAR_COLON)
					{
						attrFlags[j] = true;
						(*attrsUnresolved)--;
					}
				}
			}
		}

		if (*elNmspNameResolved && *attrsUnresolved == 0)
		{
			break;
		}
	}
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

/*
 * Collect names of all namespaces used in the current node (element) and find out which
 * have no declaration, neither in this node nor above.
 */
static void
checkNodeNamespaces(XMLNodeHdr *stack, unsigned int depth, void *userData)
{
	XMLNamespaceCheckState *state;
	XMLCompNodeHdr currentNode = (XMLCompNodeHdr) stack[depth];
	char	   *elNmspName = XNODE_ELEMENT_NAME(currentNode);
	char	   *elNmspColon;
	bool		elNmspNameResolved = true;
	XMLNodeHdr *attrsPrefixed = NULL;
	bool	   *attrFlags = NULL;
	unsigned short attrsUnresolved = 0;
	unsigned int attrsPrefixedCount = 0;
	unsigned int attrCount = 0;
	unsigned int nmspDeclCount = 0;

	/*
	 * The tree walker is invoked with 'attributes=false' so it does not
	 * descend to attributes.
	 */
	Assert(currentNode->common.kind != XMLNODE_ATTRIBUTE);

	state = (XMLNamespaceCheckState *) userData;

	if (currentNode->common.kind == XMLNODE_DOC_FRAGMENT)
	{
		Assert(depth == 0);

		/*
		 * The first element of the stack must not stay uninitialized even if
		 * we're just gong to return.
		 */
		state->counts[depth] = 0;
	}

	if (currentNode->common.kind != XMLNODE_ELEMENT)
	{
		return;
	}

	if (currentNode->children > 0)
	{
		collectXMLNamespaceDeclarations(currentNode, &attrCount, &nmspDeclCount, &state->declarations, false,
										&attrsPrefixed, &attrsPrefixedCount);
		attrsUnresolved = attrsPrefixedCount;
	}

	if (attrCount > 0)
	{
		unsigned int flagsSize = currentNode->children * sizeof(bool);

		attrFlags = (bool *) palloc(flagsSize);
		memset(attrFlags, false, flagsSize);
	}

	/*
	 * 'nmspStack->counts[depth]' says how many declarations (counting from
	 * position 0 in the stack) are valid for the current xml element.
	 */
	state->counts[depth] = nmspDeclCount;

	if (depth > 0)
	{
		state->counts[depth] += state->counts[depth - 1];
	}

	/* And now finally check prefixes used in the current element. */

	Assert(elNmspName[0] != XNODE_CHAR_COLON);
	elNmspColon = strchr(elNmspName, XNODE_CHAR_COLON);
	if (elNmspColon != NULL)
	{
		elNmspNameResolved = false;
	}

	if (!elNmspNameResolved || attrsUnresolved > 0)
	{
		resolveXMLNamespaces(&state->declarations, state->counts[depth], elNmspName, &elNmspNameResolved, attrsPrefixed,
		  attrsPrefixedCount, attrFlags, &attrsUnresolved, NULL, NULL, NULL);
	}

	if (!elNmspNameResolved)
	{
		addUniqueNamespace(&state->result, getNamespaceName(elNmspName, elNmspColon));
	}

	if (attrsUnresolved > 0)
	{
		unsigned int j;

		for (j = 0; j < attrsPrefixedCount; j++)
		{
			if (!attrFlags[j])
			{
				XMLNodeHdr	attrNode = attrsPrefixed[j];
				char	   *attrName = XNODE_CONTENT(attrNode);
				char	   *attrNmspColon;

				Assert(attrName[0] != XNODE_CHAR_COLON);
				attrNmspColon = strchr(attrName, XNODE_CHAR_COLON);
				Assert(attrNmspColon != NULL);
				addUniqueNamespace(&state->result, getNamespaceName(attrName, attrNmspColon));
			}
		}
	}

	if (attrCount > 0)
	{
		pfree(attrsPrefixed);
		pfree(attrFlags);
	}
}

/*
 * Turn namespace prefix into NULL-terminated string.
 */
static char *
getNamespaceName(char *name, char *colon)
{
	unsigned int elNmspLen = colon - name;
	char	   *nmspNameCp;

	Assert(elNmspLen > 0);
	nmspNameCp = (char *) palloc(elNmspLen + 1);
	memcpy(nmspNameCp, name, elNmspLen);
	nmspNameCp[elNmspLen] = '\0';
	return nmspNameCp;
}

static void
addUniqueNamespace(XMLNodeContainer result, char *nmspName)
{
	XNodeListItem *item = result->content;
	unsigned int i;

	for (i = 0; i < result->position; i++)
	{
		char	   *nameStored;

		Assert(item->kind == XNODE_LIST_ITEM_SINGLE_PTR);
		nameStored = (char *) item->value.singlePtr;
		if (strcmp(nameStored, nmspName) == 0)
		{
			return;
		}
	}

	/* Not in the container yet, so add it. */
	xmlnodePushSinglePtr(result, (void *) nmspName);
}
