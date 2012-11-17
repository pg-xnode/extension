/*
 * Copyright (C) 2012, Antonin Houska
 */

#include "xmlnode.h"
#include "xmlnode_util.h"

typedef struct XMLNamespaceCheckState
{
	char	   *tree;

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
getUnresolvedXMLNamespaces(char *tree, XMLNodeHdr node, unsigned int *count)
{
	XMLCompNodeHdr element;
	XMLNamespaceCheckState stateData;
	char	  **result = NULL;
	unsigned int resultSize;
	unsigned int i;

	*count = 0;

	if (node->kind == XMLNODE_ATTRIBUTE && (node->flags & XNODE_NMSP_PREFIX))
	{
		char	   *attrName = XNODE_CONTENT(node);

		if (XNODE_IS_NAMESPACE_DECL(attrName))
			/* 'xmlns:...' does not depend on any namespace declaration. */
			return NULL;

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

	if (!XNODE_IS_COMPOUND(node))
		return NULL;

	xmlnodeContainerInit(&stateData.declarations);
	xmlnodeContainerInit(&stateData.result);
	stateData.tree = tree;
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
resolveXMLNamespaces(char *tree, XMLNodeContainer declarations, unsigned int declsActive, char *elNmspName,
					 bool *elNmspNameResolved, XMLNodeHdr *attrsPrefixed, unsigned int attrsPrefixedCount, bool *attrFlags,
   unsigned short *attrsUnresolved, char *specNmspURI, bool *elNmspIsSpecial)
{
	unsigned int i;
	XNodeListItem *decls = declarations->content;

	/*
	 * Search the stack bottom-up for the first appropriate declaration.
	 */
	for (i = declsActive; i > 0; i--)
	{
		char	   *declAttrName,
				   *declNmspName = NULL;
		XNodeListItem *item = decls + i - 1;
		unsigned int declNmspLength = 0;
		XMLNodeHdr	declNode;
		bool		nsDefault;

		Assert(item->kind == XNODE_LIST_ITEM_SINGLE_OFF);
		declNode = (XMLNodeHdr) (tree + item->value.singleOff);
		declAttrName = XNODE_CONTENT(declNode);
		nsDefault = XNODE_IS_DEF_NAMESPACE_DECL(declAttrName);

		if (!nsDefault)
		{
			declNmspName = declAttrName + strlen(XNODE_NAMESPACE_DEF_PREFIX) + 1;
			declNmspLength = strlen(declNmspName);
		}

		if (!(*elNmspNameResolved))
		{
			if (elNmspName != NULL)
			{
				/* The element does have namespace prefix. */

				if (!nsDefault &&
					strncmp(elNmspName, declNmspName, declNmspLength) == 0 &&
					elNmspName[declNmspLength] == XNODE_CHAR_COLON)
				{
					*elNmspNameResolved = true;

					/*
					 * This seems to be a special namespace (e.g. 'xnt'), so
					 * check whether the namespace value matches. If it does
					 * not, then we can't say the element is special
					 * (althhough the next declaration may change it.)
					 */
					if (specNmspURI != NULL && elNmspIsSpecial != NULL)
					{
						char	   *declNmspValue = (char *) declNmspName + declNmspLength + 1;

						*elNmspIsSpecial = (strcmp(declNmspValue, specNmspURI) == 0);
					}
				}
			}
			else
			{
				/*
				 * The element does not have namespace prefix. It can be
				 * special node yet: if default namespace is bound to the
				 * appropriate URI.
				 */
				if (nsDefault)
				{
					char	   *declAttrValue = declAttrName + strlen(declAttrName) + 1;

					/*
					 * The default namespace may be a special one and thus
					 * turn the node into special one.
					 */
					*elNmspIsSpecial = (specNmspURI != NULL) &&
						strcmp(specNmspURI, declAttrValue) == 0;
				}

				if (*elNmspIsSpecial)

					/*
					 * Match found, no need to check other namespace
					 * declarations. If this condition is not met and matching
					 * declaration of default namespace is not found, then the
					 * element will stay 'unresolved'. The caller of this
					 * function should know that it's o.k. for element that
					 * has no prefix.
					 */
					*elNmspNameResolved = true;
			}
		}

		if (*attrsUnresolved > 0 && !nsDefault)
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
collectXMLNamespaceDeclarations(char *tree, XMLCompNodeHdr currentNode, unsigned int *attrCount, unsigned int *nmspDeclCount,
								XMLNodeContainer declarations, bool declsOnly, XMLNodeHdr **attrsPrefixed, unsigned int *attrsPrefixedCount)
{
	XMLNodeIteratorData iterator;
	XMLNodeHdr	childNode;

	if (!declsOnly)
	{
		*attrCount = 0;
		*attrsPrefixedCount = 0;
	}

	Assert(currentNode->common.kind == XMLNODE_ELEMENT);

	initXMLNodeIterator(&iterator, currentNode, true);

	while ((childNode = getNextXMLNodeChild(&iterator)) != NULL)
	{

		if (childNode->kind != XMLNODE_ATTRIBUTE)
			break;

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

			if (XNODE_IS_NAMESPACE_DECL(attrName))
			{
				/* Namespace declaration. */

				xmlnodePushSingleNode(declarations, (char *) childNode - tree);
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
 * Return URI of namespace specified by 'prefix' or NULL if not found.
 *
 * (prefix == NULL) means default namespace.
 */
char *
getXMLNamespaceURI(char *prefix, XMLNodeContainer declarations, char *parsed)
{
	XNodeListItem *declItem;
	unsigned int i;
	unsigned int prefLen;
	XMLNodeHdr	declNode;

	if (declarations->position == 0)
		return NULL;

	prefLen = (prefix != NULL) ? strlen(prefix) : 0;

	/*
	 * Search from bottom because declaration at lower level overrides those
	 * above.
	 */
	declItem = declarations->content + declarations->position - 1;
	for (i = 0; i < declarations->position; i++)
	{

		char	   *prefixDeclared,
				   *cursor;

		declNode = (XMLNodeHdr) (parsed + declItem->value.singleOff);

		/* Skip "xmlns" */
		cursor = XNODE_CONTENT(declNode) + strlen(XNODE_NAMESPACE_DEF_PREFIX);
		if (*cursor == '\0')
		{
			/* xmlns=<namespace URI> */

			if (prefix == NULL)

				/*
				 * Declaration of default namespace found. The namespace URI
				 * is the attribute value and as such it starts right after
				 * the terminating NULL.
				 */
				return (cursor + 1);
			else
			{
				/* Not interested in declaration of default namespace. */
				declItem--;
				continue;
			}
		}

		Assert(*cursor == XNODE_CHAR_COLON);
		cursor++;

		prefixDeclared = cursor;
		if (prefix != NULL && strcmp(prefix, prefixDeclared) == 0)
		{
			/*
			 * Namespace URI is value of the declaration (attribute) node. We
			 * get it by skipping the rest of the name and the terminating
			 * NULL.
			 */
			return prefixDeclared + strlen(prefixDeclared) + 1;
		}
		declItem--;
	}

	return NULL;
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
		collectXMLNamespaceDeclarations(state->tree, currentNode, &attrCount, &nmspDeclCount, &state->declarations, false,
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
		resolveXMLNamespaces(state->tree, &state->declarations, state->counts[depth], elNmspName, &elNmspNameResolved, attrsPrefixed,
				attrsPrefixedCount, attrFlags, &attrsUnresolved, NULL, NULL);
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

	Assert(elNmspLen > 0);
	return pnstrdup(name, elNmspLen);
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
