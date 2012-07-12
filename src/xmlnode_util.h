/*
 * Copyright (C) 2012, Antonin Houska
 */

#ifndef XMLNODE_UTIL_H_
#define XMLNODE_UTIL_H_

#include "lib/stringinfo.h"
#include "mb/pg_wchar.h"

#include "xmlnode.h"
#include "xpath.h"

extern void xmlnodeContainerInit(XMLNodeContainer cont);
extern void xmlnodeContainerFree(XMLNodeContainer cont);

extern void xmlnodePushBoolean(XMLNodeContainer cont, bool boolean);
extern void xmlnodePushSingleNode(XMLNodeContainer stack, XMLNodeOffset valu);
extern void xmlnodePushSinglePtr(XMLNodeContainer cont, void *item);
extern void xmlnodeAddListItem(XMLNodeContainer cont, XNodeListItem *itemNew);
extern XMLNodeOffset xmlnodePopOffset(XMLNodeContainer stack);

extern unsigned int getXMLNodeSize(XMLNodeHdr node, bool subtree);
extern char *getXMLNodeKindStr(XMLNodeKind k);
extern char *copyXMLNode(XMLNodeHdr node, char *target, bool xmlnode, XMLNodeOffset *root);
extern char **copyXMLDocFragment(XMLCompNodeHdr fragNode, char **resCursorPtr);
extern void copyXMLNodeOrDocFragment(XMLNodeHdr newNode, unsigned int newNdSize, char **resCursor,
						 char **newNdRoot, char ***newNdRoots);
extern XMLNodeHdr getFirstXMLNodeLeaf(XMLCompNodeHdr compNode);

extern XMLNodeHdr getNextXMLNode(XMLScan xscan, bool removed);
extern void checkXMLWellFormedness(XMLCompNodeHdr root);
extern int	utf8cmp(char *c1, char *c2);

extern double xnodeGetNumValue(char *str, bool raiseError, bool *isNumber);
extern char *getElementNodeStr(XMLCompNodeHdr element);
extern char *getNonElementNodeStr(XMLNodeHdr node);

/*
 * Abstracted functionality to walk through the XML document tree recursively.
 *
 * IMPORTANT: the visitor function *must not* change any node in 'stack', neither
 * the 'depth' argument.
 */

typedef void (*VisitXMLNode) (XMLNodeHdr *stack, unsigned int depth, void *userData);

#define XMLTREE_STACK_CHUNK			16
#define XMLTREE_WALKER_MAX_DEPTH	0xFF

typedef struct
{
	XMLNodeHdr *stack;
	unsigned short stackSize;
	unsigned short depth;
	bool		attributes;
	void	   *userData;
	VisitXMLNode visitor;
} XMLTreeWalkerContext;

extern void walkThroughXMLTree(XMLNodeHdr rootNode, VisitXMLNode visitor, bool attributes, void *userData);

extern void dumpXMLNodeDebug(StringInfo output, char *data, XMLNodeOffset rootOff);

extern bool xmlStringIsNumber(char *str, double *numValue, char **end, bool skipWhitespace);

extern bool checkFragmentForAttributes(XMLCompNodeHdr fragment);

extern char **getUnresolvedXMLNamespaces(XMLNodeHdr node, unsigned int *count);
extern void resolveNamespaces(XMLNodeContainer declarations, unsigned int declsActive, char *elNmspName,
				  bool *elNmspNameResolved, XMLNodeHdr *attrsPrefixed, unsigned int attrsPrefixedCount, bool *attrFlags,
				  unsigned short *attrsUnresolved, char *specNmspName, char *specNmspValue, bool *elNmspIsSpecial);
extern void collectXMLNamespaceDeclarations(XMLCompNodeHdr currentNode, unsigned int *attrCount,
								unsigned int *nmspDeclCount, XMLNodeContainer declarations, bool declsOnly, XMLNodeHdr **attrsPrefixed,
								unsigned int *attrsPrefixedCount);

extern void xnodeInitStringInfo(StringInfo stringInfo, int len);

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

#endif   /* XMLNODE_UTIL_H_ */
