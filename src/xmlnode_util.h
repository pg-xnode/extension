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
extern void xmlnodeContainerFreeItems(XMLNodeContainer cont);
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
extern void copyXMLNodeOrDocFragment(XMLNodeHdr newNode, char **resCursor, char **newNdRoot,
						 char ***newNdRoots);
extern XMLNodeHdr getFirstXMLNodeLeaf(XMLCompNodeHdr compNode);

extern XMLNodeHdr getNextXMLNode(XMLScan xscan);
extern void checkXMLWellFormedness(XMLCompNodeHdr root);
extern int	utf8cmp(char *c1, char *c2);

extern double xnodeGetNumValue(char *str, bool raiseError, bool *isNumber);
extern char *getElementNodeStr(XMLCompNodeHdr element);
extern char *getNonElementNodeStr(XMLNodeHdr node);

#define XNODE_GET_STRING(node) (XNODE_IS_COMPOUND(node) ?\
			getElementNodeStr((XMLCompNodeHdr) node) : getNonElementNodeStr(node))

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
extern bool xmlStringWhitespaceOnly(char *str);
extern bool checkFragmentForAttributes(XMLCompNodeHdr fragment);
extern bool isXMLNodeDescendant(XMLNodeHdr node, XMLCompNodeHdr treeRoot);

extern void xnodeInitStringInfo(StringInfo stringInfo, int len);

typedef struct XMLNodeIteratorData
{
	XMLCompNodeHdr node;
	char		bwidth;
	unsigned short childrenLeft;
	char	   *childOffPtr;

	/* Is attribute considered a child? */
	bool		attributes;
} XMLNodeIteratorData;

typedef struct XMLNodeIteratorData *XMLNodeIterator;

extern void initXMLNodeIterator(XMLNodeIterator iterator, XMLCompNodeHdr node,
					bool attributes);
extern void initXMLNodeIteratorSpecial(XMLNodeIterator iterator, XMLCompNodeHdr node,
						   bool attrsSpecial);
extern XMLNodeHdr getNextXMLNodeChild(XMLNodeIterator iterator);
extern XMLCompNodeHdr getXMLDocRootElement(XMLCompNodeHdr docRoot,
					 XMLNodeKind required);

#endif   /* XMLNODE_UTIL_H_ */
