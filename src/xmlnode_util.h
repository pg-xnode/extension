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
extern void xmlnodePushSingle(XMLNodeContainer stack, XMLNodeOffset value);
extern void xmlnodeAddListItem(XMLNodeContainer cont, XNodeListItem *itemNew);
extern XMLNodeOffset xmlnodePop(XMLNodeContainer stack);

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

extern double xnodeGetNumValue(char *str, bool raiseError, bool * isNumber);
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
	void	   *userData;
	VisitXMLNode visitor;
} XMLTreeWalkerContext;

extern void walkThroughXMLTree(XMLNodeHdr rootNode, VisitXMLNode visitor, void *userData);

extern void dumpXMLNodeDebug(StringInfo output, char *data, XMLNodeOffset rootOff);

extern bool xmlStringIsNumber(char *str, double *numValue, char **end, bool skipWhitespace);

extern bool checkFragmentForAttributes(XMLCompNodeHdr fragment);



#endif   /* XMLNODE_UTIL_H_ */
