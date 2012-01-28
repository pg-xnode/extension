#ifndef XMLNODE_UTIL_H_
#define XMLNODE_UTIL_H_

#include "lib/stringinfo.h"
#include "mb/pg_wchar.h"

#include "xmlnode.h"
#include "xpath.h"

extern void xmlnodeContainerInit(XMLNodeContainer cont);
extern void xmlnodeContainerFree(XMLNodeContainer cont);
extern void xmlnodePush(XMLNodeContainer stack, XMLNodeOffset value);
extern XMLNodeOffset xmlnodePop(XMLNodeContainer stack);

extern unsigned int getXMLNodeSize(XMLNodeHdr node, bool subtree);
extern char *getXMLNodeKindStr(XMLNodeKind k);
extern char *copyXMLNode(XMLNodeHdr node, char *target, bool xmlnode, XMLNodeOffset * root);
extern XMLNodeHdr getFirstXMLNodeLeaf(XMLCompNodeHdr compNode);

extern XMLNodeHdr getNextXMLNode(XMLScan xscan, bool removed);
extern void checkXMLWellFormedness(XMLCompNodeHdr root);
extern int	utf8cmp(char *c1, char *c2);

extern double xnodeGetNumValue(char *str);
extern char *getElementNodeStr(XMLCompNodeHdr element);
extern char *getNonElementNodeStr(XMLNodeHdr node);

extern void dumpXMLNodeDebug(StringInfo output, char *data, XMLNodeOffset rootOff);

#endif   /* XMLNODE_UTIL_H_ */
