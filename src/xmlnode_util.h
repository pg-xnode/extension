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

extern unsigned int getXMLNodeSize(XMLNodeHeader node, bool subtree);
extern char *getXMLNodeKindStr(XMLNodeKind k);
extern char *copyXMLNode(XMLNodeHeader node, char *target, bool xmlnode, XMLNodeOffset * root);
extern XMLNodeHeader getFirstXMLNodeLeaf(XMLElementHeader elh);

void initXMLScan(XMLScan xscan, XMLScan parent, XPath xpath, XPathHeader xpHdr, XMLElementHeader scanRoot,
			xmldoc document, bool checkUniqueness);
extern void finalizeXMLScan(XMLScan xscan);

extern XMLNodeHeader getNextXMLNode(XMLScan xscan, bool removed);
extern		xmldoc xmlnodeAdd(xmldoc doc, XMLScan xscan, XMLNodeHeader targNode, XMLNodeHeader newNode,
		   XMLAddMode mode, bool freeSrc);
extern xmldoc xmlnodeRemove(xmldoc doc, XMLScan xscan, XMLNodeHeader targNode, bool freeSrc);
extern void checkXMLWellFormedness(XMLElementHeader root);
extern bool isXMLCharInInterval(char *c, UTF8Interval * intervals, unsigned short int intCount);
extern int	utf8cmp(char *c1, char *c2);

extern Datum xmlnode_add(PG_FUNCTION_ARGS);
extern Datum xmlnode_remove(PG_FUNCTION_ARGS);

#endif   /* XMLNODE_UTIL_H_ */
