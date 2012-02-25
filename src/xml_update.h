/*
 * Copyright (C) 2012, Antonin Houska
 */

#ifndef XMLNODE_UPDATE_H_
#define XMLNODE_UPDATE_H_

#include "xpath.h"

extern Datum xmlnode_add(PG_FUNCTION_ARGS);
extern Datum xmlnode_remove(PG_FUNCTION_ARGS);

extern xmldoc updateXMLDocument(XMLScan xscan, xmldoc doc, XMLNodeAction action, XMLNodeHdr newNode,
				  XMLAddMode addMode);
extern xmldoc xmlnodeAdd(xmldoc doc, XMLScan xscan, XMLNodeHdr targNode, XMLNodeHdr newNode,
		   XMLAddMode mode, bool freeSrc, XNodeListItem * ignore);
extern xmldoc xmlnodeRemove(xmldoc doc, XMLScan xscan, XMLNodeHdr targNode, bool freeSrc);

#endif   /* XMLNODE_UPDATE_H_ */
