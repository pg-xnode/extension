/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#ifndef XSLT_H_
#define XSLT_H_

#include "xmlnode.h"

#define XSLT_MAX_TEMPLATES_PER_SHEET	16

#define XSL_VERSION_STR		"1.0"

#define XSLNODE_NAMESPACE_URI		"http://www.w3.org/1999/XSL/Transform"

#define XSL_SHEET				"stylesheet"
#define XSL_SHEET_VERSION		0

#define XSL_TEMPLATE			"template"
#define XSL_TEMPLATE_MATCH		0

extern XNodeSpecAttributes xslAttributeInfo[];

typedef struct varlena xsltype;
typedef xsltype *xsl;

extern Datum xnode_xsl_in(PG_FUNCTION_ARGS);
extern Datum xnode_xsl_out(PG_FUNCTION_ARGS);

extern Datum xsl_transform(PG_FUNCTION_ARGS);

#endif   /* XSLT_H_ */
