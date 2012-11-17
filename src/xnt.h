/*
 * Copyright (C) 2012, Antonin Houska
 */

#ifndef XNT_H_
#define XNT_H_

#include "access/tupdesc.h"
#include "catalog/pg_cast.h"
#include "catalog/pg_proc.h"
#include "catalog/pg_type.h"
#include "utils/array.h"
#include "utils/lsyscache.h"
#include "utils/builtins.h"
#include "utils/syscache.h"
#include "utils/typcache.h"

#include "xpath.h"

#define XNTNODE_NAMESPACE_URI			"http://www.pg-xnode.org/xnt"

#define XNT_TEMPLATE			"template"
#define XNT_TEMPLATE_PRESERVE	0

#define XNT_COPY_OF				"copy-of"
#define XNT_COPY_OF_EXPR		0

#define XNT_ELEMENT				"element"
#define XNT_ELEMENT_NAME		0

#define XNT_ATTRIBUTE			"attribute"
#define XNT_ATTRIBUTE_NAME		0
#define XNT_ATTRIBUTE_VALUE		1


extern XNodeSpecAttributes xntAttributeInfo[];

typedef struct varlena xnttype;
typedef xnttype *xnt;

extern Datum xnode_template_in(PG_FUNCTION_ARGS);
extern Datum xnode_template_out(PG_FUNCTION_ARGS);

extern Datum xnode_from_template(PG_FUNCTION_ARGS);

#endif   /* XNT_H_ */
