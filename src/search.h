/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#ifndef XML_SEARCH_H_
#define XML_SEARCH_H_

#include "postgres.h"
#include "fmgr.h"
#include "mb/pg_wchar.h"

#define XML_BRANCH_MAX_STEPS	16

/*
 * The branch resembles location path, but very simple one.
 * For example it has no test predicates.
 *
 * Sequence of NULL-terminated node tests immediately follows the structure.
 * No slash character is stored.
 *
 * No alignment required so far. Reconsider when adding new attributes.
 */
typedef struct XMLBranchData
{
	uint8		depth;
} XMLBranchData;

typedef XMLBranchData *XMLBranch;

typedef struct varlena xmlbranchtype;
typedef xmlbranchtype *xmlbranch;

extern Datum xmlbranch_in(PG_FUNCTION_ARGS);
extern Datum xmlbranch_out(PG_FUNCTION_ARGS);

extern Datum xmlbranch_eq(PG_FUNCTION_ARGS);
extern Datum xmlbranch_lt(PG_FUNCTION_ARGS);
extern Datum xmlbranch_lte(PG_FUNCTION_ARGS);
extern Datum xmlbranch_gt(PG_FUNCTION_ARGS);
extern Datum xmlbranch_gte(PG_FUNCTION_ARGS);
extern Datum xmlbranch_compare(PG_FUNCTION_ARGS);

extern Datum xmldoc_contains_branch(PG_FUNCTION_ARGS);

#define GIN_STRATEGY_CONTAINS	2

/*
 * GIN support functions
 */
extern Datum ginxmlextract(PG_FUNCTION_ARGS);
extern Datum ginqueryxmlextract(PG_FUNCTION_ARGS);
extern Datum ginxmlconsistent(PG_FUNCTION_ARGS);

#endif   /* XML_SEARCH_H_ */
