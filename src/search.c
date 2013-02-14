/*
 * Copyright (C) 2012-2013, Antonin Houska
 */

#include "search.h"
#include "xpath.h"
#include "xmlnode_util.h"
#include "access/skey.h"

static int	compareBranches(xmlbranch left, xmlbranch right);
static XPath getLocationPathFromBranch(XMLBranch branch);
static void getAllBranches(XMLCompNodeHdr root, XMLNodeContainer result,
			   char **steps, unsigned int stepsSpace, unsigned int depth);

PG_FUNCTION_INFO_V1(xmlbranch_in);


Datum
xmlbranch_in(PG_FUNCTION_ARGS)
{
	char	   *branchStr,
			   *c;
	char	   *result,
			   *resTmp;
	unsigned int resSize,
				stepPos,
				stepCount,
				i;
	bool		done = false;
	char	   *steps[XML_BRANCH_MAX_STEPS];
	unsigned int stepLenghts[XML_BRANCH_MAX_STEPS];
	XMLBranch	branch;


	c = branchStr = PG_GETARG_CSTRING(0);
	stepCount = 0;
	resSize = VARHDRSZ + sizeof(XMLBranchData);

	if (strlen(branchStr) <= 1)
		elog(ERROR, "unrecognized xml branch '%s'", branchStr);
	if (*c != XNODE_CHAR_SLASH)
		elog(ERROR, "xml branch step must start with '/'");

	while (!done)
	{
		if (stepCount >= XML_BRANCH_MAX_STEPS)
			elog(ERROR, "maximum number of branch steps is %u", XML_BRANCH_MAX_STEPS);

		c++;
		steps[stepCount] = c;
		stepPos = 0;

		while (*c != XNODE_CHAR_SLASH && *c != '\0')
		{
			if (stepPos == 0)
			{
				if (!XNODE_VALID_NAME_START(c))
					elog(ERROR, "unrecognized start character in branch element");
			}
			else if (!XNODE_VALID_NAME_CHAR(c))
				elog(ERROR, "unrecognized character in branch element");

			c += pg_utf_mblen((unsigned char *) c);
			stepPos++;
		}

		if (*c == '\0')
		{
			if (stepPos == 0)
				elog(ERROR, "xml branch must not end with '/'");
			else
				done = true;
		}

		stepLenghts[stepCount] = stepPos;
		resSize += stepPos + 1;
		stepCount++;
	}

	result = (char *) palloc(resSize);
	resTmp = result + VARHDRSZ;
	branch = (XMLBranch) resTmp;
	resTmp += sizeof(XMLBranchData);

	for (i = 0; i < stepCount; i++)
	{
		memcpy(resTmp, steps[i], stepLenghts[i]);
		resTmp += stepLenghts[i];
		*resTmp = '\0';
		resTmp++;
	}

	branch->depth = stepCount;

	SET_VARSIZE(result, resSize);
	PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(xmlbranch_out);

Datum
xmlbranch_out(PG_FUNCTION_ARGS)
{
	xmlbranch	branchRaw;
	char	   *data;
	XMLBranch	branch;
	unsigned int i;
	StringInfoData output;

	branchRaw = (xmlbranch) PG_GETARG_VARLENA_P(0);
	branch = (XMLBranch) VARDATA(branchRaw);
	data = (char *) branch + sizeof(XMLBranchData);
	xnodeInitStringInfo(&output, 64);

	for (i = 0; i < branch->depth; i++)
	{
		appendStringInfo(&output, "/%s", data);
		data += strlen(data) + 1;
	}

	PG_RETURN_POINTER(output.data);
}

PG_FUNCTION_INFO_V1(xmlbranch_eq);

Datum
xmlbranch_eq(PG_FUNCTION_ARGS)
{
	xmlbranch	left = (xmlbranch) PG_GETARG_VARLENA_P(0);
	xmlbranch	right = (xmlbranch) PG_GETARG_VARLENA_P(1);
	int			cmpRes = compareBranches(left, right);

	PG_RETURN_BOOL(cmpRes == 0);
}

PG_FUNCTION_INFO_V1(xmlbranch_lt);

Datum
xmlbranch_lt(PG_FUNCTION_ARGS)
{
	xmlbranch	left = (xmlbranch) PG_GETARG_VARLENA_P(0);
	xmlbranch	right = (xmlbranch) PG_GETARG_VARLENA_P(1);
	int			cmpRes = compareBranches(left, right);

	PG_RETURN_BOOL(cmpRes < 0);
}

PG_FUNCTION_INFO_V1(xmlbranch_lte);

Datum
xmlbranch_lte(PG_FUNCTION_ARGS)
{
	xmlbranch	left = (xmlbranch) PG_GETARG_VARLENA_P(0);
	xmlbranch	right = (xmlbranch) PG_GETARG_VARLENA_P(1);
	int			cmpRes = compareBranches(left, right);

	PG_RETURN_BOOL(cmpRes <= 0);
}

PG_FUNCTION_INFO_V1(xmlbranch_gt);

Datum
xmlbranch_gt(PG_FUNCTION_ARGS)
{
	xmlbranch	left = (xmlbranch) PG_GETARG_VARLENA_P(0);
	xmlbranch	right = (xmlbranch) PG_GETARG_VARLENA_P(1);
	int			cmpRes = compareBranches(left, right);

	PG_RETURN_BOOL(cmpRes > 0);
}


PG_FUNCTION_INFO_V1(xmlbranch_gte);

Datum
xmlbranch_gte(PG_FUNCTION_ARGS)
{
	xmlbranch	left = (xmlbranch) PG_GETARG_VARLENA_P(0);
	xmlbranch	right = (xmlbranch) PG_GETARG_VARLENA_P(1);
	int			cmpRes = compareBranches(left, right);

	PG_RETURN_BOOL(cmpRes >= 0);
}


PG_FUNCTION_INFO_V1(xmlbranch_compare);

Datum
xmlbranch_compare(PG_FUNCTION_ARGS)
{
	xmlbranch	left = (xmlbranch) PG_GETARG_VARLENA_P(0);
	xmlbranch	right = (xmlbranch) PG_GETARG_VARLENA_P(1);

	PG_RETURN_INT32(compareBranches(left, right));
}


PG_FUNCTION_INFO_V1(xmldoc_contains_branch);
/*
 * Return TRUE if XML document contains at least one occurrence
 * of the branch.
 */
Datum
xmldoc_contains_branch(PG_FUNCTION_ARGS)
{
	xmldoc		doc = (xmldoc) PG_GETARG_VARLENA_P(0);
	XMLCompNodeHdr docRoot = (XMLCompNodeHdr) XNODE_ROOT(doc);
	xmlbranch	branchRaw = (xmlbranch) PG_GETARG_VARLENA_P(1);
	XMLBranch	branch = (XMLBranch) VARDATA(branchRaw);
	XPath		locPath;
	XMLScanData scan;
	XMLNodeHdr	node;

	locPath = getLocationPathFromBranch(branch);
	initXMLScan(&scan, NULL, locPath, NULL, docRoot, doc, false);
	node = getNextXMLNode(&scan);
	finalizeXMLScan(&scan);
	pfree(locPath);
	PG_RETURN_BOOL(node != NULL);
}

PG_FUNCTION_INFO_V1(ginxmlextract);

Datum
ginxmlextract(PG_FUNCTION_ARGS)
{
	xmldoc		doc = PG_GETARG_VARLENA_P(0);
	int32	   *nkeys = (int32 *) PG_GETARG_POINTER(1);
	bool	  **nullFlags = (bool **) PG_GETARG_POINTER(2);
	XMLCompNodeHdr root = (XMLCompNodeHdr) XNODE_ROOT(doc);
	char	   *steps[XML_BRANCH_MAX_STEPS];
	unsigned int i;
	XMLNodeContainerData branchesCont;
	XNodeListItem *item;
	Datum	   *branches;
	int32		branchCount;
	bool	   *nulls;

	memset(steps, 0, XML_BRANCH_MAX_STEPS * sizeof(char *));
	xmlnodeContainerInit(&branchesCont);
	getAllBranches(root, &branchesCont, steps, 0, 1);
	branchCount = branchesCont.position;

	for (i = 0; i < XML_BRANCH_MAX_STEPS; i++)
	{
		char	   *step = steps[i];

		if (step == NULL)
			break;
		pfree(step);
	}

	branches = (Datum *) palloc(branchCount * sizeof(Datum));
	item = branchesCont.content;
	for (i = 0; i < branchCount; i++)
	{
		branches[i] = PointerGetDatum(item->value.singlePtr);
		item++;
	}
	xmlnodeContainerFree(&branchesCont);

	nulls = (bool *) palloc(branchCount * sizeof(bool));
	memset(nulls, FALSE, branchCount);
	*nullFlags = nulls;

	*nkeys = branchCount;
	PG_RETURN_POINTER(branches);
}


PG_FUNCTION_INFO_V1(ginqueryxmlextract);

Datum
ginqueryxmlextract(PG_FUNCTION_ARGS)
{
	xmlbranch	branch = PG_GETARG_VARLENA_P(0);
	int32	   *nkeys = (int32 *) PG_GETARG_POINTER(1);
	StrategyNumber strategy = PG_GETARG_UINT16(2);
	bool	  **nullFlags = (bool **) PG_GETARG_POINTER(5);
	Datum	   *branches = (Datum *) palloc(sizeof(Datum));
	bool	   *nulls = (bool *) palloc(sizeof(bool));

	if (strategy != GIN_STRATEGY_CONTAINS)
		elog(ERROR, "strategy %d not supported", strategy);

	branches[0] = PointerGetDatum(branch);
	nulls[0] = FALSE;
	*nkeys = 1;
	*nullFlags = nulls;
	PG_RETURN_POINTER(branches);
}


PG_FUNCTION_INFO_V1(ginxmlconsistent);

Datum
ginxmlconsistent(PG_FUNCTION_ARGS)
{
	bool	   *check = (bool *) PG_GETARG_POINTER(0);
	StrategyNumber strategy = PG_GETARG_UINT16(1);
	int32		nkeys = PG_GETARG_INT32(3);
	bool	   *recheck = (bool *) PG_GETARG_POINTER(5);
	bool	   *nullFlags = (bool *) PG_GETARG_POINTER(7);
	bool		result;

	if (strategy != GIN_STRATEGY_CONTAINS)
		elog(ERROR, "strategy %d not supported", strategy);
	if (nkeys != 1)
		elog(ERROR, "exactly one key entry expected");

	/*
	 * With our single-entry key the 'nullFlags' array should never contain
	 * TRUE. Check it anyway.
	 */
	result = check[0] && nullFlags[0] == false;

	/*
	 * As long as the single key entry exists in the document, the search
	 * condition must evaluate to TRUE.
	 */
	*recheck = false;
	PG_RETURN_BOOL(result);
}


/*
 * Returns -1, 0 or 1 if the left branch is lower than,
 * equal to or greater than the right branch respectively.
 */
static int
compareBranches(xmlbranch left, xmlbranch right)
{
	XMLBranch	leftHdr = (XMLBranch) VARDATA(left);
	XMLBranch	rightHdr = (XMLBranch) VARDATA(right);
	unsigned int depthCommon,
				i;
	char	   *stepLeft,
			   *stepRight;

	Assert(leftHdr->depth <= XML_BRANCH_MAX_STEPS);
	Assert(rightHdr->depth <= XML_BRANCH_MAX_STEPS);

	depthCommon = Min(leftHdr->depth, rightHdr->depth);
	stepLeft = (char *) leftHdr + sizeof(XMLBranchData);
	stepRight = (char *) rightHdr + sizeof(XMLBranchData);
	for (i = 0; i < depthCommon; i++)
	{
		int			cmpRes;

		cmpRes = strcmp(stepLeft, stepRight);
		if (cmpRes != 0)
			return cmpRes;

		stepLeft += strlen(stepLeft) + 1;
		stepRight += strlen(stepRight) + 1;
	}

	/* If the common part is identical, the longer branch wins. */
	if (leftHdr->depth == rightHdr->depth)
		return 0;

	if (leftHdr->depth < rightHdr->depth)
		return -1;
	else
		return 1;
}

static XPath
getLocationPathFromBranch(XMLBranch branch)
{
	char	   *branchStep;
	XPathElement pathSteps[XML_BRANCH_MAX_STEPS];
	unsigned int pathStepSizes[XML_BRANCH_MAX_STEPS];
	unsigned int i,
				resSizeMax = 0;
	XPath		locPath;
	char	   *target;

	Assert(branch->depth <= XML_BRANCH_MAX_STEPS);

	/*
	 * Convert the branch to a locaiton path so that standard search can be
	 * used.
	 */
	branchStep = (char *) branch + sizeof(XMLBranchData);
	for (i = 0; i < branch->depth; i++)
	{
		XPathElement pathStep;

		pathStepSizes[i] = sizeof(XPathElementData) + strlen(branchStep);
		pathStep = (XPathElement) palloc(pathStepSizes[i]);
		pathStep->hasPredicate = false;
		pathStep->descendant = false;
		pathStep->attrsAll = false;
		strcpy(pathStep->name, branchStep);
		branchStep += strlen(branchStep) + 1;
		pathSteps[i] = pathStep;

		/* No padding needed for the element (there is no predicate). */
		resSizeMax += pathStepSizes[i];
	}

	/*
	 * The padding is actually not that restrictive here (there are no
	 * predicate expressions), but it's not worth introducing a new (lower)
	 * constant.
	 */
	resSizeMax += MAX_PADDING(XPATH_ALIGNOF_LOC_PATH) + sizeof(XPathData);
	resSizeMax += (branch->depth - 1) * sizeof(XPathOffset);

	locPath = (XPath) palloc(resSizeMax);
	locPath->relative = false;
	locPath->depth = branch->depth;
	locPath->descendants = false;
	locPath->size = resSizeMax;
	locPath->targNdKind = XMLNODE_ELEMENT;
	locPath->allAttributes = false;
	locPath->piTestValue = false;

	target = (char *) &(locPath->elements[locPath->depth]);

	for (i = 0; i < branch->depth; i++)
	{
		/*
		 * Padding not forgotten, As long as there is no predicate expression,
		 * XPathElement really does not need it.
		 */
		memcpy(target, pathSteps[i], pathStepSizes[i]);
		pfree(pathSteps[i]);
		locPath->elements[i] = target - (char *) locPath;
		target += pathStepSizes[i];
	}

	return locPath;
}

/*
 * Examine the document tree recursively and get all branches out of it.
 */
static void
getAllBranches(XMLCompNodeHdr root, XMLNodeContainer result,
			   char **steps, unsigned int stepsSpace, unsigned int depth)
{
	XMLNodeIteratorData iterator;
	XMLNodeHdr	node;

	initXMLNodeIterator(&iterator, (XMLCompNodeHdr) root, false);
	while ((node = getNextXMLNodeChild(&iterator)) != NULL)
	{
		XMLCompNodeHdr element;
		unsigned int resSize;
		char	   *branchOutput,
				   *branchData;
		XMLBranch	branch;
		unsigned int i;

		if (node->kind != XMLNODE_ELEMENT)
			continue;

		element = (XMLCompNodeHdr) node;
		steps[depth - 1] = pstrdup(XNODE_ELEMENT_NAME(element));
		stepsSpace += strlen(steps[depth - 1]) + 1;
		resSize = VARHDRSZ + sizeof(XMLBranchData) + stepsSpace;

		/* Create a new branch for the current level. */
		branchOutput = (char *) palloc(resSize);
		branchData = VARDATA(branchOutput);
		branch = (XMLBranch) branchData;
		branch->depth = depth;
		branchData += sizeof(XMLBranchData);
		for (i = 0; i < depth; i++)
		{
			strcpy(branchData, steps[i]);
			branchData += strlen(steps[i]) + 1;
		}
		SET_VARSIZE(branchOutput, resSize);
		xmlnodePushSinglePtr(result, branchOutput);

		if (element->children > 0)
		{
			if (depth >= XML_BRANCH_MAX_STEPS)
				elog(ERROR, "failed to extract branch from document, maximum depth %u reached", XML_BRANCH_MAX_STEPS);

			/* Recurse to add all child branches. */
			getAllBranches(element, result, steps, stepsSpace, depth + 1);
		}
	}
}
