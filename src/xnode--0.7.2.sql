CREATE FUNCTION check_encoding() RETURNS boolean AS $$
DECLARE
	e varchar;
BEGIN
	SELECT pg_encoding_to_char(encoding) INTO e
	FROM pg_database
	WHERE datname=current_database();

	IF e <> 'UTF8' THEN
	RAISE EXCEPTION 'the pg_xnode extension can only be installed to UTF8-encoded databases';
	END IF;

	RETURN true;
END
$$ LANGUAGE plpgsql;

SELECT check_encoding();

DROP FUNCTION check_encoding();


CREATE FUNCTION node_in(cstring) RETURNS node
	as 'MODULE_PATHNAME', 'xmlnode_in'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION node_out(node) RETURNS cstring
	as 'MODULE_PATHNAME', 'xmlnode_out'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE TYPE node (
	internallength = variable,
	input = node_in,
	output = node_out,
	alignment = int,
	storage = extended 
);

CREATE FUNCTION node_kind(node) RETURNS text
	as 'MODULE_PATHNAME', 'xmlnode_kind'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION node_debug_print(node) RETURNS text
	as 'MODULE_PATHNAME', 'xmlnode_debug_print'
	LANGUAGE C
	IMMUTABLE
	STRICT;


CREATE FUNCTION doc_in(cstring) RETURNS doc
	as 'MODULE_PATHNAME', 'xmldoc_in'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION doc_out(doc) RETURNS cstring
	as 'MODULE_PATHNAME', 'xmldoc_out'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE TYPE doc (
	internallength = variable,
	input = doc_in,
	output = doc_out,
	alignment = int,
	storage = extended
);


CREATE FUNCTION @extschema@.path_in(cstring) RETURNS @extschema@.path
	as 'MODULE_PATHNAME', 'xpath_in'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION @extschema@.path_out(@extschema@.path) RETURNS cstring
	as 'MODULE_PATHNAME', 'xpath_out'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE TYPE @extschema@.path (
	internallength = variable,
	input = @extschema@.path_in,
	output = @extschema@.path_out,
	-- 8-aligned because float8 is used to represent numbers
	alignment = double,
	storage = plain
);


CREATE FUNCTION path_debug_print(@extschema@.path) RETURNS text
	as 'MODULE_PATHNAME', 'xpath_debug_print'
	LANGUAGE C
	IMMUTABLE	
	STRICT;


CREATE FUNCTION pathval_in(cstring) RETURNS pathval
	as 'MODULE_PATHNAME', 'xpathval_in'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION pathval_out(pathval) RETURNS cstring
	as 'MODULE_PATHNAME', 'xpathval_out'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE TYPE pathval (
	internallength = variable,
	input = pathval_in,
	output =pathval_out,
	alignment = double,
	storage = extended
);


CREATE FUNCTION node_to_doc(node) RETURNS doc
	as 'MODULE_PATHNAME', 'xmlnode_to_xmldoc'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (node as doc)
	WITH FUNCTION node_to_doc(node)
	AS IMPLICIT;


CREATE FUNCTION doc_to_node(doc) RETURNS node 
	as 'MODULE_PATHNAME', 'xmldoc_to_xmlnode'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (doc as node)
	WITH FUNCTION doc_to_node(doc)
	AS IMPLICIT;


CREATE FUNCTION pathval_to_bool(pathval) RETURNS bool 
	as 'MODULE_PATHNAME', 'xpathval_to_bool'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (pathval as bool)
	WITH FUNCTION pathval_to_bool(pathval)
	AS IMPLICIT;


CREATE FUNCTION pathval_to_float8(pathval) RETURNS float8 
	as 'MODULE_PATHNAME', 'xpathval_to_float8'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (pathval as float8)
	WITH FUNCTION pathval_to_float8(pathval)
	AS IMPLICIT;


CREATE FUNCTION pathval_to_numeric(pathval) RETURNS numeric 
	as 'MODULE_PATHNAME', 'xpathval_to_numeric'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (pathval as numeric)
	WITH FUNCTION pathval_to_numeric(pathval)
	AS IMPLICIT;


CREATE FUNCTION pathval_to_int4(pathval) RETURNS int4 
	as 'MODULE_PATHNAME', 'xpathval_to_int4'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (pathval as int4)
	WITH FUNCTION pathval_to_int4(pathval)
	AS IMPLICIT;


CREATE FUNCTION pathval_to_node(pathval) RETURNS node 
	as 'MODULE_PATHNAME', 'xpathval_to_xmlnode'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (pathval as node)
	WITH FUNCTION pathval_to_node(pathval)
	AS IMPLICIT;


CREATE FUNCTION path(@extschema@.path, doc)
	RETURNS pathval 
	as 'MODULE_PATHNAME', 'xpath_single'
	LANGUAGE C
	IMMUTABLE	
	STRICT;

CREATE FUNCTION @extschema@.path(@extschema@.path, @extschema@.path[], doc)
	RETURNS SETOF pathval[] 
	as 'MODULE_PATHNAME', 'xpath_array'
	LANGUAGE C
	IMMUTABLE	
	STRICT;

CREATE FUNCTION children(node)
	RETURNS node[] 
	as 'MODULE_PATHNAME', 'xmlnode_children'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION element(text, text[][2], node)
	RETURNS node 
	as 'MODULE_PATHNAME', 'xmlelement'
	LANGUAGE C
	IMMUTABLE;

CREATE FUNCTION fragment_sfunc(node, node) RETURNS node 
	as 'MODULE_PATHNAME', 'xmlfragment'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE AGGREGATE fragment(node) (
	SFUNC = fragment_sfunc,
	STYPE = node
);

CREATE DOMAIN add_mode AS CHAR(1)
	NOT NULL
	CHECK (VALUE ~ 'A|B|I|R|a|b|i|r');

CREATE FUNCTION add(doc, @extschema@.path, node, add_mode)
	RETURNS doc
	as 'MODULE_PATHNAME', 'xmlnode_add'
	LANGUAGE C
	IMMUTABLE
	STRICT;


CREATE FUNCTION remove(doc, @extschema@.path)
	RETURNS doc
	as 'MODULE_PATHNAME', 'xmlnode_remove'
	LANGUAGE C
	IMMUTABLE
	STRICT;


CREATE FUNCTION xnt_in(cstring) RETURNS xnt 
	as 'MODULE_PATHNAME', 'xnode_template_in'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION xnt_out(xnt) RETURNS cstring
	as 'MODULE_PATHNAME', 'xnode_template_out'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE TYPE xnt (
	internallength = variable,
	input = xnt_in,
	output = xnt_out,
	-- 8-aligned because it may contain XPath expressions
	alignment = double,
	storage = extended
);

CREATE FUNCTION node(xnt, text[], record) 
	RETURNS node
	as 'MODULE_PATHNAME', 'xnode_from_template'
	LANGUAGE C
	IMMUTABLE
	STRICT;



CREATE FUNCTION branch_in(cstring) RETURNS branch 
	as 'MODULE_PATHNAME', 'xmlbranch_in'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION branch_out(branch) RETURNS cstring
	as 'MODULE_PATHNAME', 'xmlbranch_out'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE TYPE branch (
	internallength = variable,
	input = branch_in,
	output = branch_out,
	alignment = int,
	storage = extended 
);


CREATE FUNCTION branch_eq(branch, branch) RETURNS bool 
	as 'MODULE_PATHNAME', 'xmlbranch_eq'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION branch_lt(branch, branch) RETURNS bool 
	as 'MODULE_PATHNAME', 'xmlbranch_lt'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION branch_lte(branch, branch) RETURNS bool 
	as 'MODULE_PATHNAME', 'xmlbranch_lte'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE FUNCTION branch_gt(branch, branch) RETURNS bool 
	as 'MODULE_PATHNAME', 'xmlbranch_gt'
	LANGUAGE C
	IMMUTABLE;

CREATE FUNCTION branch_gte(branch, branch) RETURNS bool 
	as 'MODULE_PATHNAME', 'xmlbranch_gte'
	LANGUAGE C
	IMMUTABLE;
	
CREATE FUNCTION branch_compare(branch, branch) RETURNS int4
	as 'MODULE_PATHNAME', 'xmlbranch_compare'
	LANGUAGE C
	IMMUTABLE
	STRICT;
	
CREATE FUNCTION doc_contains_branch(doc, branch) RETURNS bool
	as 'MODULE_PATHNAME', 'xmldoc_contains_branch'
	LANGUAGE C
	IMMUTABLE
	STRICT;
	
CREATE FUNCTION ginxmlextract(doc, internal, internal) RETURNS internal
	as 'MODULE_PATHNAME', 'ginxmlextract'
	LANGUAGE C
	IMMUTABLE
	STRICT;
	
CREATE FUNCTION ginqueryxmlextract(branch, internal, int2, internal, internal, internal, internal) RETURNS internal
	as 'MODULE_PATHNAME', 'ginqueryxmlextract'
	LANGUAGE C
	IMMUTABLE
	STRICT;
	
CREATE FUNCTION ginxmlconsistent(internal, int2, branch, int4, internal, internal, internal, internal) RETURNS internal
	as 'MODULE_PATHNAME', 'ginxmlconsistent'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE OPERATOR = (LEFTARG=branch, RIGHTARG=branch, PROCEDURE=branch_eq);
CREATE OPERATOR < (LEFTARG=branch, RIGHTARG=branch, PROCEDURE=branch_lt);
CREATE OPERATOR <= (LEFTARG=branch, RIGHTARG=branch, PROCEDURE=branch_lte);
CREATE OPERATOR > (LEFTARG=branch, RIGHTARG=branch, PROCEDURE=branch_gt);
CREATE OPERATOR >= (LEFTARG=branch, RIGHTARG=branch, PROCEDURE=branch_gte);
CREATE OPERATOR @> (LEFTARG=doc, RIGHTARG=branch, PROCEDURE=doc_contains_branch);


CREATE OPERATOR FAMILY xml_ops USING btree;

CREATE OPERATOR CLASS branch_ops DEFAULT FOR TYPE branch USING btree FAMILY xml_ops
	AS
	OPERATOR 1 < (branch, branch) FOR SEARCH,
	OPERATOR 2 <= (branch, branch) FOR SEARCH,
	OPERATOR 3 = (branch, branch) FOR SEARCH,
	OPERATOR 4 >= (branch, branch) FOR SEARCH,
	OPERATOR 5 > (branch, branch) FOR SEARCH,
	FUNCTION 1 branch_compare (branch, branch),
	STORAGE branch;


CREATE OPERATOR FAMILY xml_ops USING gin;

-- So far we only have cross-type operators, so the default class is this one.
-- It could change as soon as single-type operators have been introduced
-- for the XML documents.
CREATE OPERATOR CLASS branch_ops DEFAULT FOR TYPE doc USING gin FAMILY xml_ops
	AS
	OPERATOR 2 @> (doc, branch) FOR SEARCH,
	FUNCTION 1 branch_compare (branch, branch),
	FUNCTION 2 ginxmlextract(doc, internal, internal),
	FUNCTION 3 ginqueryxmlextract(branch, internal, int2, internal, internal, internal, internal),
	FUNCTION 4 ginxmlconsistent(internal, int2, branch, int4, internal, internal, internal, internal),
	STORAGE branch;


