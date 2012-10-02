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
	alignment = int,
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


