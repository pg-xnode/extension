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
	alignment = int,
	storage = plain
);


CREATE FUNCTION path_debug_print(@extschema@.path) RETURNS text
	as 'MODULE_PATHNAME', 'xpath_debug_print'
	LANGUAGE C
	STABLE
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


CREATE FUNCTION doc(node) RETURNS doc
	as 'MODULE_PATHNAME', 'xmlnode_to_xmldoc'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (node as doc)
	WITH FUNCTION doc(node);


CREATE FUNCTION node(pathval) RETURNS node 
	as 'MODULE_PATHNAME', 'xpathval_to_xmlnode'
	LANGUAGE C
	IMMUTABLE
	STRICT;

CREATE CAST (pathval as node)
	WITH FUNCTION node(pathval)
	AS ASSIGNMENT;


CREATE FUNCTION path(@extschema@.path, doc)
	RETURNS pathval 
	as 'MODULE_PATHNAME', 'xpath_single'
	LANGUAGE C
	VOLATILE
	STRICT;

CREATE FUNCTION @extschema@.path(@extschema@.path, @extschema@.path[], doc)
	RETURNS SETOF pathval[] 
	as 'MODULE_PATHNAME', 'xpath_array'
	LANGUAGE C
	VOLATILE
	STRICT;


CREATE DOMAIN @extschema@.add_mode AS CHAR(1)
	NOT NULL
	CHECK (VALUE ~ 'A|B|I|R|a|b|i|r');

CREATE FUNCTION add(doc, @extschema@.path, node, @extschema@.add_mode)
	RETURNS doc
	as 'MODULE_PATHNAME', 'xmlnode_add'
	LANGUAGE C
	VOLATILE
	STRICT;


CREATE FUNCTION remove(doc, @extschema@.path)
	RETURNS doc
	as 'MODULE_PATHNAME', 'xmlnode_remove'
	LANGUAGE C
	VOLATILE
	STRICT;



