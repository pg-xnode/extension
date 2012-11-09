set search_path to xml;

CREATE TABLE templates (
	id integer NOT NULL,
	template xnt
);

INSERT INTO templates VALUES 
-- The simplest case: only attribute values are parametrized
(0, '<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt"><point x="{$p1}" y="{$p2}"/></xnt:template>'),
-- In addition, parameter is used to generate the element name as well as attribute names
-- this template also shows that attributes can have different namespace prefixes
-- as long as they point to the same URI
(1, '<xnt:template
	xmlns:xnt="http://www.pg-xnode.org/xnt"
	xmlns:xnt_2="http://www.pg-xnode.org/xnt">
<xnt:element name="point_{$p1}">
	<xnt:attribute xnt:name="x_{$p1}" xnt_2:value="{$p2}"/>
	<xnt:attribute name="y_{$p2}" value="{$p3}"/>
</xnt:element>
</xnt:template>'),
-- Similar to the previous, except that the XNT namespace is the default one.
-- Also, the prefix may be different from 'xnt' (Notice that 't:value' becomes
-- just 'value' when printed out. The priter just does not bother to look
-- for the nearest declaration of 't:' as it does with elements.
-- Attribute must share the namespace with the owning element anyway.) 
(2, '<template
	xmlns="http://www.pg-xnode.org/xnt"
	preserve-space="false">
<element name="point_{$p1}">
	<attribute name="x_{$p1}" t:value="{$p2}" xmlns:t="http://www.pg-xnode.org/xnt"/>
	<u:attribute xmlns:u="http://www.pg-xnode.org/xnt" name="y_{$p2}" value="{$p3}"/>
</element>
</template>'),
-- node can also be used as parameter
(3, '<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt">
<area><xnt:copy-of expr="$p1"/></area>
</xnt:template>'),
(4, '<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt"><point x="{1}" y="{2}"/></xnt:template>');

SELECT id, template
FROM templates
ORDER BY id;

CREATE TABLE points (
	id integer NOT NULL,
	x float NOT NULL,
	y float NOT NULL
);

INSERT INTO points VALUES
(0, 0.0, 0.0), (1, 0.3, 5.4), (2, 3.15, 0.01);

CREATE TABLE point_nodes (
	id integer NOT NULL,
	p node NOT NULL
);

INSERT INTO point_nodes
SELECT p.id, xml.node(t.template, '{p1, p2}', ROW (x, y))
FROM templates t, points p
WHERE t.id=0;

SELECT *
FROM point_nodes
ORDER BY id;

SELECT xml.node(template, '{p1, p2, p3}', ROW (p.id, p.x, p.y))
FROM templates t, points p
WHERE t.id=1
ORDER BY p.id;

WITH fragments(f) AS (
	-- order the rows so that the fragment is never different from the expected test output.  
	SELECT xml.fragment(p ORDER BY id)
	FROM point_nodes
)
SELECT xml.node(t.template, '{p1}', ROW(f))
FROM fragments f,
templates t
WHERE t.id=3;

-- Is unary minus processed correctly when used with expression parameters?
select xml.node('<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt"><a i="{-$x}"/></xnt:template>', '{ x }', ROW( 3.14::float ));
select xml.node('<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt"><a i="{$x}"/></xnt:template>', '{ x }', ROW( -3.14::float ));
select xml.node('<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt"><a i="{-$x}"/></xnt:template>', '{ x }', ROW( -3.14::float ));

DROP TABLE templates;
DROP TABLE points;
DROP TABLE point_nodes;
