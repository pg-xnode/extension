set search_path to xml;

CREATE TABLE templates (
	id integer NOT NULL,
	template xnt
);

INSERT INTO templates VALUES 
-- the simplest case: only attribute values are parametrized
(0, '<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt"><point x="{$p1}" y="{$p2}"/></xnt:template>'),
-- in addition, parameter is used to generate the element name as well as attribute names
(1, '<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt">
<xnt:element name="point_{$p1}">
	<xnt:attribute name="x_{$p1}" value="{$p2}"/>
	<xnt:attribute name="y_{$p2}" value="{$p3}"/>
</xnt:element>
</xnt:template>'),
-- node can also be used as parameter
(2, '<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt">
<area><xnt:copy-of expr="$p1"/></area>
</xnt:template>'),
(3, '<xnt:template xmlns:xnt="http://www.pg-xnode.org/xnt"><point x="{1}" y="{2}"/></xnt:template>');

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
WHERE t.id=2;

DROP TABLE templates;
DROP TABLE points;
DROP TABLE point_nodes;
