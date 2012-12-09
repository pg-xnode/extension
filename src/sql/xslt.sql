set search_path to xml;

CREATE TABLE templates (
	id integer NOT NULL,
	template xsl 
);


INSERT INTO templates VALUES
(0, '<?xml version="1.0"?>
	<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	
<xsl:template match="/root/a">
	<x/>
</xsl:template>
</xsl:stylesheet>');

CREATE TABLE documents (
	id integer NOT NULL,
	doc doc
);

INSERT INTO documents VALUES
(0, '<root><a/></root>');

SELECT xml.transform(t.template, d.doc)
FROM templates t, documents d
WHERE (t.id, d.id)=(0, 0);
