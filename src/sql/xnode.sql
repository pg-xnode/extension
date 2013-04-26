CREATE EXTENSION xnode;

-- This is rather a generic set of tests.

SET search_path TO xml;

CREATE TABLE STATES ( 
	id int not null,
	data doc not null
);

CREATE TABLE PATHS (
	id int,
	name varchar(32),
	path xml.path not null
);

CREATE TABLE DOCS_OTHER (
	id int,
	data doc not null
);

-- Random geografic data
-- Czech and German languages are both good to test handling of multi-byte characters

INSERT INTO STATES VALUES(1,  '<?xml version="1.0" encoding="UTF-8"?>
	<state name="Česká republika" area="78866" population="10674947" tld="cz">
	<!-- Resource: http://en.wikipedia.org/wiki/Czech_Republic -->
	<city name="Praha" capital="true"/>
	<city name="Plzeň"><part name="Doubravka"/></city>
	<city name="Čerčany"/>
	<region name="Šumava"><destination name="Lipno"><![CDATA[http://en.wikipedia.org/wiki/Lipno_Dam]]></destination></region>
	<languages><language>Cestina</language></languages>
</state>');

INSERT INTO STATES VALUES(2,  '<?xml version="1.0" encoding="UTF-8"?><state name="Bundesrepublik Deutschland" area="357021" population="81757600" tld="de">
	<!-- Resource: http://en.wikipedia.org/wiki/Deutschland -->
	<city name="Berlin" capital="true"/>
	<region name="Baden-Württemberg"><city name="Stuttgart" capital="true"/></region>
	<region name="Bayern"><city name="Munchen" capital="true"/></region>
	<region name="Thüringen"><city name="Erfurt" capital="true"/></region>
	<languages><language official="true">Deutsch</language></languages>
</state>');

INSERT INTO STATES VALUES(3,  '<?xml version="1.0" encoding="UTF-8" ?><state name="Österreich" area="83872" population="8356707" tld="at">
	<!-- Resource: http://en.wikipedia.org/wiki/Oesterreich -->
	<city name="Wien" capital="true"/>
	<region name="Tirol"><city name="Innsbruck" capital="true"/></region>
	<region name="Steiermark"><city name="Graz" capital="true"/></region>
	<region name="Vorarlberg"><city name="Bregenz" capital="true"/></region>
	<languages><language official="true">Deutsch</language></languages>
</state>');

update states set data=xml.add(data, '/state/languages', '<region name="Brandenburg"><city name="Potsdam" capital="true"/></region>', 'b') where id=2;
update states set data=add(data, '/state/languages', '<region name="Saarland" area="2569" population="1056000"/>', 'b') where id=2;
update states set data=add(data, '/state/region[@name="Saarland"]', '<city name="Saarbrücken" capital="true"/>', 'i') where id=2;
update states set data=add(data, '/state/region[@name="Saarland"]', '<city name="Ottweiler"/>', 'i') where id=2;
update states set data=add(data, '/state/region[@name="Saarland"]/city[@name="Saarbrücken"]', '<city name="Neunkirchen"/>', 'a') where id=2;


-- Simply dump the XML documents
select *
from states
order by id;


-- Query some XML paths 
select path('/state/@name', data) as state, path('/state/@tld', data) as domain,
path('/state/languages/language/text()', data) as language
from states
order by id;

select id, path('count(//city)', data)
from states
order by id;


with paths(name, area, population)
as (select '/state/@name'::xml.path, '/state/@area'::xml.path, '/state/@population'::xml.path)
select  id, path(p.name, data) as name, path(p.area, data) as area,
path(p.population, data) as population
from states s, paths p
order by s.id;

-- The XML path expressions can also be parsed (preprocessed) and stored into a table for later use...
insert into paths (name, path) values ('names', '/state/@name'), ('capitals', '/state/city[@capital="true"]/@name'), ('regions', '/state/region'), ('region_capitals', '/region/city[@capital="true"]/@name');

-- ... and used
select path(pn.path, s.data) as name, path(pc.path, s.data) as capital
from states s
join paths pn on (pn.name='names') 
join paths pc on (pc.name='capitals')
order by s.id;


select s.id, path(p.path, data)
from states s
join paths p on (p.name='regions')
order by s.id;


-- Convert data to table-like form
select id, path('/state', '{"region/city", "@area", "@population", "@tld"}', data) as state_info
from states
order by id;

-- Replace node
update states set data=add(data, '/state/region[@name="Šumava"]/destination', '<destination name="Boubín"/>', 'r') where id=1;

-- Add document fragment
-- before:
update states set data=add(data, '/state/languages', '<city name="České Budějovice" population="95071"/><city name="Kroměříž"/>', 'b') where id=1;
-- after:
 update states set data=add(data, '/state/city[@name="Praha"]', '<city name="Uherské Hradiště"/><city name="Františkovy Lázně"/>', 'a') where id=1;
-- into:
update states set data=add(data, '/state/city[@name="Praha"]', '<city name="Žižkov"/><city name="Břevnov"/>', 'i') where id=1;
-- into empty element (special case):
update states set data=add(data, '/state', '<people/>', 'i') where id=1;
update states set data=add(data, '/state/people', '<person first_name="Antonín" last_name="Dvořák"/><person first_name="Václav" last_name="Havel"/>', 'i') where id=1;
-- replace:
update states set data=add(data, '/state/languages/language', '<language official="true">Čeština</language><language>Hantec</language>', 'r') where id=1;

select data
from states
where id=1
order by id;

-- remove node:
update states set data=remove(data, '/state/city[@name="Kroměříž"]') where id=1;
-- ... and test if node will be correctly displayed as an empty element when the only child is removed:
update states set data=remove(data, '/state/city[@name="Plzeň"]/part[@name="Doubravka"]') where id=1;

select data
from states
where id=1
order by id;

-- remove() - repeated occurrence of the target path:
update states set data=remove(data, '/state/text()');

select data
from states
order by id;


-- Additional tests

-- add() - repeated occurrence of the target path:
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x>', 'b');
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x>', 'a');
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x>', 'r');
-- insert into both empty and non-empty element
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x>', 'i');
select add('<a><b><d/></b><b><d/></b></a>', '/a/b', '<x><y/></x>', 'i');
select add('<a><b id="1"/><b id="2"/></a>', '/a/b', '<x><y/></x>', 'i');
-- and the same for document fragment:
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x><!--no comment-->', 'b');
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x><!--no comment-->', 'a');
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x><!--no comment-->', 'r');
select add('<a><b/><b/></a>', '/a/b', '<x><y/></x><!--no comment-->', 'i');
select add('<a><b><d/></b><b><d/></b></a>', '/a/b', '<x><y/></x><!--no comment-->', 'i');
select add('<a><b id="1"/><b id="2"/></a>', '/a/b', '<x><y/></x><!--no comment-->', 'i');

-- remove() - repeated occurrence of the target path:
-- remove attribute
select remove('<a><b id="1" d="1"/><b id="2" d="2"/></a>', '/a/b/@id');
-- remove the last attribute
select remove('<a><b id="1"/><b id="2"/></a>', '/a/b/@id');
-- remove the last nested node(s)
select remove('<a><b/><b/></a>', '/a/b');

-- DOM
select xml.children('<root><a><!--cmnt--></a><b><?abc def?></b><c/></root>');

select xml.element('root', '{{"i", "x"}, {"j", 1}, {"k", ""}}', '<x/><y/>');
select xml.element('root', '{{"i", "x"}, {"j", 1}}', '<x k="u"><y l="v"/></x>');
select xml.element('root', NULL, '<x k="u"><y l="v"/></x>');
select xml.element('root', '{{"i", "x"}, {"j", 1}}', NULL);

-- xml.fragment()
WITH tmp(n) as (VALUES ('<a i="1"/>'), ('<!--c-->'), ('<c><d j="2"/></c>'))
SELECT xml.fragment(n::xml.node)
FROM tmp;
