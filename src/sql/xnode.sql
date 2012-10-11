CREATE EXTENSION xnode;
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

-- test the XPath parser
delete from paths;

insert into paths (id, path) values (0, '/c[@cd != @e != "a" and @b and @c or @f]/@g'),
(1, '/a[@a or @cd != @e != "a" and @b=@k=@l or @s]/b/text()'),
(2, '/state[@tld or "cz" = "cz" and @tld and @a or @b]'),
(3, '/state[@tld="de" or @area="78866" and @population="1067494"]'),
(4, '/a[(@b  != "a" and @k or @l)]/b/text()'),
(5, '/a[(@b  != "a" and (@k or @l))]/b/text()'),
(6, '/a[(( @cd and @e)  != "a" and @k)]/b[(@a > @c<=@g=@h)=i and @k=l]/text()'),
(7, '/a[@a or ((@e)  != "a" = @k or @l)]/b/text()'),
-- ... don't forget to use (predicate) subpaths
(8, '/a[ab/cd/@a!=b=c/a[@a=@b=a[@c and (@b or @c)]] = @a]'),
-- check if 'npaths' is propagated from subexpressions (both explicit and implicit)
(9, '/a[@a and @b or (@c=a[@e=@f])]'),
(10, '/a[@a and @b or (@c=a[@e=f])]'),
(11, '/a[@a and @b or @c=a[@e=@f]]'),
(12, '/a[@a and @b or @c=a[@e=f]]'),
-- Absolute sub-path
(13, '//root[/a and //b or c]'),
-- Functions
(14, '/root/a[position()=position()]'),
(15, '/root/b[count(a)>=count(c)]'),
(16, '/root[contains("", a)]');

select id, path
from paths
order by id asc;

select id, path_debug_print(path)
from paths
order by id asc;

-- accidental match of attribute name and element name (bug found during development)
select xml.path('/root/@b', '<root><b>3</b></root>');
select xml.path('/root/@b', '<root b="1"><b>3</b></root>');

-- some non-trivial XPath predicates
select path('/state[@tld="at" or @area="78866"]/@name', data) from states;
select path('/state[@tld="de" or @area="78866" and @population="0"]/@tld', data) from states;
select path('/state[@tld="de" or @area="78866" and @population="10674947"]/@tld', data) from states;
select path('/doc/a[(@b="1" or @b="2") and @c="3"]', '<doc><a b="1" c="2"/><a b="2" c="3"/></doc>');


-- MB characters in XPath expression
with paths(id, path) as (
values
((0), ('/dům/střecha'::xml.path)),
((1), ('/dům/poschodí[@číslo="2"]'::xml.path)),
((2), ('/dům/poschodí[@cislo="1"]'::xml.path)),
((3), ('/dům/přízemí'::xml.path))
)
select id, path(path, '<dům><střecha/><poschodí číslo="2"/><poschodí číslo="1"/><poschodi/><přízemí/><sklep/></dům>')
from paths
order by id;


-- subpaths

insert into docs_other(id, data)
values (1, '<root b="xxyy"><a>x<c/>y</a><b>x<c/>y</b></root>'), 
(2, '<root b="xxyy"><a>xx<c/>yy</a><b>x<c/></b></root>'),
(3, '<root><a i="1">x<c/>y1<d>z1<e/>z2</d>y2</a><b>x<c/>y</b><b i="1"></b></root>');

select id, path('/root[a!=@b]', data)
from docs_other
order by id;

select id, path('/root[a=@b]', data)
from docs_other
order by id;

select id, path('/root[b="xy"]', data)
from docs_other
order by id;

-- check that descendant text nodes are concantenated and compared correctly
select id, path('/root[a="xy1z1z2y2"]', data)
from docs_other
order by id;

select id, path('/root[a="xy1z1z2y"]', data)
from docs_other
order by id;

select id, path('/root[a="xy1z1z2y23"]', data)
from docs_other
order by id;

-- compare non-empty node set to zero-length literal

select id, path('/root[a=""]', data)
from docs_other
order by id;

select id, path('/root[a!=""]', data)
from docs_other
order by id;

-- compare non-empty node set, having no text nodes, to zero-length literal

select id, path('/root[b=""]', data)
from docs_other
order by id;

select id, path('/root[b!=""]', data)
from docs_other
order by id;

-- compare empty node set to zero-length literal

select id, path('/root[f=""]', data)
from docs_other
order by id;

select id, path('/root[f!=""]', data)
from docs_other
order by id;

-- compare non-empty node, having no text nodes, set to non-zero-length literal

select id, path('/root[b="xy1z1z2y2"]', data)
from docs_other
order by id;

select id, path('/root[b!="xy1z1z2y2"]', data)
from docs_other
order by id;

-- compare empty node set to non-zero-length literal

select id, path('/root[f="xy1z1z2y2"]', data)
from docs_other
order by id;

select id, path('/root[f!="xy1z1z2y2"]', data)
from docs_other
order by id;

-- compare non-empty node set to null value (attribute that doesn't exist)

select id, path('/root[a=@t]', data)
from docs_other
order by id;

select id, path('/root[a!=@t]', data)
from docs_other
order by id;

-- compare non-empty node set, having no text nodes, to null value

select id, path('/root[b=@t]', data)
from docs_other
order by id;

select id, path('/root[b!=@t]', data)
from docs_other
order by id;

-- compare empty node set to null value

select id, path('/root[f=@t]', data)
from docs_other
order by id;

select id, path('/root[f!=@t]', data)
from docs_other
order by id;

-- compare string to a set of text nodes (only sets of elements tested so far)

select path('/root[@i=a/text()]', '<root i="x"><a>y</a></root>');
select path('/root[@i!=a/text()]', '<root i="x"><a>x</a></root>');
select path('/root[@i=a/text()]', '<root i="x"><a>x</a><a>y</a></root>');
select path('/root[@i!=a/text()]', '<root i="x"><a>x</a><a>y</a></root>');

-- compare 2 node sets
select path('/root[a=b]', '<root><a i="1"/><b i="1"/></root>');
select path('/root[a=b]', '<root><a i="1">x1<c/>x2</a><b i="1">x1<c/>x2</b></root>');
select path('/root[a=b]', '<root><a i="1">x1<c>y1<d>z1</d>y3</c>x2</a><b i="1">x1y1<c/>z1y3x2</b></root>');
select path('/root[a=b]', '<root><a i="1">x1<c>y1<d>z1</d>y3</c>x2x3</a><b i="1">x1y1<c/>z1y3x2</b></root>');
select path('/root[a=b]', '<root><a i="1">x1</a><a i="2">x2</a><b i="1">x2</b></root>');
select path('/root[a!=b]', '<root><a i="1">x1</a><a i="2">x2</a><b i="1">x2</b></root>');
-- non-existing subpaths
select path('/root[a=a/b]', '<root i="x"><a>y</a></root>');
select path('/root[a!=a/b]', '<root i="x"><a>y</a></root>');

-- again, test sets of other than element nodes
select path('/root[a=a/@a]', '<root i="x"><a a="y">y</a></root>');
select path('/root[a!=a/@a]', '<root i="x"><a a="y">y</a></root>');
select path('/root[a!=a/@*]', '<root><a a="x" b="y">x</a></root>');
select path('/root[a/@*=a]', '<root><a a="x" b="y">x</a></root>');
select path('/root[a=comment()]', '<root><a>x</a><!--x--></root>');
select path('/root[a!=comment()]', '<root><a>x</a><!--x--></root>');
select path('/root[a=b/node()]', '<root><a>x</a><b><![CDATA[y]]></b></root>');
select path('/root[a!=b/node()]', '<root><a>x</a><b><![CDATA[y]]></b></root>');
select path('/root[a=b/processing-instruction("abc")]', '<root><a>def</a><b><?abc def?></b></root>');
select path('/root[a!=b/processing-instruction("abc")]', '<root><a>def</a><b><?abc def?></b></root>');

-- sets where non-element is on both sides
select path('/root[node()=node()]', '<root><![CDATA[x]]></root>');
select path('/root[node()!=node()]', '<root><!--x--></root>');

-- attributes having numeric values
select path('/root[@b=1.5 and @c=1.1000000]', '<root b="1.50" c="1.1"/>');
select path('/root[@b!=1.5]', '<root b="1.50" c="1.1"/>');

-- Compare number to a node-set. First, set of elements...
select path('/root[b=0.1]', '<root><b> 0.<c/>1<d/> </b></root>');
select path('/root[b!=0.1]', '<root><b> 0.<c/>1<d/> </b></root>');
-- ... and then also that of another type:
select path('/root[comment()=1.551]', '<root><!-- 1.5510 --></root>');
select path('/root[comment()=1.551]', '<root><!----></root>');
select path('/root[comment()!=1.551]', '<root><!----></root>');

-- Operators '<' and '>'
select path('/root[@b>1.01]', '<root b="1.1"/>');
select path('/root[@b<1.01]', '<root b="1.1"/>');
select path('/root[1.01<@b]', '<root b="1.1"/>');
select path('/root[1.01>@b]', '<root b="1.1"/>');
select path('/root[b<1]', '<root><b>1.5</b></root>');
select path('/root[b>1]', '<root><b>1.5</b></root>');
select path('/root[1<b]', '<root><b>1.5</b></root>');
select path('/root[1>b]', '<root><b>1.5</b></root>');
select path('/a/@j=1', '<a i="0.1 " j="0.1"/>');
select path('/a/@j<1', '<a i="0.1 " j=""/>');
select path('/a/@j!=1', '<a i="0.1 " j=""/>');
select xml.path('/=/a', '<a/>');
select xml.path('/=/b', '<a/>');
select xml.path('/=/', '<a/>');
select xml.path('/=/b', '<a/>');
select xml.path('/!=/b', '<a/>');

-- No match if one operand can't be cast to a number:
select path('/root[@b>1.1]', '<root b="1.o1"><b/></root>');
select path('/root[@b<1.1]', '<root b="1.o1"><b/></root>');
select path('/root[@b<"a"]', '<root b="1.1"><b/></root>');
select path('/root[@b>"a"]', '<root b="1.1"><b/></root>');
select path('/root[@b>b]', '<root b="abc"><b>def</b></root>');
select path('/root[@b<b]', '<root b="abc"><b>def</b></root>');

-- Operator '|' (union)
-- First, check that operator output type is correctly set:
select xml.path_debug_print('/a[@i > @j]|/b or /c');
select xml.path_debug_print('(/a[@i > @j]|/b) or /c');
select xml.path_debug_print('((/a[@i > @j]|/b) or /c)');
-- Does the union operator eliminate duplicate nodes?
select xml.path('/root/a|/root/a', '<root><a/></root>');
select xml.path('/root/a|/root/a|/root', '<root><a/></root>');

-- Operators '+' and '-'
-- Again, check result type
select xml.path_debug_print('/root[@i + @j > 0]');
select xml.path_debug_print('/root[(@i + @j) > 0]');
-- Simple examples
select xml.path('/root/@i + /root/@j', '<root i="1" j="2"/>');
select xml.path('/root/@i - /root/@j', '<root i="1" j="2"/>');
select xml.path('boolean(/root/@i + /root/@j)', '<root i="1" j="2"/>');
select xml.path('string(/root/@i + /root/@j)', '<root i="1" j="2"/>');
select xml.path('/root[/root/@i + /root/@j]', '<root i="1" j="2"/>');
select xml.path('/root[(/root/@j - /root/@i)]', '<root i="1" j="2"/>');
select xml.path('/root[/root/@j - /root/@i]', '<root i="1" j="2"/>');
-- Involve null values 
select xml.path('/root/@i + /root/@k', '<root i="1" j="2"/>');
select xml.path('/root/@i - /root/@k', '<root i="1" j="2"/>');
select xml.path('(/root/@j + /root/@k) = (/root/@j + /root/@k)', '<root i="1" j="2"/>');
select xml.path('(/root/@j + /root/@k) != (/root/@j + /root/@k)', '<root i="1" j="2"/>');
select xml.path('(/root/@j + /root/@k) = 2', '<root i="1" j="2"/>');
select xml.path('(/root/@j + /root/@k) != 2', '<root i="1" j="2"/>');
select xml.path('/root/@j + /root/@k', '<root i="1" j="2"/>');
select xml.path('(/root/@j + /root/@k)', '<root i="1" j="2"/>');
select xml.path('boolean(/root/@j + /root/@k)', '<root i="1" j="2"/>');
select xml.path('string(/root/@j + /root/@k)', '<root i="1" j="2"/>');
select xml.path('/root[/root/@j + /root/@k]', '<root i="1" j="2"/>');
select xml.path('/root[(/root/@j + /root/@k)]', '<root i="1" j="2"/>');
select xml.path('boolean(1+/root)', '<root/>');
select xml.path('string(1+/root)', '<root/>');
select xml.path('boolean(1+/root)', '<root>1</root>');
select xml.path('string(1+/root)', '<root>1</root>');
-- null values where short evaluation is expected
select xml.path('boolean(false() or /root/@j + /root/@k or false())', '<root i="1" j="2"/>');
select xml.path('boolean(true() and /root/@j + /root/@k and true())', '<root i="1" j="2"/>');

-- Multiply operator
select xml.path_debug_print('/a/@i*/a/@j');
select xml.path_debug_print('/a/@**/a/@j');
select xml.path('/a/@i*/a/@j', '<a i="3" j="2"/>');
select xml.path('/a/@**/a/@j', '<a i="3" j="2"/>');

-- Unary operator
select xml.path('boolean(-/root)', '<root/>');
select xml.path('boolean((-/root))', '<root/>');
select xml.path('string(-/root)', '<root/>');
select xml.path('string((-/root))', '<root/>');
select xml.path('-/root<3', '<root>3</root>');
select xml.path('-/root+-2', '<root>3</root>');
select xml.path('/root + - boolean(/root)', '<root>3</root>');
select xml.path('/root/@i + - 1 + 2', '<root i="3"/>'); 
select xml.path('/root/@i + - (1 + 2)', '<root i="3"/>');
select xml.path_debug_print('/root + - boolean(/root)');
select xml.path_debug_print('/root/@i + - 1 + 2'); 
select xml.path_debug_print('/root/@i + - (1 + 2)');
--Special cases where the '-' has to be propagated to top-level exression
select xml.path('-(-(-(1)))', '<a/>');
select xml.path('-(-(-1))', '<a/>');
select xml.path('-(-(1))', '<a/>');
select xml.path('-(-1)', '<a/>');
select xml.path('-(1)', '<a/>');
select xml.path('-1', '<a/>');
select xml.path('-(-(1 - 2))', '<a/>');
--..similar, with functions instead of subexpressions
select xml.path('-((-(1 - 2) + -count(/a)))', '<a/>');
select xml.path('-((-(1 - 2) + -count(/a)))', '<a><b/><b/></a>');
select xml.path('-((-(1 - 2) + -count(/a/b)))', '<a><b/><b/></a>');
select xml.path('-((-(1 - 2) + (-count(/a/b))))', '<a><b/><b/></a>');
select xml.path('-((-(1 - 2) + -(-count(/a/b))))', '<a><b/><b/></a>');
select xml.path('-((-(1 - 2) + -(count(/a/b))))', '<a><b/><b/></a>');
--...and some where node test predicate is the top-level expression
select xml.path('/root/a[-((-1 + -count(/root)))]', '<root><a/></root>');
select xml.path('/root/a[-((-(1) + -count(/root/a)))]', '<root><a/></root>');
select xml.path('/root/a[-((-1 + -count(/root/a)))]', '<root><a/><a/></root>');
select xml.path('/root/a[-((-(1) + (-count(/root/a))))]', '<root><a/><a/></root>');
select xml.path('/root/a[-((-1 + -(-count(/root/a))))]', '<root><a></a></root>');
select xml.path('/root/a[-((-(1) + -(count(/root/a))))]', '<root><a/><a/></root>');

-- The XPath predicate can be used even if the element has no children/attributes
select path('/root["a"]', '<root/>');
select path('/root[@a or a]', '<root/>');

-- Absolute sub-path
select path('/root[/root/a=x]', '<root><a>c</a><x>c</x></root>');
select path('/root[/root/a=/x]', '<root><a>c</a><x>c</x></root>');
select path('/root[//a=x]', '<root><a>c</a><x>c</x></root>');
-- Special case: the sub-path represents the whole document:
select path('/root[/=/]', '<root/>');
select path('/root[/!=/]', '<root/>');
select path('/root[/=1]', '<root/>');
select path('/root[/>1]', '<root/>');
select path('/root[/=a]', '<root><a/></root>');
select path('/root[/!=a]', '<root><a/></root>');

-- position() function (implicit)
select path('/root/a[position()=@i]', '<root><a i="2"/><a i="1"/><a i="3.0"/></root>');
select path('/root/a[position()=1 or position()=2]', '<root><a i="1"/><a i="2"/></root>'); 
select path('/state/city[6]/@name', data) from states where id=1;
select path('/state/region[5]/city[1]/@name', data) from states where id=2;
select path('/root//a[position()=3]', '<root><a i="1"/><a i="2"><a i="3">xy</a></a><a i="4"/></root>');

select id, path('/state/comment()', data)
from states
order by id;

-- node() test: verify that only non-attribute child nodes are returned
select path('/a/node()', '<a i="1" j="2"><b/><c/><d i="1"/><e j="2"/><!--no comment--></a>');

-- all attributes but no other nodes
select path('/a/@*', '<a i="1"><b/><c/><d i="4"/><e j="5"/></a>');

-- Some more exercises for '@*'
select xml.path('/root[count(@*)]', '<root/>');
select xml.path('/root[count(@*)]', '<root k="2"/>');
select xml.path('/root[@*=@*]', '<root k="2" j="3"/>');
select xml.path('/root[@*!=@*]', '<root k="2" j="3"/>');
select xml.path('/root[@*=@*]', '<root k="2"/>');
select xml.path('/root[@*!=@*]', '<root k="2"/>');
select xml.path('/root[@*=@*]', '<root/>');
select xml.path('/root[@*!=@*]', '<root/>');
select xml.path('/@i', '<root i="1"/>');
select xml.path('/@*', '<root i="1"/>');
select xml.path('/root', '{"@*"}', '<root i="1"/>');
select xml.path('//@id', '<a><b id="1" d="1"/></a>');
-- Related case: if the attribute operand is wrapped in (sub)expression, that expression's value type must be node set
select xml.path_debug_print('count((@*))');



-- descendants
select path('/a//b', '<a><b/><x><b i="3"/><a><b i="2"/></a></x></a>');
select path('//x/b', '<root><x><b><test1/><x><b><test2/></b></x></b><a><b><test3/></b><d/></a></x></root>');
-- special case: with 2 location steps searching for descendants, some nodes may be found multiple times (by various combinations
-- of scan and its sub-scan). In this case that would happen for '<b><test2/></b>' if uniqueness wasn't enforced:
select path('//x//b', '<root><x><b><test1/><x><b><test2/></b></x></b><a><b><test3/></b><d/></a></x></root>');

-- Functions

select path('/root/b[count(a)>1]', '<root><a i="1"><b/></a><a i="2"><b/><b/></a></root>');
select path('/root/b[count(c)=0]', '<root><a i="1"><b/></a><a i="2"><b/><b/></a></root>');


select path('/root[contains(a, "x")]', '<root><a>xy</a><a>z</a></root>');
select path('/root[contains(a, "z")]', '<root><a>xy</a><a>z</a></root>');
select path('/root[contains(a, "")]', '<root><a>xy</a><a>z</a></root>');
select path('/root[contains("", a)]', '<root><a>xy</a><a>z</a></root>');
select path('/root[contains("", a)]', '<root><a>xy</a><a>z</a></root>');
select path('/root[contains(b, a)]', '<root><a>xy</a><a>z</a></root>');
select path('/root[contains(a, b)]', '<root><a>xy</a><a>z</a></root>');
select path('/root[contains(b, c)]', '<root><a>xy</a><a>z</a></root>');
select path('contains("c", /root/a)', '<root><a/><a/></root>');
select path('/state[contains(@name, "rep") and (@area<=100000 or @population>11000000)]', '{@name, @area, @population}', data) 
from states
order by id;

select id, path('count(/state/region)', data)
from states
order by id;

select path('sum(/)', '<a>1<b>2</b><b>3<c/>1 </b></a>');
select path('sum(/a)', '<a>1<b>2</b><b>3<c/>1 </b></a>');
select path('sum(/a/b)', '<a>1<b>2</b><b>3<c/>1 </b></a>');
select path('sum(/a/b)', '<a>1<b>2</b><b>3<c/> 1 </b></a>');
select path('sum(//@*)', '<a i="3.5" j="0.4">1<b i=".1"/></a>');
-- Special case: the node-set is empty
select path('sum(/a/x)', '<a i="3.5" j="0.4">1<b i=".1"/></a>');

select path('/root//a[position()=last()]', '<root><a i="3"/><a i="4"/><b><d/><a i="1"/><a i="2"/><c/><a i="5"/><d/></b><a/></root>');
select path('/root//a[position()=last() and @i]', '<root><a i="3"/><a i="4"/><b><a i="1"/><a i="2"/><c/><a i="5"/><d/></b><a/></root>');

select xml.path('/root[concat("a", /root, "c", /root)="axcx"]', '<root>x</root>');
select xml.path('/root[concat("a", /root, "c")="axcx"]', '<root>x</root>');

select xml.path('name(/)', '<a/>');
select xml.path('name(/a/b)', '<a i="1"><b/><b/></a>');
select xml.path('name(/a/@i)', '<a i="1"><b/><b/></a>');
select xml.path('name(/a/node())', '<a i="1"><b/><b/></a>');
select xml.path('/a[name()="a"]', '<a/>');
select xml.path('/a[name()!="a"]', '<a/>');
select xml.path('/a[name()="b"]', '<a/>');
select xml.path('/a[name(/)="b"]', '<a/>');
select xml.path('/a[name(/)!="b"]', '<a/>');
select xml.path('/a[name()!="b"]', '<a/>');
select xml.path('/a[name()=""]', '<a/>');
select xml.path('/a[name()!=""]', '<a/>');
select path('local-name(/a/node())', '<a xmlns:ns_1="namespace"><ns_1:b/><c/></a>');
select path('/a/b[local-name()="b"]', '<a xmlns:ns_1="namespace"><b/><c/></a>');
select path('/a/b[local-name()!="b"]', '<a xmlns:ns_1="namespace"><b/><c/></a>');

--Special case: name() used relative to '/' base path
select path('/', '{"name()"}', '<a/>');

-- Nested functions. In addition, result has to be implicitly cast.
select xml.path('concat(count(/root/a), /root)', '<root><b>50</b></root>');
select xml.path('concat(count(/root), /root)', '<root><b>50</b></root>');

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


-- Cleanup

drop table states;
drop table paths;
drop table docs_other;
