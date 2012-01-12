
MODULE_big = xnode
OBJS = xmlnode.o xpath.o xpath_parser.o xml_parser.o xmlnode_util.o xpath_functions.o \
xml_scan.o xml_update.o

EXTENSION = xnode
DATA = xnode--0.5.9.sql

#REGRESS = xnode

PG_CONFIG = /usr/local/pgsql/bin/pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
