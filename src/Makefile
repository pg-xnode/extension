
MODULE_big = xnode
OBJS = xmlnode.o xpath.o xpath_parser.o xml_parser.o xmlnode_util.o xpath_functions.o \
xml_scan.o xml_update.o

EXTENSION = xnode
DATA = xnode--0.6.sql

#REGRESS = xnode

ifndef PG_CONFIG
PG_CONFIG = pg_config
endif
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
