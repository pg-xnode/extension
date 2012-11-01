MODULE_big = xnode
OBJS = xmlnode.o xpath.o xpath_parser.o xml_parser.o xml_printer.o \
xmlnode_util.o xpath_functions.o xml_scan.o xml_update.o xnt.o dtd.o \
namespace.o

EXTENSION = xnode
DATA = xnode--0.7.2.sql

REGRESS = xnode update xnt
REGRESS_OPTS += --user=postgres

ifndef PG_CONFIG
PG_CONFIG = pg_config
endif
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
