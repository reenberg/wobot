COMPONENT = Query

include ../common.mk

QUERY_FILE = temp.sql
GENERATED = QueryC.nc QueryM.nc

exe0 : $(GENERATED)

clean: FORCE
	rm -rf build $(GENERATED)

QueryC.nc QueryM.nc : $(QUERY_FILE)
	$(FLASKDB) --prelude $(FLASKDB_PRELUDE) -o Query $(QUERY_FILE)
