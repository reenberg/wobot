COMPONENT = EWMA

include ../common.mk

GENERATED += EWMAC.nc EWMAM.nc

exe0 : $(GENERATED)

clean: FORCE
	rm -rf build $(GENERATED) obj flaskgen *.dump.*

EWMAC.nc EWMAM.nc : Main.hs
	mkdir -p obj
	ghc --make -odir obj -hidir obj -o flaskgen -W \
	    -DNESC \
	    $<
	./flaskgen $(FLASK_FLAGS) --prelude $(FLASK_PRELUDE) -o EWMA
