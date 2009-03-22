COMPONENT = EWMA

include ../common.mk

exe0 : $(COMPONENT)

clean:
	rm -rf build $(GENERATED) obj flaskgen *.dump.*

EWMA : Main.hs
	mkdir -p obj
	ghc --make -odir obj -hidir obj -o flaskgen -W \
	    -DNESC \
	    $<
	./flaskgen $(FLASK_FLAGS) --prelude $(FLASK_PRELUDE) -o helloworld
