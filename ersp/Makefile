#
# ERSP Player driver
#

#####################################################################
# Setup
#####################################################################

CXX                = g++ -g

PLAYERCXX_CFLAGS   = $(shell pkg-config --cflags playerc++)
PLAYERCXX_LIBS     = $(shell pkg-config --libs playerc++)

# The driver install and include directories.
PLUGIN_DIR         = driver/lib
PLUGIN_PATH        = $(PWD)/$(PLUGIN_DIR)
INCLUDE_DIR        = driver/include
INCLUDE_PATH       = $(PWD)/driver/include

PLAYER             = PLAYERPATH="$(PLUGIN_PATH):$(PLAYERPATH)" player

#####################################################################
# General rules
#####################################################################

all: test-all driver-all

clean: stop test-clean driver-clean

distclean: clean driver-distclean

help: stage-help test-help

#####################################################################
# Driver plugin
#####################################################################

# Automake requires these files to be there.
DRIVER_AMDEPS      = NEWS AUTHORS INSTALL COPYING ChangeLog
DRIVER_SUBDIRS     = ersp hdaps stage
DRIVER_MAKEFILES   = Makefile.in Makefile .deps .libs
DRIVER_SUBDIRFILES = $(foreach subdir,$(DRIVER_SUBDIRS), \
                             $(foreach file,$(DRIVER_MAKEFILES), \
                                     $(subdir)/$(file)))
DRIVER_GENFILES    = aclocal.m4 config.h.in config.h configure libtool \
                     config.log config.status autom4te.cache stamp-h1 \
                     $(DRIVER_MAKEFILES) $(DRIVER_SUBDIRFILES)

driver/configure:
	touch $(patsubst %,driver/%,$(DRIVER_AMDEPS))
	cd driver && autoreconf -fi

# Run configure to generate config.h. By setting libdir, running make
# install in the driver folder will place the driver plugin files in
# PLUGIN_PATH.
driver/config.h: driver/configure 
	cd driver && ./configure --libdir=$(PLUGIN_PATH)

driver-all: driver/config.h
	$(MAKE) -C driver install

driver-clean:
	-test -z "$(wildcard driver/Makefile)" || $(MAKE) -C driver clean

driver-distclean: driver-clean
	$(RM) $(patsubst %,driver/%,$(DRIVER_AMDEPS))
	$(RM) -r $(patsubst %,driver/%,$(DRIVER_GENFILES))
	$(RM) -r $(PLUGIN_DIR)

#####################################################################
# Player/Stage rules
#####################################################################

# Find world configurations
STAGE_CFGS         = $(wildcard stage/*/*.cfg)

# The default real world player driver.
# May be specified on the command line as: make player PLAYER_CFG=foo
PLAYER_CFGS         = $(wildcard player/*.cfg driver/*/*.cfg)
PLAYER_CFG          ?= player/scorpion.cfg

# List available "simulation" configurations
stage-help:
	@echo "Stage Help:"
	@echo "-----------"
	@for i in $(STAGE_CFGS); do \
		printf "%-25s: %s\n" "$$i" \
			   "$$(head -n 1 "$$(dirname $$i)/README" 2>/dev/null)"; \
	done
	@echo
	@echo "    Use: $(MAKE) player PLAYER_CFG=stage/cave/cave.cfg"
	@echo "     or: $(MAKE) stage/cave/cave.cfg"
	@echo "    to run the cave simulation."
	@echo

$(STAGE_CFGS) $(PLAYER_CFGS): stop
	$(PLAYER) $@ &

# Real world testing
player: stop
	$(PLAYER) $(PLAYER_CFG) &

# Stop any player server
stop:
	if ps -o command | grep -q "^player"; then killall player; fi
	$(RM) .player

#####################################################################
# Test programs
#####################################################################

# Find programs and the resulting "targets"
TEST_PROGS    = $(wildcard test/*/main.cc)
TEST_TARGETS  = $(patsubst test/%/main.cc,%,$(TEST_PROGS)) \
			    move-turn ir-avoid bump-stop 

test-all: $(TEST_TARGETS)

test-help:
	@echo "Test programs:"
	@echo "--------------"
	@for i in $(TEST_TARGETS); do \
		printf "%-20s: %s\n" "$$i" \
			   "$$(head -n 1 test/$$i/README 2>/dev/null)"; \
	done

test-clean:
	$(RM) $(TEST_TARGETS)

# Add driver/include/ and test/ to include path to provide the
# robot (scorpion.h) and argument parsing (args.h) header files.
%: test/%/main.cc
	$(CXX) -o $@ -Itest -I$(INCLUDE_DIR) $(PLAYERCXX_CFLAGS) $(PLAYERCXX_LIBS) $<

%: test/%.cc
	$(CXX) -o $@ -Itest -I$(INCLUDE_DIR) $(PLAYERCXX_CFLAGS) $(PLAYERCXX_LIBS) $<
