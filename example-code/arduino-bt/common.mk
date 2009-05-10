# Flask is one dir up from this one.
FLASK=./../../../kode/

FLASK_RUNTIME=$(FLASK)/runtime
FLASK_PRELUDE=$(FLASK_RUNTIME)/Prelude.hs

include $(MAKERULES)

PFLAGS += -g

PFLAGS += -I. -I.. \
	  -I$(FLASK_RUNTIME)/common \
	  -I$(FLASK_RUNTIME)/Flow

PFLAGS += -DFLOW_APP_ACKS=1
PFLAGS += -DTOSH_MAX_TASKS_LOG2=4

ifeq ($(PLATFORM),pc)
PFLAGS += -I$(TINYOSDIR)/tos/platform/pc/packet
PFLAGS += -DTOSH_DATA_LENGTH=48
PFLAGS += -DBASE_NODE_ID=0
endif

ifeq ($(PLATFORM),telosb)
PFLAGS += -DTOSH_DATA_LENGTH=48
PFLAGS += -DBASE_NODE_ID=0
endif

ifeq ($(PLATFORM),mica2)
PFLAGS += -DTOSH_DATA_LENGTH=48
PFLAGS += -DBASE_NODE_ID=0
endif

ifeq ($(PLATFORM),micaz)
PFLAGS += -DTOSH_DATA_LENGTH=48
PFLAGS += -DBASE_NODE_ID=0
endif
