#ifndef ERSP_INC
#define ERSP_INC

typedef enum {
	ERSP_DRIVE,     /* Drive interface */
	ERSP_AVOID,     /* Avoidance interface */
	ERSP_BATTERY,   /* Battery interface */
	ERSP_BUMP,      /* Bump sensor */
	ERSP_IR_BINARY, /* Digital IR sensor */
	ERSP_IR_RANGE,  /* Range IR sensor */
} ersp_dev_type_t;

typedef int ersp_robot_dev_t;

struct ersp_dev {
	ersp_robot_dev_t robot_id;
	ersp_robot_dev_t player_id;
	ersp_dev_type_t type;
	const char *id;
};


#define ERSP_INTERFACE(ROBOT, TYPE, ID) \
        enum ROBOT##_##TYPE##_##ID##_id {

#define ERSP_DEV(ROBOT, ID, TYPE, NAME) \
                ROBOT##_##ID,

#define ERSP_INTERFACE_END \
        };

#include ERSP_INCLUDE

#undef ERSP_INTERFACE
#undef ERSP_INTERFACE_END
#undef ERSP_DEV

/** Scorpion specific device IDs:
 *
 * The IDs are used internally and contains the device type
 * information. Example: SCORPION_IR_RANGE_IR_TW_NNW.
 */
enum {

#define ERSP_INTERFACE(ROBOT, TYPE, NAME)
#define ERSP_DEV(ROBOT, ID, TYPE, NAME) \
        ROBOT##_##TYPE##_##ID,
#define ERSP_INTERFACE_END

#include ERSP_INCLUDE

#undef ERSP_INTERFACE
#undef ERSP_INTERFACE_END
#undef ERSP_DEV

	ERSP_DEVICES,
};


#define ERSP_INTERFACE(ROBOT, TYPE, NAME) \
	static inline char *ROBOT##_##TYPE##_##NAME##_name(int id) \
	{ \
		switch (id) {

#define ERSP_DEV(ROBOT, ID, TYPE, NAME) \
			case ROBOT##_##ID: return NAME;

#define ERSP_INTERFACE_END \
		} \
		return "unknown type"; \
	}
		


#include ERSP_INCLUDE

#undef ERSP_INTERFACE
#undef ERSP_INTERFACE_END
#undef ERSP_DEV

#endif
