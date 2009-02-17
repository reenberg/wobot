/*
 * HDAPS Player driver
 */

/** @ingroup drivers */
/** @{ */
/** @defgroup driver_hdaps HDAPS
 * @brief HDAPS Player driver
 *
 * The HDAPS driver provides a "joystick" interface. There currently
 * does not exist a JoystickProxy usable for client programs so the main
 * goal of this driver is to expose the "joystick" interface so that
 * virtual drivers can take robots may use it.
 *
 * This driver is still experimental. Due to problems to get the Linux
 * HDAPS driver module to work it has not been tested. To test it use
 * the player/hdaps-scorpion.cfg configuration file together with the
 * playerjoy program, which maps "joystick" commands to "position2d"
 * commands.
 *
 * @par Compile-time dependencies
 *
 *  - hdaps Linux kernel module
 *
 * @par Provides
 *
 * The driver provides the following device interfaces:
 *
 *  - "joystick" @ref interface_joystick.
 *     This interface returns joystick data.
 *
 * @par Supported configuration requests
 *
 *  - None.
 *
 * @par Configuration file options
 *
 *  - None.
 *
 * @par Example
 *
 * @verbinclude hdaps.cfg
 *
 * @par References
 *
 * @anchor interface_joystick
 *  - joystick interface: http://playerstage.sourceforge.net/doc/Player-2.0.0/player/group__interface__joystick.html
 *
 * @author Jonas Fonseca <fonseca@diku.dk>
 * @author Bue Petersen <buep@diku.dk>
 */
/** @} */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/fcntl.h>
#include <unistd.h>

#include "hdaps.h"


#define SYSFS_HDAPS_POSITION "/sys/devices/platform/hdaps/position"

static int hdaps_position(int *x, int *y);

/////////////////////////////////////////////////////
//	Driver setup and shutdown
/////////////////////////////////////////////////////

HDAPS::HDAPS(ConfigFile* cf, int section)
    : Driver(cf, section, true, PLAYER_MSGQUEUE_DEFAULT_MAXLEN)
{
	// zero ids, so that we'll know later which interfaces were requested
	memset(&joystick_id, 0, sizeof(joystick_id));

	if (cf->ReadDeviceAddr(&joystick_id, section, "provides",
	                       PLAYER_JOYSTICK_CODE, -1, NULL) == 0) {
		if (AddInterface(joystick_id) != 0) {
			SetError(-1);
			return;
		}
	}
}

int HDAPS::Setup()
{
	int xpos, ypos;

	if (hdaps_position(&xpos, &ypos)) {
		PLAYER_ERROR("Failed to read HDAPS position data\n"
		             "Maybe you need to run: modprobe hdaps");
		//return -1;
	}

	StartThread();
	return 0;
}

int HDAPS::Shutdown()
{
	StopThread();
	return 0;
}

HDAPS::~HDAPS (void)
{
}


/////////////////////////////////////////////////////
//	Driver Main Loop
/////////////////////////////////////////////////////

void
HDAPS::Main()
{
	int xpos, ypos;

	for(;;) {
		pthread_testcancel();

		Lock();

		memset(&data, 0, sizeof(data));
		if (hdaps_position(&xpos, &ypos) == 0) {
			data.xpos = (uint32_t) xpos;
			data.ypos = (uint32_t) ypos;
		}

		Unlock();

		if (memcmp(&prev_data, &data, sizeof(data))) {
			PutData();
			memcpy(&prev_data, &data, sizeof(data));
		}

		// Handle pending messages
		if (InQueue->Empty() == false) {
			ProcessMessages();
		}
	}
}


/////////////////////////////////////////////////////
//	Publish / Subscribe / Unsubscribe
/////////////////////////////////////////////////////

int
HDAPS::Subscribe(player_devaddr_t id)
{
	return Driver::Subscribe(id);
}

int
HDAPS::Unsubscribe(player_devaddr_t id)
{
	return Driver::Unsubscribe(id);
}

void
HDAPS::PutData(void)
{
	PLAYER_MSG0(1, "Publishing joystick data");
	Publish(joystick_id, NULL, PLAYER_MSGTYPE_DATA,
	        PLAYER_JOYSTICK_DATA_STATE,
			(void *) &data, sizeof(data));
}


/////////////////////////////////////////////////////
//	Message processing
/////////////////////////////////////////////////////

int
HDAPS::ProcessMessage(MessageQueue *queue, player_msghdr *hdr, void *data)
{
	PLAYER_WARN("unknown message");
	return -1;
}


/////////////////////////////////////////////////////
//	HDAPS position handling
/////////////////////////////////////////////////////

static int hdaps_position(int *x, int *y)
{
	char buf[BUFSIZ];
	int fd, ret;

	fd = open(SYSFS_HDAPS_POSITION, O_RDONLY);
	if (fd < 0) {
		PLAYER_ERROR1("error opening: %s", strerror(errno));
		return fd;
	}

	ret = read(fd, buf, sizeof(buf));
	if (ret > 0) {
		ret = 0;
		if (sscanf (buf, "(%d,%d)\n", x, y) != 2)
			ret = 1;

	} else if (ret < 0) {
		PLAYER_ERROR1("error reading: %s", strerror(errno));

	} else {
		PLAYER_ERROR("Failed to read position");
		ret = 1;
	}

	if (close (fd))
		PLAYER_ERROR1("error closing: %s", strerror(errno));

	return ret;
}


/////////////////////////////////////////////////////
//	Driver module registration and initializaion
/////////////////////////////////////////////////////

Driver*
HDAPS_Init(ConfigFile* cf, int section)
{
	return (Driver*)(new HDAPS(cf, section));
}

void HDAPS_Register(DriverTable* table)
{
	table->AddDriver("hdaps", HDAPS_Init);
}

/* Need the extern to avoid C++ name-mangling  */
extern "C" {

int player_driver_init(DriverTable* table)
{
	PLAYER_MSG0(1, "Registering HDAPS driver.");
	HDAPS_Register(table);
	return 0;
}

}
