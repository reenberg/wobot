/*
 * Sonar2IR Player driver
 */

/** @ingroup drivers */
/** @{ */
/** @defgroup driver_sonar2ir Sonar2IR
 * @brief Sonar2IR Player driver
 *
 * This driver maps a "sonar" interface (as provided by Stage) to an
 * "ir" interface. For now Stage does not provide any "ir" interface,
 * however, stage uses the same internal model for simulating the "ir"
 * and "sonar" interface. Rather than changing Stage to support the
 * "ir" interface (which it hopefully will eventually), this driver
 * works around this issue by providing an "ir" interface based on the
 * "sonar" interface from stage. 
 * 
 * This driver is fully functional, although not all functionality has
 * been properly tested. To try it simply run the cave stage and use
 * the "ir" interface (via the IrProxy class).
 *
 * @par Compile-time dependencies
 *
 *  - None.
 *
 * @par Provides
 *
 * The driver provides the following device interfaces:
 *
 *  - "ir" @ref interface_ir.
 *    This interface returns IR data (player_ir_data_t).
 *
 * @par Requires
 *
 *  - "sonar" @ref interface_sonar.
 *    The interface from which to read sonar sensor readings (player_sonar_data_t).
 *
 * @par Supported configuration requests
 *
 *  - PLAYER_IR_POWER: Turn off the IR devices (player_power_req_t).
 *  - PLAYER_IR_POSE: Get IR sensor poses (player_ir_pose_t).
 *
 * @par Configuration file options
 *
 *  - None.
 *
 * @par Example
 *
 * @verbinclude sonar2ir.cfg
 *
 * @par References
 *
 * @anchor interface_ir
 *  - ir interface: http://playerstage.sourceforge.net/doc/Player-2.0.0/player/group__interface__ir.html
 *
 * @anchor interface_sonar
 *  - sonar interface: http://playerstage.sourceforge.net/doc/Player-2.0.0/player/group__interface__sonar.html
 *
 * @author Jonas Fonseca <fonseca@diku.dk>
 * @author Bue Petersen <buep@diku.dk>
 *
 * January 2007
 */
/** @} */

#include "sonar2ir.h"

#define MAX_IR_SAMPLES(sonar_samples) \
	((sonar_samples) <= PLAYER_IR_MAX_SAMPLES \
	 ? (sonar_samples) : PLAYER_IR_MAX_SAMPLES)


/////////////////////////////////////////////////////
//	Driver setup and shutdown
/////////////////////////////////////////////////////

Sonar2IR::Sonar2IR(ConfigFile* cf, int section)
    : Driver(cf, section, true, PLAYER_MSGQUEUE_DEFAULT_MAXLEN)
{
	// zero ids, so that we'll know later which interfaces were requested
	memset(&ir_id, 0, sizeof(ir_id));
	memset(&sonar_id, 0, sizeof(sonar_id));
	subscriptions = 0;
	republish_data = republish_pose = false;

	if (cf->ReadDeviceAddr(&ir_id, section, "provides",
	                       PLAYER_IR_CODE, -1, NULL) == 0) {
		if (this->AddInterface(ir_id) != 0) {
			this->SetError(-1);
			return;
		}
	}

	if (cf->ReadDeviceAddr(&sonar_id, section, "requires",
	                       PLAYER_SONAR_CODE, -1, NULL) != 0) {
		this->SetError(-1);
		return;
	}
}

int Sonar2IR::Setup()
{
	if (Device::MatchDeviceAddress(sonar_id, ir_id)) {
		PLAYER_ERROR("subscribing to self");
		return -1;
	}

	if (!(sonar = deviceTable->GetDevice(sonar_id))) {
		PLAYER_ERROR("sonar device unavailable");
		return -1;
	}

	if (sonar->Subscribe(InQueue) != 0) {
		PLAYER_ERROR("sonar device subscription failed");
		return -1;
	}

	StartThread();
	return 0;
}

int Sonar2IR::Shutdown()
{
	StopThread();
	sonar->Unsubscribe(InQueue);
	return 0;
}

Sonar2IR::~Sonar2IR (void)
{
}

void
Sonar2IR::Main()
{
	for(;;) {
		pthread_testcancel();

		// Wait for sonar messages
		InQueue->Wait();

		// Handle pending messages
		ProcessMessages();

		PutData();
	}
}


/////////////////////////////////////////////////////
//	Message processing
/////////////////////////////////////////////////////

int
Sonar2IR::ProcessMessage(MessageQueue *queue, player_msghdr *hdr, void *data)
{
	if (Message::MatchMessage(hdr, PLAYER_MSGTYPE_DATA,
	                          PLAYER_SONAR_DATA_RANGES, sonar_id)) {
		Lock();
		if (!republish_data) {
			player_sonar_data_t *sonar_data = (player_sonar_data_t *) data;

			memset(&ir_data, 0, sizeof(ir_data));

			ir_data.ranges_count = MAX_IR_SAMPLES(sonar_data->ranges_count);

			for (uint32_t i = 0; i < ir_data.ranges_count; i++) {
				ir_data.ranges[i] = sonar_data->ranges[i];
			}

			republish_data = true;
		}
		Unlock();
		return 0;
	}

	if (Message::MatchMessage(hdr, PLAYER_MSGTYPE_DATA,
	                          PLAYER_SONAR_DATA_GEOM, sonar_id)) {
		Lock();
		if (!republish_pose) {
			player_sonar_geom_t *sonar_geom = (player_sonar_geom_t *) data;

			memset(&ir_pose, 0, sizeof(ir_pose));

			ir_pose.poses_count = MAX_IR_SAMPLES(sonar_geom->poses_count);

			for (uint32_t i = 0; i < ir_pose.poses_count; i++) {
				ir_pose.poses[i] = sonar_geom->poses[i];
			}

			republish_pose = true;
		}
		Unlock();
		return 0;
	}

	// Forward requests to the sonar device
	if (Message::MatchMessage(hdr, PLAYER_MSGTYPE_REQ,
	                          PLAYER_IR_POSE, ir_id) ||
		Message::MatchMessage(hdr, PLAYER_MSGTYPE_REQ,
	                          PLAYER_IR_POWER, ir_id)) {
		Publish(sonar_id, NULL, hdr->type, hdr->subtype,
		        data, hdr->size);
		return 0;
	}

	return -1;
}


/////////////////////////////////////////////////////
//	Publish / Subscribe / Unsubscribe
/////////////////////////////////////////////////////

void
Sonar2IR::PutData(void)
{
	Lock();
	if (republish_data) {
		Unlock();
		Publish(ir_id, NULL, PLAYER_MSGTYPE_DATA,
		        PLAYER_IR_DATA_RANGES,
		        (void *) &ir_data, sizeof(ir_data));
		Lock();
		republish_data = republish_pose = false;
	}
	Unlock();

	Lock();
	if (republish_pose) {
		Unlock();
		Publish(ir_id, NULL, PLAYER_MSGTYPE_DATA, PLAYER_IR_POSE,
		        (void *) &ir_pose, sizeof(ir_pose));
		Lock();
		republish_pose = false;
	}
	Unlock();
}

int
Sonar2IR::Subscribe(player_devaddr_t id)
{
	return Driver::Subscribe(id);
}

int
Sonar2IR::Unsubscribe(player_devaddr_t id)
{
	return Driver::Unsubscribe(id);
}


/////////////////////////////////////////////////////
//	Driver module registration and initializaion
/////////////////////////////////////////////////////

Driver*
Sonar2IR_Init(ConfigFile* cf, int section)
{
	return (Driver*)(new Sonar2IR(cf, section));
}

void Sonar2IR_Register(DriverTable* table)
{
	table->AddDriver("sonar2ir", Sonar2IR_Init);
}

/* Need the extern to avoid C++ name-mangling  */
extern "C" {

int player_driver_init(DriverTable* table)
{
	Sonar2IR_Register(table);
	return 0;
}

}
