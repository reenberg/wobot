/*
 * ERSP  Player driver
 */

/** @ingroup drivers */
/** @{ */
/** @defgroup driver_ersp ersp
 * @brief Evolution ERSP-based robots


@author Bue, Jonas
*/
/** @} */

#include "ersp.h"

#ifndef DEBUG
#define PLAYER_INFO(msg) PLAYER_MSG0(-1, msg)
#define PLAYER_DEBUG(msg) PLAYER_MSG0(-2, msg)
#else
#define PLAYER_INFO(msg)
#define PLAYER_DEBUG(msg)
#endif

/////////////////////////////////////////////////////
//	Driver module registration and initializaion
/////////////////////////////////////////////////////

Driver*
ERSP_Init(ConfigFile* cf, int section)
{
	return (Driver*)(new ERSP(cf, section));
}

void ERSP_Register(DriverTable* table)
{
	table->AddDriver("ersp", ERSP_Init);
}

/* Need the extern to avoid C++ name-mangling  */
extern "C" {

int player_driver_init(DriverTable* table)
{
	PLAYER_INFO("Registering ERSP driver.");
	ERSP_Register(table);

	return 0;
}

}

/////////////////////////////////////////////////////
//	Driver setup and shutdown
/////////////////////////////////////////////////////

ERSP::ERSP(ConfigFile* cf, int section)
		: Driver(cf, section, true, PLAYER_MSGQUEUE_DEFAULT_MAXLEN)
{
	// zero ids, so that we'll know later which interfaces were requested
	memset(&this->position_id, 0, sizeof(player_devaddr_t));
	memset(&this->bumper_id, 0, sizeof(player_devaddr_t));
	memset(&this->ir_range_id, 0, sizeof(player_devaddr_t));
	memset(&this->ir_binary_id, 0, sizeof(player_devaddr_t));
	memset(&this->power_id, 0, sizeof(player_devaddr_t));

	this->position_subscriptions = 0;
	this->bumper_subscriptions = 0;
	this->ir_range_subscriptions = 0;
	this->ir_binary_subscriptions = 0;
	this->power_subscriptions = 0;
	this->running = false;

	if (cf->ReadDeviceAddr(&(this->position_id), section, "provides",
						   PLAYER_POSITION2D_CODE, -1, NULL) == 0) {
		if (this->AddInterface(this->position_id) != 0) {
			this->SetError(-1);
			return;
		}
	}
 
	if (cf->ReadDeviceAddr(&(this->bumper_id), section, "provides",
						   PLAYER_BUMPER_CODE, -1, "bumper") == 0) {
		if (this->AddInterface(this->bumper_id) != 0) {
			this->SetError(-1);
			return;
		}
	}

	if (cf->ReadDeviceAddr(&(this->ir_range_id), section, "provides",
						   PLAYER_IR_CODE, -1, NULL) == 0) {
		if (this->AddInterface(this->ir_range_id) != 0) {
			this->SetError(-1);
			return;
		}
	}

	if (cf->ReadDeviceAddr(&(this->ir_binary_id), section, "provides",
						   PLAYER_BUMPER_CODE, -1, "ir") == 0) {
		if (this->AddInterface(this->ir_binary_id) != 0) {
			this->SetError(-1);
			return;
		}
	}
}

int ERSP::SetupERSP()
{
	static const struct ersp_dev ersp_devs[] = {

#define ERSP_INTERFACE(ROBOT, TYPE, NAME)
#define ERSP_INTERFACE_END
#define ERSP_DEV(ROBOT, ID, TYPE, NAME) \
	    { ROBOT##_##TYPE##_##ID, ROBOT##_##ID, ERSP_##TYPE, NAME },

#include ERSP_INCLUDE

#undef ERSP_INTERFACE
#undef ERSP_INTERFACE_END
#undef ERSP_DEV

	};
	devs = ersp_devs;

	if (this->running)
		return 0;

#ifdef HAVE_ERSP
	Evolution::Result result;

	// Get the ERSP resource manager
	manager = new Evolution::ResourceManager(NULL, &result); 
	if (result != Evolution::RESULT_SUCCESS) {
		PLAYER_ERROR("Cannot create a resource manager.");
		return -1;
	}

	result = manager->get_resource_container(0, &container);
	if (result != Evolution::RESULT_SUCCESS) {
		PLAYER_ERROR("Cannot create a resource container.");
		return -1;
	}

	for (unsigned int i = 0; i < ERSP_DEVICES; i++) {
		/* XXX: Ordered by ersp_dev_type_t. */
		static const char *interfaces[] = {
			Evolution::IDriveSystem::INTERFACE_ID,
			Evolution::IAvoidance::INTERFACE_ID,
			Evolution::IBattery::INTERFACE_ID,
			Evolution::IBumpSensor::INTERFACE_ID,
			Evolution::IBumpSensor::INTERFACE_ID,
			Evolution::IRangeSensor::INTERFACE_ID,
		};
		void **handle = &devices[i].handle;
		const char *id = devs[i].id;
		const char *interface = interfaces[devs[i].type];

		result = container->obtain_interface(Evolution::NO_TICKET,
											 id, interface, handle);
		if (result != Evolution::RESULT_SUCCESS) {
			PLAYER_ERROR1("Cannot create a %s system.", id);
			return -1;
		}
	}

	devices[SCORPION_AVOID_AVOID].avoid->disable_avoidance(Evolution::NO_TICKET);
#endif /* HAVE_ERSP */

	return 0;
}

/* Should return 1 on error, and 0 on success. */
int ERSP::Setup()
{
	PLAYER_INFO("Driver setup");

	if (SetupERSP()) {
#ifdef HAVE_ERSP
		// FIXME: This might need to be changed so that
		// nothing is freed until the destructor is called
		// and instead have the SetupERSP conditionally
		// initialize variables.
		if (manager != NULL)
			delete manager;
#endif /* HAVE_ERSP */

		return 1;
	}

	// Now spawn reading thread
	// Prospone this to here since ::Shutdown will not be called if
	// an error condition is returned.
	this->StartThread();
	this->running = true;

	PLAYER_INFO("Driver setup done");

    return 0;
}

int ERSP::Shutdown()
{
	PLAYER_INFO("Driver shutdown");

	if (!this->running)
		return 0;

	this->StopThread();
	this->running = false;

	PLAYER_INFO("Driver has been shutdown");

	return 0;
}

ERSP::~ERSP (void)
{
#ifdef HAVE_ERSP
	/* Deallocate the Resource Manager.
	 *
	 * This means that the ERSP library will be initialized and ready
	 * throughout the life of the player server.
	 *
	 * FIXME: We still need to ensure that the various interfaces
	 * (driver, ...) are reset so the robot doesn't do anything. */

	if (manager != NULL)
		delete manager;
#endif /* HAVE_ERSP */
}

void
ERSP::Main()
{
#ifdef HAVE_ERSP
	Evolution::Timestamp timestamp;
	Evolution::Result result;
#endif /* HAVE_ERSP */
	time_t prev_time = 0;

	for(;;) {
		pthread_testcancel();

		this->Lock();

		ersp_prev = ersp_data;

		if (this->bumper_subscriptions != 0) {
			player_bumper_data_t *bumper = &ersp_data.bumper;

			memset(bumper, 0, sizeof(*bumper));

			for (unsigned int i = 0; i < ERSP_DEVICES; i++) {
				bool triggered = false;

				if (devs[i].type != ERSP_BUMP)
					continue;

#ifdef HAVE_ERSP
				result = devices[i].bumper->is_triggered(Evolution::NO_TICKET,
                                                         &timestamp,
														 &triggered);
				if (result != Evolution::RESULT_SUCCESS) {
					PLAYER_ERROR1("Error reading from binary sensor %s",
							devs[i].id);
					SetError(-1);
				}
#endif /* HAVE_ERSP */

				bumper->bumpers[bumper->bumpers_count++] = (uint8_t) triggered == true;
			}
		}

		if (this->ir_range_subscriptions != 0) {
			player_ir_data_t *ir = &ersp_data.ir_range;

			memset(ir, 0, sizeof(*ir));

			for (unsigned int i = 0; i < ERSP_DEVICES; i++) {
				double distance = 0;

				if (devs[i].type != ERSP_IR_RANGE)
					continue;

#ifdef HAVE_ERSP
				result = devices[i].range->get_distance_reading(Evolution::NO_TICKET,
												    &timestamp, &distance);
				if (result != Evolution::RESULT_SUCCESS) {
					PLAYER_ERROR1("Error reading from range sensor %s",
							devs[i].id);
					SetError(-1);
				}
#endif /* HAVE_ERSP */

				/* Convert to meters */
				distance = distance / 100;
				if (distance > IR_MAX_RANGE) {
					distance = IR_MAX_RANGE;
				}

				ir->ranges[ir->ranges_count++] = (float) distance;
			}
		}

		if (this->ir_binary_subscriptions != 0) {
			player_bumper_data_t *ir = &ersp_data.ir_binary;

			memset(ir, 0, sizeof(*ir));

			for (unsigned int i = 0; i < ERSP_DEVICES; i++) {
				bool triggered = false;

				if (devs[i].type != ERSP_IR_BINARY)
					continue;

#ifdef HAVE_ERSP
				result = devices[i].bumper->is_triggered(Evolution::NO_TICKET,
                                                         &timestamp,
														 &triggered);
				if (result != Evolution::RESULT_SUCCESS) {
					PLAYER_ERROR1("Error reading from binary sensor %s",
							devs[i].id);
					SetError(-1);
				}
#endif /* HAVE_ERSP */

				ir->bumpers[ir->bumpers_count++] = (uint8_t) triggered == true;
			}
		}

		this->Unlock();

		if (this->position_subscriptions ||
		    this->bumper_subscriptions ||
			this->ir_range_subscriptions ||
			this->ir_binary_subscriptions ||
			this->power_subscriptions) {
			time_t now = time(NULL);

			/* Force publication of data every second. */
			if (now > prev_time) {
				prev_time = now;
				memset(&ersp_prev, 0, sizeof(ersp_prev));
			}
		}
				PutData();

		// Handle pending messages
		if (!this->InQueue->Empty()) {
			PLAYER_DEBUG("Processing messages");
			ProcessMessages();
		}
	}
}

/////////////////////////////////////////////////////
//	Message processing
/////////////////////////////////////////////////////

void
ERSP::set_motor_velocity(double velocity, double angular_velocity)
{
	const double ACCELERATION = 20;
	const double ANGULAR_ACCELERATION = M_PI / 2.0; // radians/sec^2

#ifdef DEBUG
	PLAYER_MSG4(2, "Setting motors: vel=%f, ang_vel=%f, acc=%f, ang_acc=%f",
			    velocity, angular_velocity,
				ACCELERATION, ANGULAR_ACCELERATION);
#endif

#ifdef HAVE_ERSP
	Evolution::Result result;
	Evolution::IDriveSystem *driver = devices[SCORPION_DRIVE_DRIVE].driver;

	// Player uses m/sec while ERSP uses cm/s
	result = driver->move_and_turn(Evolution::NO_TICKET,
								   velocity * 100, ACCELERATION,
								   angular_velocity,
								   ANGULAR_ACCELERATION);
	if (result != Evolution::RESULT_SUCCESS) {
		PLAYER_ERROR("Failed to set velocity");
		SetError(-1);
	}
#endif
}

int
ERSP::ProcessMessage(MessageQueue * resp_queue,
					 player_msghdr * hdr,
					 void * data)
{
	// Look for configuration requests
	if (hdr->type == PLAYER_MSGTYPE_REQ)
		return this->HandleConfig(resp_queue, hdr, data);

	if (hdr->type == PLAYER_MSGTYPE_CMD)
		return this->HandleCommand(hdr, data);

	return -1;
}

int
ERSP::HandleConfig(MessageQueue* resp_queue,
				   player_msghdr * hdr,
				   void * data)
{
	// check for position config requests
	if (Message::MatchMessage(hdr, PLAYER_MSGTYPE_REQ,
								   PLAYER_IR_POSE,
								   this->ir_range_id)) {
		/* Return the bumper geometry. */
		if(hdr->size != 0) {
			PLAYER_WARN("Arg get ir range geom is wrong size; ignoring");
			return -1;
		}
		player_ir_pose_t ir_pose;

		memset(&ir_pose, 0, sizeof(ir_pose));
		ir_pose.poses[SCORPION_IR_BN_ENE].px = 0;

#define IR_POSE(id, _px, _py, _pa) \
		ir_pose.poses[SCORPION_##id].px = _px; \
		ir_pose.poses[SCORPION_##id].py = _py; \
		ir_pose.poses[SCORPION_##id].pa = _pa; \
		ir_pose.poses_count++;

		IR_POSE(IR_BN_ENE, 0.126, 0.092, -1.134); 
		IR_POSE(IR_BN_WNW, 0.126, -0.092, 1.134);
		IR_POSE(IR_BN_N, 0.049, 0.000, 0);
		IR_POSE(IR_BN_NE, 0.092, 0.064, -0.611); 
		IR_POSE(IR_BN_NW, 0.092, -0.064, 0.611); 
		IR_POSE(IR_TE_NNW, 0.016, -0.173, 0.175); 
		IR_POSE(IR_TE_NNE, -0.065, -0.169, -0.262); 
		IR_POSE(IR_TW_NNW, -0.065, 0.169, 0.262); 
		IR_POSE(IR_TW_NNE, 0.016, 0.173, -0.175); 
		IR_POSE(IR_BS_W, -0.172, 0.019, 1.571); 
		IR_POSE(IR_BS_E, -0.172, -0.019, -1.571); 
		IR_POSE(IR_BW_S, -0.077, 0.069, 3.054); 
		IR_POSE(IR_BE_S, -0.077, -0.069, -3.054) 

		Publish(ir_range_id, resp_queue,
				PLAYER_MSGTYPE_RESP_ACK, PLAYER_IR_POSE,
				&ir_pose, sizeof(ir_pose));

		return 0;
	}
	else {
		PLAYER_WARN("unknown config request to ersp driver");
		return -1;
	}
}

int
ERSP::HandleCommand(player_msghdr *hdr, void* data)
{
	if (Message::MatchMessage(hdr, PLAYER_MSGTYPE_CMD,
							  PLAYER_POSITION2D_CMD_VEL,
							  this->position_id)) {

		player_position2d_cmd_vel_t cmd = *(player_position2d_cmd_vel_t*) data;

		// FIXME: check state: if false turn off the motors?
		set_motor_velocity(cmd.vel.px, cmd.vel.pa);
		return 0;
	}
	else if (Message::MatchMessage(hdr, PLAYER_MSGTYPE_CMD,
								   PLAYER_POSITION2D_CMD_CAR,
								   this->position_id)) {
		player_position2d_cmd_car_t cmd = *(player_position2d_cmd_car_t*) data;

		set_motor_velocity(cmd.velocity, cmd.angle);
		return 0;
	}

	return -1;
}


/////////////////////////////////////////////////////
//	Publish / Subscribe / Unsubscribe
/////////////////////////////////////////////////////

int
ERSP::Subscribe(player_devaddr_t id)
{
	int result;

	// Do the subscription ...
	if ((result = Driver::Subscribe(id)) == 0) {
		// ... and increment the appropriate subscription counter
		if (Device::MatchDeviceAddress(id, this->position_id)) {
			position_subscriptions++;
			PLAYER_MSG1(1, "%d position subscriptions", position_subscriptions);
		}
		else if (Device::MatchDeviceAddress(id, this->bumper_id)) {
			bumper_subscriptions++;
			PLAYER_MSG1(1, "%d bumper subscriptions",
					 this->bumper_subscriptions);
		}
		else if (Device::MatchDeviceAddress(id, this->ir_range_id)) {
			ir_range_subscriptions++;
			PLAYER_MSG1(1, "%d ir range subscriptions",
					 this->ir_range_subscriptions);
		}
		else if (Device::MatchDeviceAddress(id, this->ir_binary_id)) {
			ir_binary_subscriptions++;
			PLAYER_MSG1(1, "%d ir binary subscriptions",
					 this->ir_binary_subscriptions);
		}
#ifdef POWER
		else if (Device::MatchDeviceAddress(id, this->power_id)) {
			power_subscriptions++;
			PLAYER_MSG1(1, "%d power subscriptions", this->power_subscriptions);
		}
#endif
	}

	return result;
}

int
ERSP::Unsubscribe(player_devaddr_t id)
{
	int result;

	// Do the unsubscription ...
	if ((result = Driver::Unsubscribe(id)) == 0) {
		// ... and decrement the appropriate subscription counter
		if (Device::MatchDeviceAddress(id, this->position_id)) {
			position_subscriptions--;
			assert(position_subscriptions >= 0);
			PLAYER_MSG1(1, "%d position subscriptions",
					 position_subscriptions);
			if (position_subscriptions == 0) {
				set_motor_velocity(0, 0);
			}
		}
		else if (Device::MatchDeviceAddress(id, this->bumper_id)) {
			bumper_subscriptions--;
			assert(bumper_subscriptions >= 0);
			PLAYER_MSG1(1, "%d bumper subscriptions",
					 bumper_subscriptions);
		}
		else if (Device::MatchDeviceAddress(id, this->ir_range_id)) {
			ir_range_subscriptions--;
			assert(ir_range_subscriptions >= 0);
			PLAYER_MSG1(1, "%d ir subscriptions",
					 ir_range_subscriptions);
		}
		else if (Device::MatchDeviceAddress(id, this->ir_binary_id)) {
			ir_binary_subscriptions--;
			assert(ir_binary_subscriptions >= 0);
			PLAYER_MSG1(1, "%d ir subscriptions",
					 ir_binary_subscriptions);
		}
#ifdef POWER
		else if (Device::MatchDeviceAddress(id, this->power_id)) {
			power_subscriptions--;
			assert(power_subscriptions >= 0);
			PLAYER_MSG1(1, "%d power subscriptions", power_subscriptions);
		}
#endif
	}

	return result;
}

void
ERSP::PutData(void)
{
	if (bumper_subscriptions &&
		memcmp(&ersp_data.bumper, &ersp_prev.bumper, sizeof(ersp_data.bumper)))
	this->Publish(this->bumper_id, NULL,
				  PLAYER_MSGTYPE_DATA,
				  PLAYER_BUMPER_DATA_STATE,
				  (void*)&(this->ersp_data.bumper),
				  sizeof(player_bumper_data_t),
				  NULL);
	
	if (ir_range_subscriptions &&
		memcmp(&ersp_data.ir_range, &ersp_prev.ir_range, sizeof(ersp_data.ir_range)))
	this->Publish(this->ir_range_id, NULL,
				  PLAYER_MSGTYPE_DATA,
				  PLAYER_IR_DATA_RANGES,
				  (void*)&(this->ersp_data.ir_range),
				  sizeof(player_ir_data_t),
				  NULL);

	if (ir_binary_subscriptions &&
		memcmp(&ersp_data.ir_binary, &ersp_prev.ir_binary, sizeof(ersp_data.ir_binary)))
	this->Publish(this->ir_binary_id, NULL,
				  PLAYER_MSGTYPE_DATA,
				  PLAYER_BUMPER_DATA_STATE,
				  (void*)&(this->ersp_data.ir_binary),
				  sizeof(player_bumper_data_t),
				  NULL);
}
