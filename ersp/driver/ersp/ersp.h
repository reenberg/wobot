/*
 * ERSP  Player driver 
 */

/*
 * This driver is for interacting with the "ERSP" robots, made by Evolution
 * Robotics.    
 *
 * This code is inspired by the player module of ER1 and P2OS; thanks to the
 * authors of these modules.
 */

#ifndef ERSP_ERSP_H
#define ERSP_ERSP_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <stdlib.h>  /* for abs() */

#include <libplayercore/playercore.h>

#ifdef HAVE_ERSP
#include <evolution/Resource.hpp>
#endif

#include <scorpion.h>

/*
 * Relevant types and constants for the "ERSP" robots, made by Evolution
 * Robotics.    
 */

#define IR_MAX_RANGE 0.80

// The main driver device interface data
typedef struct
{
	player_position2d_data_t position;
	player_bumper_data_t bumper;
	player_ir_data_t ir_range;
	player_bumper_data_t ir_binary;
	player_power_data_t power;
} __attribute__ ((packed)) player_ersp_data_t;

class ERSP : public Driver 
{
  private:

	////////////////////////////////////////////////////////////
	//	Generic driver
	////////////////////////////////////////////////////////////

	player_ersp_data_t ersp_data;
	player_ersp_data_t ersp_prev;
	bool running;

	// Position state data
	player_devaddr_t position_id;
	int position_subscriptions;

	// Bumper state data
	player_devaddr_t bumper_id;
	int bumper_subscriptions;

	// IR range state data
	player_devaddr_t ir_range_id;
	int ir_range_subscriptions;

	// IR binary state data
	player_devaddr_t ir_binary_id;
	int ir_binary_subscriptions;

	// Battery state data
	player_devaddr_t power_id;
	int power_subscriptions;

	// Handle incoming configuration messages.
	int HandleConfig(MessageQueue *queue, player_msghdr *hdr, void *data);

	// Handle incoming command messages.
	int HandleCommand(player_msghdr *hdr, void *data);

	// Publish state data.
	void PutData(void);

	////////////////////////////////////////////////////////////
	//	Evolution
	////////////////////////////////////////////////////////////

#ifdef HAVE_ERSP
	Evolution::ResourceManager *manager;
	Evolution::IResourceContainer *container;
#endif
	union {
#ifdef HAVE_ERSP
		Evolution::IDriveSystem *driver;
		Evolution::IAvoidance *avoid;
		Evolution::IBattery *battery;
		Evolution::IBumpSensor *bumper;
		Evolution::IRangeSensor *range;
#endif
		void *handle;
	} devices[ERSP_DEVICES];
	const struct ersp_dev *devs;

	int SetupERSP();
	void set_motor_velocity(double velocity, double angular_velocity);

  public:
	ERSP(ConfigFile *cf, int section);
	~ERSP(void);

	// Thread life-cycle
	virtual void Main();
	virtual int Setup();
	virtual int Shutdown();

	// Message handling
	virtual int Subscribe(player_devaddr_t id);
	virtual int Unsubscribe(player_devaddr_t id);
	virtual int ProcessMessage(MessageQueue *queue,
							   player_msghdr *msghdr,
							   void *data);
};

#endif
