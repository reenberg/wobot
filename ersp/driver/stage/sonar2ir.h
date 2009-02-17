/*
 * Sonar2IR Player driver
 */

#ifndef SONAR2IR_H
#define SONAR2IR_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#include <sys/fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <libplayercore/playercore.h>

class Sonar2IR : public Driver 
{
  private:
	// State data
	player_devaddr_t ir_id, sonar_id;
	player_ir_data_t ir_data;
	player_ir_pose_t ir_pose;
	player_sonar_power_config_t sonar_power;
	Device *sonar;
	bool republish_data, republish_pose, republish_geom, republish_power;

	// Publish state data.
	void PutData(void);

  public:
	Sonar2IR(ConfigFile *cf, int section);
	~Sonar2IR(void);

	// Thread life-cycle
	void Main();
	int Setup();
	int Shutdown();

	// Message handling
	int Subscribe(player_devaddr_t id);
	int Unsubscribe(player_devaddr_t id);
	int ProcessMessage(MessageQueue *queue, player_msghdr *msghdr, void *data);
};

#endif
