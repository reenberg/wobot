/*
 * HDAPS Player driver
 *
 * This driver is for interacting with IBM's Hard Drive Active
 * Protection System available on various ThinkPads models.
 */

#ifndef HDAPS_H
#define HDAPS_H

#include <libplayercore/playercore.h>

class HDAPS : public Driver 
{
  private:
	// Joystick state data
	player_devaddr_t joystick_id;
	player_joystick_data_t data, prev_data;

	// Publish state data.
	void PutData(void);

  public:
	HDAPS(ConfigFile *cf, int section);
	~HDAPS(void);

	// Thread life-cycle
	virtual void Main();
	virtual int Setup();
	virtual int Shutdown();

	// Message handling
	virtual int Subscribe(player_devaddr_t id);
	virtual int Unsubscribe(player_devaddr_t id);
	virtual int ProcessMessage(MessageQueue *queue, player_msghdr *msghdr, void *data);
};

#endif
