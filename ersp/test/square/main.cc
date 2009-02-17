#include <iostream>
#include <stdio.h>
#include <libplayerc++/playerc++.h>
#include <args.h>
using namespace PlayerCc;

int
main(int argc, char *argv[])
{
	// Parse input arguments
	parse_args(argc,argv);

	try
	{
		PlayerClient robot(gHostname, gPort);
		Position2dProxy pp(&robot, gIndex);

		// Speed and turn settings
		double turn_rate = 0.5;
		double move_speed = 0.20;
		
		timespec move_sleep = { 5, 0 };
		timespec turn_sleep = { 3, 0 };
		timespec stop_sleep = { 10, 0 };
	
		// move 1, turn 1
		pp.SetSpeed(move_speed, 0);
		nanosleep(&move_sleep, NULL);
		pp.SetSpeed(0, turn_rate);
		nanosleep(&turn_sleep, NULL);
	
		// move 2, turn 2
		pp.SetSpeed(move_speed, 0);
		nanosleep(&move_sleep, NULL);
		pp.SetSpeed(0, turn_rate);
		nanosleep(&turn_sleep, NULL);
	
		// move 3, turn 3
		pp.SetSpeed(move_speed, 0);
		nanosleep(&move_sleep, NULL);
		pp.SetSpeed(0, turn_rate);
		nanosleep(&turn_sleep, NULL);
	
		// move 4, turn 4
		pp.SetSpeed(move_speed, 0);
		nanosleep(&move_sleep, NULL);
		pp.SetSpeed(0, turn_rate);
		nanosleep(&turn_sleep, NULL);

		// Set motor stop command and wait so they can propagate
		pp.SetSpeed(0, 0);
		nanosleep(&stop_sleep, NULL);
	} //end try
	catch (PlayerCc::PlayerError e)
	{
		std::cerr << e << std::endl;
		return -1;
	}
}
