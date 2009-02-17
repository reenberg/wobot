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


	// libplayerc++ throws a PlayerError exception when
	// it runs into trouble
	// The PlayerError is often more readable...
	try
	{
		PlayerClient robot(gHostname, gPort);
		Position2dProxy pp(&robot, gIndex);

		// Time settings
		timespec sleep_time = { 4, 0 };

		// Speed and turn settings
		double turnrate = dtor(-21);
		double speed = 0.20;

		pp.SetCarlike(speed, 0);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(speed, turnrate);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(speed, 0);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(speed, turnrate);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(speed, 0);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(speed, turnrate);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(speed, 0);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(speed, turnrate);
		nanosleep(&sleep_time, NULL);

		pp.SetCarlike(0, 0);

	} //end try
	catch (PlayerCc::PlayerError e)
	{
		std::cerr << e << std::endl;
		return -1;
	} //end catch
} //end main
