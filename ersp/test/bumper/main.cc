#include <iostream>
#include <stdio.h>
#include <libplayerc++/playerc++.h>
#include <scorpion.h>
#include <args.h>
using namespace PlayerCc;

int
main(int argc, char *argv[])
{
	// Parse input arguments
  parse_args(argc,argv);
	
	timespec move_sleep = { 5, 0 };
	timespec turn_sleep = { 3, 0 };
	timespec stop_sleep = { 10, 0 };

  // libplayerc++ throws a PlayerError exception when
  // it runs into trouble
	// The PlayerError is often more readable...
	try
	{
		PlayerClient robot(gHostname, gPort);
		Position2dProxy pp(&robot, gIndex);
		BumperProxy bp(&robot, gIndex);

		// Using pull datamode, so we only get newest data
		// Replacerule make old data being overwritten
		robot.SetDataMode(PLAYER_DATAMODE_PULL);
		robot.SetReplaceRule(-1, -1, PLAYER_MSGTYPE_DATA, -1, 1);
	  
		// Speed and turn settings
		double turn_rate = 0.5;
		double move_speed = 0.10;
	
		
		/* Go into read-think-act loop */
		while (true) {
			// read from the proxies
			robot.Read();
	
			// Stop if bumpsensors not activated
			if (!bp.IsAnyBumped()) {
				printf("Nothing ... waiting for bump\n");
				pp.SetSpeed(0, 0);
			}
			
			// depending on the "bumpside" avoid the bump
			if (bp.IsBumped(SCORPION_BUMP_BNE) && bp.IsBumped(SCORPION_BUMP_BNW)) 
			{ // both bumped
				printf("Both bumpsensors bumped \n");
				pp.SetSpeed(-move_speed, 0);					// go backwards
				nanosleep(&move_sleep, NULL);					
				nanosleep(&move_sleep, NULL);					
				pp.SetSpeed(0, 0);										// stop
			} 
			else if (bp.IsBumped(SCORPION_BUMP_BNW))  
			{	//left bump
				printf("Left bump!\n");
				pp.SetSpeed(-move_speed, 0);					// go backwards
				nanosleep(&move_sleep, NULL);
				pp.SetSpeed(-move_speed, -turn_rate); // go backwards turning left 
				nanosleep(&turn_sleep, NULL);
				pp.SetSpeed(0, 0);										// stop
			} 
			else if (bp.IsBumped(SCORPION_BUMP_BNE)) 
			{ // right bump
				printf("Right bump!\n");
				pp.SetSpeed(-move_speed, 0);					// go backwards
				nanosleep(&move_sleep, NULL);					
				pp.SetSpeed(-move_speed, turn_rate);	// go backwards turning right
				nanosleep(&turn_sleep, NULL);
				pp.SetSpeed(0, 0);										// stop
			}
		}
	}
    catch (PlayerCc::PlayerError e)
   {
    std::cerr << e << std::endl;
    return -1;
   }
}
