/* ************************************************************ 
 * randomwalk
 * Modified for the for the Evolution Robotics Scorpion robot
 *
 * Based on "randomwalk" from the Player project examples
 * 
 * Jonas Fonseca <fonseca@diku.dk>
 * Bue Petersen <buep@diku.dk>
 * Department of Computer Science University of Copenhagen
 * January 2007
 * ************************************************************/
#include <libplayerc++/playerc++.h>
#include <iostream>
#include <args.h>
#include <scorpion.h>
using namespace PlayerCc;

int main(int argc, char** argv)
{
  // Parse input arguments
  parse_args(argc,argv);

	/* Setting values for driving and avoiding */
	// Allowed minimum distances to obstacles (meters)
	double mindistance = 0.70;
	
	// Speed and turn settings
	double speed = 0.200;
	double avoidspeed = 0;
	double turnrate = DTOR(40);
	
	// Changing speed and turn when avoiding
	double newturnrate=0.0f, newspeed=0.0f;
	
	// avoidcount counts for how many iteration the avoid face takes
	// start at 0, but when avoiding is set to AVOIDCOUNT.
	#define AVOIDCOUNT 15
	int avoidcount = 0;
	// True if obstacles closer than mindistance in the front
	bool obs = false;
	
	// Randomization integers
	int randint, randint2;
	int randcount = 0;
	// This set how for how many iteration robot drives with a certain turnrate
	// when there is no obstacles
	#define RANDCOUNT 20
	
  // libplayerc++ throws a PlayerError exception when
  // it runs into trouble
	// The PlayerError is often more readable...
	try
	{
	  PlayerClient    robot(gHostname, gPort);
	  Position2dProxy pp(&robot, gIndex);
	  IrProxy ir(&robot, gIndex);
	  
		/* Go into read-think-act loop */
	  for(;;)
	  { 
			// read from proxies
	    robot.Read();
		
			/* See if there is an obstacle in front */
	    obs = (
							// Front ?
							(ir.GetRange(SCORPION_IR_BN_N) < mindistance) ||
						 	// On top of wheels looking forward 
							// ... from left to the middle
							(ir.GetRange(SCORPION_IR_TW_NNE) < mindistance) ||
		   				// ... from right to the middle
							(ir.GetRange(SCORPION_IR_TE_NNE) < mindistance) ||
							// In the bumpers left side looking forward to the right
							(ir.GetRange(SCORPION_IR_BN_NE) < mindistance) ||
							// In the bumpers right side looking forward to the left
							(ir.GetRange(SCORPION_IR_BN_NW) < mindistance) ||
	           	// Behind the wheels looking forward ...
							// ... left side looking little left to outside of the robot
							(ir.GetRange(SCORPION_IR_TW_NNW) < mindistance) ||
							// ... right side looking little right to outside of the robot
							(ir.GetRange(SCORPION_IR_TE_NNE) < mindistance));
			
			// Print sensor data detecting the obstacles
			if(obs && gDebug)
			{
				printf("=== FRONT === one of the %i sensors measure less than %f meters\n", ir.GetCount(), mindistance);
				for (int i = 0; i < ir.GetCount(); i++) {
					double measure = ir[i];
					if (measure < mindistance)
						printf("%-10s -> %f\n", scorpion_ir_range_name(i), measure);
				}
			}
			
			/* Start or continue avoiding */
	    if(obs || avoidcount ) // obstacles found phase...
			{
	    	newspeed = avoidspeed;
	
	      // Once we start avoiding, continue avoiding for AVOIDCOUNT iterations
	      if(!avoidcount)
	      {
	        avoidcount = AVOIDCOUNT;
	        randcount = 0;
					
					// If obstacles - turn the way in which there is the "most space"
					if(obs) 
					{
	        	// Less space left than right?
		  			if(	ir.GetRange(SCORPION_IR_BS_W) + ir.GetRange(SCORPION_IR_BN_WNW) 
								< 
								ir.GetRange(SCORPION_IR_BS_E) + ir.GetRange(SCORPION_IR_BN_ENE))
						{
	          	newturnrate = -turnrate; // more space right, then turn right
							if(gDebug) printf("=== LEFT === sensors measure less space on the left:\n");
							if(gDebug) printf("%-10s -> %f\n", scorpion_ir_range_name(SCORPION_IR_BS_W), ir.GetRange(SCORPION_IR_BS_W));
							if(gDebug) printf("%-10s -> %f\n", scorpion_ir_range_name(SCORPION_IR_BN_WNW), ir.GetRange(SCORPION_IR_BN_WNW));
						}
						else
						{
	            newturnrate = turnrate;  // else turn left
							if(gDebug) printf("=== RIGHT === sensors measure less space on the right:\n");
							if(gDebug) printf("%-10s -> %f\n", scorpion_ir_range_name(SCORPION_IR_BS_E), ir.GetRange(SCORPION_IR_BS_E));
							if(gDebug) printf("%-10s -> %f\n", scorpion_ir_range_name(SCORPION_IR_BN_ENE), ir.GetRange(SCORPION_IR_BN_ENE));
						}
					}
				}
	      avoidcount--;
	    } // end obstacle found phase
	    else //no obstacles phase
	    {
	      avoidcount = 0;
	      newspeed = speed;
	
	      // Random walk -  update turnrate every RANDCOUNT iteration seconds
	      if(!randcount)
	      {
	        // make random int tween -20 and 20
	        randint = rand() % 41 - 20;
	
	        newturnrate = dtor(randint);
	        randcount = RANDCOUNT;
	       }
	       randcount--;
	    } // end no obstacles phase
			
			// End thinking about what to do...
	    // write commands to robot
	    pp.SetSpeed(newspeed, newturnrate);
	  }
	}
	catch (PlayerCc::PlayerError e)
  {
    std::cerr << e << std::endl;
    return -1;
  }
}
