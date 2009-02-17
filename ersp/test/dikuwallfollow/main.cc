/* ************************************************************ 
 * dikuwallfollow
 * Simple wall follow program
 * Written to follow a wall a DIKU modelled in Stage
 * (for the Evolution Robotics Scorpion robot)
 *
 * Based on "wallfollow.c" from the Player project examples
 * 
 * Jonas Fonseca <fonseca@diku.dk>
 * Bue Petersen <buep@diku.dk>
 * Department of Computer Science University of Copenhagen
 * January 2007
 * ************************************************************/
#include <iostream>
#include <stdint.h>
#include <libplayerc++/playerc++.h>

// states
const uint WALL_FOLLOWING = 0;
const uint COLLISION_AVOIDANCE = 1;
const uint START = 2;
const uint NONE = 0;
const uint LEFT = 1;
const uint RIGHT = 2;

// parameters
const double VEL       = 0.3; // normal_advance_speed
const double DIST      = 0.60; // preferred_wall_following_distance
const double TURN_RATE = 30; // maximal_wall_following_turnrate
const double STOP_DIST = 0.45; // stop_distance
const double STOP_ROT  = 50; // stop_rotation_speed
const double MAXRANGE  = 0.8; //  Sensor read from [0.1;0.8]meters

int
main(int argc, char *argv[])
{
  using namespace PlayerCc;

  PlayerClient    robot("localhost");
  Position2dProxy pp(&robot, 0);
  IrProxy ir(&robot, 0);
  robot.SetDataMode(PLAYER_DATAMODE_PULL);
  robot.SetReplaceRule(-1, -1, PLAYER_MSGTYPE_DATA, -1, 1);
	
	// Stores for IR range sensor data
	int ircount;
	for(int i=0;i<10;i++){	
		// ir.GetCount function need some iterations  - huhh? bug?
		robot.Read();
		ircount = ir.GetCount();
	}
	double r[ircount];
	double rnorm[ircount]; // normalized sensor data

	// initialization
  bool escape_direction;
  uint previous_mode = START;
	uint wall = NONE;
  uint previous_wall = wall;
	double dist_norm = DIST/MAXRANGE;
	
	for (;;)
  {
  	double speed = VEL;
  	double turnrate = 0;
		previous_wall = wall;
    
		// read from the proxies
    robot.Read();
		for (int i = 0; i < ircount; i++) {
			r[i] = ir.GetRange(i);
			// scale and normalization
			rnorm[i] = ((r[i]-0.1)/(MAXRANGE-0.1));
		}
    std::cout << "------------------------------------------------------\n" 
							<< " previous_mode: "	<< previous_mode
							<< " previous_wall: "   << previous_wall
							<< "\n Left (bs_w): "   << r[9]
              << " Front (bn_n): " << r[2]
              << " Right (bs_e): " << r[10]
              << std::endl;

    // Just start following nearest wall
		double left = ((rnorm[9]+rnorm[1])/2); // normalized distances
		double right = ((rnorm[10]+rnorm[0])/2);
	  if (right < left) {
			// wall on the right
			// To turn/from the wall based upon distance
			turnrate = dtor(-TURN_RATE*(right - dist_norm));
			wall = RIGHT;
		} else if (left < right) {
			// wall on the left
			turnrate = dtor(TURN_RATE*(left - dist_norm));
			wall = LEFT;
		} else {
			turnrate = dtor(0);
			wall = NONE;
		}
		std::cout << "\n left: " << left
							<< " right: " << right
              << " dist_norm: " << dist_norm
              << std::endl;
		
		// The above nearest wall strategi wont work if we miss the wall
		// as it will make the robot drive straight
		// This is ok if we never have seen a wall, but not if previous
		// mode was wallfollowing.
		if ((previous_mode == WALL_FOLLOWING) && wall == NONE) {
    	std::cout << " ooops missed wall!!!" << std::endl;
			if (previous_wall == RIGHT) {
				turnrate = dtor(-TURN_RATE);
				wall = previous_wall;
			} else if (previous_wall == LEFT) {
				turnrate = dtor(TURN_RATE);
				wall = previous_wall;
		  }
		}


		// avoid collision: find closest range in the collision avoidance field of
    // view, calculate statistical mean to select escape direction
		double min_dist_norm = 1.0;
    double left_mean = (rnorm[9]+rnorm[1]+rnorm[7]+rnorm[4])/4;
    double right_mean = (rnorm[10]+rnorm[0]+rnorm[6]+rnorm[3])/4;
    double front_mean = (rnorm[8]+rnorm[2]+rnorm[5])/3;

		if (left_mean < right_mean) min_dist_norm = left_mean; else min_dist_norm = right_mean;
		if (front_mean < min_dist_norm) min_dist_norm = front_mean;
		
		double min_dist = min_dist_norm*(MAXRANGE-0.1);
		
    std::cout << "\n left_mean : "  << left_mean
              << " front_mean: " 		<< front_mean
              << " right_mean: "		<< right_mean
							<< " min_dist_mean: "	<< min_dist_norm
              << " min_dist: " << min_dist
							<< std::endl;

    if (min_dist < STOP_DIST)
    {
      speed = 0;
      // selection of escape direction (done once for each object encounter)
      if (previous_mode == WALL_FOLLOWING)
      {
        // go towards direction with most open space
        escape_direction = left_mean < right_mean;
        // change this so that we know we have chosen the escape direction
        previous_mode = COLLISION_AVOIDANCE;
      }
      
      if (escape_direction) // right turn
        turnrate = dtor(-STOP_ROT);
      else // left turn
        turnrate = dtor(STOP_ROT);
    }
    else
      previous_mode = WALL_FOLLOWING;

    std::cout << "\n turnrate: "	<< turnrate 
							<< " speed: " 		<< speed
							<< std::endl;
    // command the motors
		// SAFETY control of speed
		if (speed > VEL) speed = VEL;
    pp.SetSpeed(speed, turnrate);
  }
}
