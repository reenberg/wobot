#include <libplayerc++/playerc++.h>
#include <iostream>
#include <args.h>
#include <scorpion.h>
using namespace PlayerCc;

int main(int argc, char** argv)
{
	// Parse input arguments
	parse_args(argc,argv);

	// libplayerc++ throws a PlayerError exception when
	// it runs into trouble
	// The PlayerError is often more readable...
	try
	{
		PlayerClient    robot(gHostname, gPort);
		Position2dProxy pp(&robot, gIndex);
		IrProxy ir(&robot, gIndex);
		BumperProxy bp(&robot, gIndex);
		BumperProxy irb(&robot, gIndex+1); // binary IR sensors acts like bumpers
#ifdef POWER
		PowerProxy pow(&robot, gIndex);
#endif
		/* Go into read-think-act loop */

		for(;;)
		{ 
			// Read from proxies
			robot.Read();

			printf("=== %d IR range sensors ===\n", ir.GetCount());
			// Print infared sensor readings
			printf("=== %d IR range sensors ===\n", ir.GetCount());
			for (int i = 0; i < ir.GetCount(); i++) {
				printf("%-10s -> %f\n", scorpion_ir_range_name(i), ir.GetRange(i));
			}

			// Print if bumper is bumped
			printf("=== %d Bump sensors ===\n", bp.GetCount());
			for (int i = 0; i < bp.GetCount(); i++) {
				printf("%-10s bumper reads %i\t", scorpion_bumper_front_name(i), bp.IsBumped(i));
				if (bp.IsBumped(i)) 
					printf(" and was therefore BUMPED\n");
				else 
					printf(" and was NOT bumped\n");
			}

			// Print infared binary sensor readings
			// Notice they acts like bumper sensors
			printf("=== %d IR binary sensors ===\n", irb.GetCount());
			for (int i = 0; i < irb.GetCount(); i++) {
				printf("%-10s ir binary sensor reads %i\t", scorpion_bumper_ir_name(i), irb.IsBumped(i));
				if (irb.IsBumped(i)) 
					printf(" and was therefore ACTIVATED\n");
				else 
					printf(" and was NOT activated\n");
			}

#ifdef POWER 
			// Powerinterface reading
			printf("=== Powerintface ===\n");
			printf("Battery charge reads: %d\n", scorpion_power_battery_name(1), pow.GetCharge());
#endif			
		} // end for-loop
	} // end try
	catch (PlayerCc::PlayerError e)
	{
		std::cerr << e << std::endl;
		return -1;
	}
}
