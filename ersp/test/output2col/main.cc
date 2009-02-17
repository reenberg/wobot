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
		/* Go into read-think-act loop */

		bool firsttime = true;
		for(;;)
		{ 
			// Read from proxies
			robot.Read();

			// Print infared sensor readings
			if (firsttime) {
				for (int i = 0; i < 5; i++) {
					// delayed update of count fix!
					int tmp;
					double tmp2;
					robot.Read();
					tmp = ir.GetCount();
					tmp2 = ir.GetRange(i);
				}
				printf("Number of IR range sensors %i:\n", ir.GetCount());
				for (int i = 0; i < ir.GetCount(); i++) {
					printf("%-10s\t", scorpion_ir_range_name(i));
				}
				printf("\n");
				firsttime = false;
			}
			for (int i = 0; i < ir.GetCount(); i++) {
				printf("%f\t", ir.GetRange(i));
			}

			std::cout << std::endl; 			
			sleep(1);

		} // end for-loop
	} // end try
	catch (PlayerCc::PlayerError e)
	{
		std::cerr << e << std::endl;
		return -1;
	}
}
