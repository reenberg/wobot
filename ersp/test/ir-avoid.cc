/*
 * The ir-avoid example from the user manual.
 */

#include <libplayerc++/playerc++.h>
#include <scorpion.h>
using namespace PlayerCc;

PlayerClient robot("localhost");
Position2dProxy position(&robot);
IrProxy ir(&robot);

int
main(int argc, char *argv[])
{
	while (true) {
		robot.Read();

		if (ir[SCORPION_IR_TW_NNW] < 0.50 ||
		    ir[SCORPION_IR_BN_N] < 0.50 ||
		    ir[SCORPION_IR_TE_NNE] < 0.50) {
			position.SetSpeed(0.0, -0.5);
			continue;
		}

		position.SetSpeed(0.10, 0.0);
	}

	return 0;
}
