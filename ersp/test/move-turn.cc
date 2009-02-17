/*
 * The move-turn example from the user manual.
 */

#include <libplayerc++/playerc++.h>
using namespace PlayerCc;

PlayerClient robot("localhost");
Position2dProxy position(&robot);

int
main(int argc, char *argv[])
{
	position.SetSpeed(0.20, 0.0);
	sleep(3);

	position.SetSpeed(0.0, -0.5);
	sleep(3);

	position.SetSpeed(0.0, DTOR(30));
	sleep(3);

	position.SetSpeed(0.0, 0.0);

	return 0;
}
