/*
 * The bump-stop example from the user manual.
 */

#include <libplayerc++/playerc++.h>
#include <scorpion.h>
using namespace PlayerCc;

int
main(int argc, char *argv[])
{
	PlayerClient robot("localhost");
	Position2dProxy position(&robot);
	BumperProxy bumper(&robot);

	robot.SetDataMode(PLAYER_DATAMODE_PULL);
	robot.SetReplaceRule(-1, -1, PLAYER_MSGTYPE_DATA, -1, 1);

	while (true) {
		robot.Read();

		if (bumper.IsAnyBumped()) {
			position.SetSpeed(0, 0);
			sleep(1);
		} else {
			position.SetSpeed(0.10, 0);
		}
	}

	return 0;
}
