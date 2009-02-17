/** Scorpion robot configuration
 *
 * This file defines IDs that can be used by Player programs when
 * accessing data from sensor arrays, such as the bumper and range
 * sensors. It also defines internal IDs used by the driver for
 * refering to the various Scorpion devices. To make the interface
 * for Player programs simpler the "exported" IDs closely mirror
 * the IDs of the robot by using the following syntax:
 *
 *       SCORPION_<robot_id>
 *
 * Examples of defined IDs are:
 *
 *       SCORPION_BUMP_BNW    - The bump sensor aka "Bump_bnw"
 *       SCORPION_IR_TN_WDOWN - The binary IR sensor aka "IR_tn_wdown"
 *
 * @note This file should keep free of include dependencies such as
 *       ersp.h, so that test programs can easily include it.
 */

#ifndef ERSP_SCORPION_H
#define ERSP_SCORPION_H

#define ERSP_INCLUDE "scorpion.inc"

#define SCORPION_(ID, TYPE, NAME) \
        ERSP_DEV(SCORPION, ID, TYPE, NAME)

#define SCORPION_INTERFACE(TYPE, ID) \
        ERSP_INTERFACE(scorpion, TYPE, ID)

#define SCORPION_END ERSP_INTERFACE_END

#include "ersp.inc"

#endif
