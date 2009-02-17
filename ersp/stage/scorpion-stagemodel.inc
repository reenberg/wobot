# Scorpion Robot model for Stage
# ...Include file for Stage World File

# =========================================================================== #
# A model describing a Evolution Robotics Scorpion robot
#
# Use this file for inclusion in Stage World files when simulating 
#  Scorpion robots
#

# Use this include in your Stage World file:
#	include "../scorpion-stagemodel.inc"
# Then create an object of the "scorpionrobot" model after the include:
#	scorpionrobot
#	(
#		name "scorpionrobot1"
#		pose [-2.000 -1.500 0.000]
#	)
# ... of course changing the name and start position to your own choices

# Jonas Fonseca <fonseca@diku.dk>
# Bue Petersen <buep@diku.dk>
# Department of Computer Science University of Copenhagen
# January 2007
# =========================================================================== #

# The Evolution Robotics Scorpion Robot has ir sensors, digital sensor and a bumper device.
# The robot itself is described below, but its devices (sensors and bumper) are
#  described in other files included here below.
# The creation of the devices on the robot is done last in the robot
#  description.
# The sensor and bumper are models as well as the robot itself.

# Include device description from other files
# Makes them reuseable and save space in this file.
# Notice path relative to the Stage Configuration file
include "../scorpion-ir-range-stagemodel.inc"
# Stage has no binary sensor models and they are not needed in 2D
#include "../scorpion-ir-binary-stagemodel.inc"
# Bumpers not supported in Stage>=1.5
#include "../scorpion-bumper-stagemodel.inc"


# The model of the Scorpion robot defined from the position model
define scorpionrobot position
(
  # - Properties of the "position" model -
  # The Scorpion have differential steering (a seperate motor for each wheel)
  drive "diff"

  # - Robots localization -
  # If "gps" the position model reports its position with perfect accuracy. 
  # If "odom", a simple odometry model is used and position data drifts from
  #  the ground truth over time. The odometry model is parameterized by the
  #  odom_error property. 
  # As standard use a precise odometry in Stage because we often prototype
  # It can be overwritten when the robot object is created
  localization "gps"


  # - Properties of general model ("model)" -
  # This specify the pose of the model in its parent's coordinate system
  # As ERSP world coordinate system and the robots coordinate system is
  #  coincident when turning the robot on we set this to origo
  # Typically the this start position is overwritten when object is created
  pose [0 0 0]
  # Size of the Scorpion robot in 2D projection on the floor is:
  # 43,9 cm (length) and 40,6 cm (width)
  size [0.44 0.41]
  # The Scorpion robot's center - coincident with origo the robot's 
  #  coordinate system as ERSP uses. It is -6 cm a the X-axis
  # Heading is not used
  origin [-0.06 0.0 0]
  # Velocity of the model is zero at start
  velocity [0 0 0]
  
  # Choosing from X11 database (rgb.txt) looks darker gray
  color "DarkGray"

  # The followin lines determine how the model appears in various sensors
  
  # Our robot should collide with other models that have this property set
  obstacle_return 1
  
  # Even though Scorpion robots don't have laser they can be seen by laser
  laser_return 1
  
  # ... it will also be detected by ranger sensors
  ranger_return 1
  
  # ... if other robots have a gripper this can "grap" the robot
  gripper_return 1

  # - GUI properties of the general model ("model") -
  # Draw a nose on the model so we can see where the bumper is placed
  gui_nose 1
  
  # Scaling grid over the robot is not used. Typically the map model has it
  gui_grid 0
  
  # Allow to move the model with a mouse in the GUI window
  gui_movemask 1
  
  
  # - Unit square body shape properties of the general model ("model") -
  # This polygon approximates the shape of the robot
  polygons 1
  polygon[0].points 4
  polygon[0].point[0] [  0.06  0.21 ]
  polygon[0].point[1] [  0.06 -0.21 ]
  polygon[0].point[2] [ -0.38 -0.21 ]
  polygon[0].point[3] [ -0.38  0.21 ]

  # - Devices on the robots -
  # Create robots sensors and bumper - they are models as well.
  # They are describe above (included from other files) but first created here
  scorpionirrangesensors()
  # Binary sensors not supported! See above
 #scorpionirbinarysensors()
  # Bumper not supported! See above
 #scorpionbumper()
)
