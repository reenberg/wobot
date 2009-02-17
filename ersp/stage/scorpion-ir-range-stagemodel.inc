# Scorpion Robot infrared sensors model for Stage
# ...Include file for the Scorpion Robot model for Stage

# =========================================================================== #
# A model describing a Evolution Robotics Scorpion robot infrared sensors
#
# This file is included in Scorpion Robot model for Stage, which again is 
#  included in the Stage World files when simulation Scorpion robots

# The Scorpion Robot model for Stage included this file first with::
#	include "../scorpion-ir-ranger-stagemodel.inc"
# Then it uses it in the model as: 
#	define scorpionrobot position
#		(
#...
#		scorpionirrangesensors()
#...
#		)

# Jonas Fonseca <fonseca@diku.dk>
# Bue Petersen <buep@diku.dk>
# Department of Computer Science University of Copenhagen
# January 2007
# =========================================================================== #


# A model of the Scorpion robots infrared sensor defined as a ranger model
define scorpionirrangesensors ranger
(
  # There is 13 Sharp GP2D12 IR sensor on the Scorpion
  scount 13

  # Define the position of each sensor:
  # spose[index] [xpos ypos heading]
  # The position is relative to its parents - that is the robots coordinate
  #  system because this model is intended to be included in the robot model
  # The Scorpion robot documentation also state placement of sensor in
  #  the robot coordinate system.
  
  # Index matches the defintion in the Player driver for the robot
  
  # Scorpion Device ID: IR_bn_ene
  # Position (cm): x=12.6 y=9,2 z=7.1 cm
  # Angles (rad): Roll=3.142, Pitch=0, Yaw=-1.134
  spose[0] [ 0.126 0.092 -64.973 ] 
  # Scorpion Device ID: IR_bn_wnw
  # Position (cm): x=12.6 y=-9,2 z=7.1 cm
  # Angles (rad): Roll=0, Pitch=0, Yaw=1.134
  spose[1] [ 0.126 -0.092 64.973 ]
  # Scorpion Device ID: IR_bn_n
  # Position (cm): x=4.9 y=0 z=7.2 cm
  # Angles (rad): Roll=0, Pitch=0, Yaw=0
  spose[2] [ 0.049 0.000 0 ]
  # Scorpion Device ID: IR_bn_ne
  # Position (cm): x=9.2 y=6.4 z=7.2 cm
  # Angles (rad): Roll=3.142, Pitch=0, Yaw=-0.611
  spose[3] [ 0.092 0.064 -35.008 ] 
  # Scorpion Device ID: IR_bn_nw
  # Position (cm): x=9.2 y=-6.4 z=7.2 cm
  # Angles (rad): Roll=0, Pitch=0, Yaw=0.611
  spose[4] [ 0.092 -0.064 35.008 ] 
  # Scorpion Device ID: IR_te_nnw
  # Position (cm): x=1.6 y=-17.3 z=11.4 cm
  # Angles (rad): Roll=3.142, Pitch=0, Yaw=0.175
  spose[5] [ 0.016 -0.173 10.027 ] 
  # Scorpion Device ID: IR_te_nne
  # Position (cm): x=-6.5 y=-16.9 z=14.6 cm
  # Angles (rad): Roll=1.571, Pitch=0, Yaw=-0.262
  spose[6] [ -0.065 -0.169 -15.011 ] 
  # Scorpion Device ID: IR_tw_nnw
  # Position (cm): x=-6.5 y=16.9 z=14.6 cm
  # Angles (rad): Roll=-1.571, Pitch=0, Yaw=0.262
  spose[7] [ -0.065 0.169 15.011 ] 
  # Scorpion Device ID: IR_tw_nne
  # Position (cm): x=1.6 y=17.3 z=11.4 cm
  # Angles (rad): Roll=0, Pitch=0, Yaw=0.-175
  spose[8] [ 0.016 0.173 -10.027 ] 
  # Scorpion Device ID: IR_bs_w
  # Position (cm): x=-17.2 y=1.9 z=8.7 cm
  # Angles (rad): Roll=3.142, Pitch=0, Yaw=1.571
  spose[9] [ -0.172 0.019 90.011 ] 
  # Scorpion Device ID: IR_bs_e
  # Position (cm): x=-17.2 y=-1.9 z=8.7 cm
  # Angles (rad): Roll=3.142, Pitch=0, Yaw=-1.571
  spose[10] [ -0.172 -0.019 -90.011 ] 
  # Scorpion Device ID: IR_bw_s
  # Position (cm): x=-7.7 y=6.9 z=8.7 cm
  # Angles (rad): Roll=3.142, Pitch=0, Yaw=3.054
  spose[11] [ -0.077 0.069 174.981 ] 
  # Scorpion Device ID: IR_be_s
  # Position (cm): x=-7.7 y=-6.9 z=8.7 cm
  # Angles (rad): Roll=3.142, Pitch=0, Yaw=-3.054
  spose[12] [ -0.077 -0.069 -174.981 ] 

  
  # Define the field of view of each transducer:
  # sview [range_min range_max view_angle]
  # But instead of doing it for every sensor, we do it for all by omitting
  #  the index as in: ssize[3] [0.01 0.05]
  # The Sharp GP2D12 sensor detects distance from 10 to 80 cm (according to 
  #  Sharp documentation)
  # View angle is unsure, but some says only 5 degree
  sview [0.10 .80 5]

  
  # Define the size of each sensor [xsize ysize] in meters
  # If all have same size this could be done with eg.: ssize [0.01 0.05]
  # The Sharp GP2D12 sensor dimension is (according to Sharp documentation):
  # (looking into the lens)
  # Width: 	40.2 mm (37 mm + 0.5*2*3.2mm)
  # Depth: 	13.5 mm
  # Height: 	13.0 mm
  ssize [0.013 0.040]
  
  # Not all sensors have the same size as defined above.
  # It would be more correct to overwrite these to sensors sizes with the
  #  correct size- but it won't work with Stage though is should according
  #  to the documentation.
   
  # These sensors is "standing", so we set different sizes
 #ssize[6] [ 0.013 0.0135 ]
 #ssize[7] [ 0.013 0.0135 ]
  
)

