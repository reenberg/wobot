# Map model for Stage
# ...Include file for Stage World File

# =========================================================================== #
# A Basic map model - for inclusion in Stage World files
#
# ... include this file in your Stage World file and create the model.
# The map model is based on the Simple example from the official P/S examples.

# Use this include in your Stage World file:
#	include "../basicmap-model-model.inc"
# Then create the object in your Stage World file (after the include):
#	basicmap
#	(
#		name "Basicmap"
#  		bitmap "cave.png"
#  		size [16 16]
#	)
# ... of course you could change these parameters and paint your own bitmap
#  and use.

# Jonas Fonseca <fonseca@diku.dk>
# Bue Petersen <buep@diku.dk>
# Department of Computer Science University of Copenhagen
# January 2007
# =========================================================================== #


define basicmap model
(
  # - Properties of the general model ("model)" -
  # Chose black as color to mimic eg. wall.
  color "black"

  # If 1, this model can be gripped by a gripper and can be pushed around by
  # collisions with anything that has a non-zero obstacle_return
  # We clearly do not want this to apply to a map object
  gripper_return 0
  
  
  # - GUI properties of the general model ("model") -
  # Draw a nose on the model so we can see which way it points
  # This is not used as we use this object as a map
  gui_nose 0

  # If 1, draw a bouding box around the model, indicating its size
  # Most maps will need a bounding box
  # Using bounding box will make the map seem limited even though the worlds
  #  size is bigger than the map.
  boundary 1

  # If 1, draw a scaling grid over the model 
  # This is the grid you see en the Stage Window. The grid is snapped
  # to the map model and usefull as background when robots moving around.
  gui_grid 1
  
  
  # Define how the model can be moved be the mouse in the GUI window
  # You should not be able to move the "map".
  gui_movemask 0
  
  # This is non-documentet property but it is used in some of the "official"
  #  Stage examples when making map-objects.
  gui_outline 0
 
)
