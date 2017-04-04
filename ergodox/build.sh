#!/usr/bin/env bash

CONTROLLER_DIR="$HOME/Code/test/kiibohd-controller/"
KEYBOARDS_DIR="$CONTROLLER_DIR/Keyboards"
LAYOUTS_DIR="$CONTROLLER_DIR/kll/layouts"
BUILD_SCRIPT="$KEYBOARDS_DIR/cmake.bash"
CMakeListsPath=$CONTROLLER_DIR


mkdir "$LAYOUTS_DIR/mine"
cp *.kll "$LAYOUTS_DIR/mine/"

#
# Keyboard: Ergodox
#
# These build scripts are just a convenience for configuring your keyboard (less daunting than CMake)
# Jacob Alexander 2015-2016



#################
# Configuration #
#################

######## Left Side ########

# Feel free to change the variables in this section to configure your keyboard

BuildPath="ICED-L"

## KLL Configuration ##

# Generally shouldn't be changed, this will affect every layer
BaseMap="scancode_map leftHand slave1 rightHand"

# This is the default layer of the keyboard
# NOTE: To combine kll files into a single layout, separate them by spaces
# e.g.  DefaultMap="mylayout mylayoutmod"
DefaultMap="mine/MDErgo1-Default-0 lcdFuncMap"

# This is where you set the additional layers
# NOTE: Indexing starts at 1
# NOTE: Each new layer is another array entry
# e.g.  PartialMaps[1]="layer1 layer1mod"
#       PartialMaps[2]="layer2"
#       PartialMaps[3]="layer3"
PartialMaps[1]="mine/MDErgo1-Default-1 lcdFuncMap"
PartialMaps[2]="mine/MDErgo1-Default-2 lcdFuncMap"


##########################
# Advanced Configuration #
##########################

# Don't change the variables in this section unless you know what you're doing
# These are useful for completely custom keyboards
# NOTE: Changing any of these variables will require a force build to compile correctly

# Keyboard Module Configuration
ScanModule="Infinity_Ergodox"
MacroModule="PartialMap"
OutputModule="pjrcUSB"
DebugModule="full"

# Microcontroller
Chip="mk20dx256vlh7"

# Compiler Selection
Compiler="gcc"



########################
# Bash Library Include #
########################

# Shouldn't need to touch this section

# Check if the library can be found
if [ ! -f "$BUILD_SCRIPT" ]; then
	echo "ERROR: Cannot find '$BUILD_SCRIPT'"
	exit 1
fi

# Load the library
source $BUILD_SCRIPT



#########################
# Re-run for right side #
#########################

######## Right Side ########

# Feel free to change the variables in this section to configure your keyboard

BuildPath="ICED-R"

## KLL Configuration ##

# Only changing the basemap (everything else is the same)
# Generally shouldn't be changed, this will affect every layer
BaseMap="defaultMap rightHand slave1 leftHand"

# Load the library (starts the build)
source $BUILD_SCRIPT


