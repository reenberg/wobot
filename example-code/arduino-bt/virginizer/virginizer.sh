#!/bin/sh

# virginizer.command  2008 mbanzi
# 
# Arduino project http://www.arduino.cc
#
# quick and dirty script to set the proper fuses and lock bits
# on an arduino BT board that has gone crazy
#
# 

BINDIR=/Applications/arduino-0010/hardware/tools/avr/bin/
PPORT=/dev/cu.usbserial-A4000QpS
BTPORT=/dev/tty.ARDUINOBT



# seting lock bits
$BINDIR/avrdude -p m168 -b 115200 -c stk500v2 -P $PPORT -e -u -U lock:w:0x3f:m -U efuse:w:0x00:m -U hfuse:w:0xdd:m -U lfuse:w:0xff:m


#uploading init software
$BINDIR/avrdude -p m168 -b 115200 -c stk500v2 -P $PPORT -e -U flash:w:bt_test_00.hex

echo "the script will now sleep for 10 seconds so that the init software can set the BT name and password"
sleep 10

#burn the bootloader
$BINDIR/avrdude -p m168 -b 115200 -c stk500v2 -P $PPORT -e -U flash:w:ATmegaBOOT_168.hex

echo "the bootloader is ready to be tested!"
echo "press the reset button within 3 seconds"
sleep 3

#upload the test software
echo "testing the bootloader on $BTPORT"
$BINDIR/avrdude -p m168 -b 115200 -c stk500v1 -P $BTPORT -e -U flash:w:bt_test_01.hex




