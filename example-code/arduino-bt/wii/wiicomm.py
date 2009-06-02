#!/usr/bin/python

######################################################
#             wiimotecomm 0.1.1 by Cadex             #
#                                                    #
# Tiny tool for testing communication with a Wiimote #
#               by sending custom data               #
#                                                    #
#             Requires Python an PyBluez             #
######################################################

import bluetooth
import sys
import thread
import time

# Sockets and status
receivesocket = bluetooth.BluetoothSocket(bluetooth.L2CAP)
controlsocket = bluetooth.BluetoothSocket(bluetooth.L2CAP)
status = "Disconnected"

# Connect to the Wiimote at bluetooth address <address>
def connect(address):
	global status
	receivesocket.connect((address, 0x13))
	controlsocket.connect((address, 0x11))
	if receivesocket and controlsocket:
		print "[Connected to Wiimote at address " + address + "]"
		status = "Connected"
		thread.start_new_thread(receivethread, ())
	else:
		print "[Could not connect to Wiimote at address " + address + "]"

# Disconnect from the Wiimote
def disconnect():
	global status
	status = "Disconnecting"
	while status == "Disconnecting":
		wait(1)
	print "[Disconnected]"

# Try to discover a Wiimote
def discover():
	print "[Searching for Wiimotes...]"
	address = None
	bluetoothdevices = bluetooth.discover_devices(lookup_names = True)
	for bluetoothdevice in bluetoothdevices:
		if bluetoothdevice[1] == "Nintendo RVL-CNT-01":
        		address = bluetoothdevice[0]
			print "[Found Wiimote at address " + address + "]"
	if address == None:
		print "[No Wiimotes discovered. Press buttons 1 + 2 at the same time prior to running the script in order to switch Wiimote to discoverable mode]"
	return address

# Create a String that is a hexdump of data
def hexdump(data):
	hexdump = ""
	for byte in data:
		hexdump += str(byte).encode("hex").upper() + " "
	return "(" + hexdump[0:2] + ")" + hexdump[2:-1]

# Thread that listens for incoming data
def receivethread():
	global status
	receivesocket.settimeout(0.1)
	while status == "Connected":
		try:
			data = receivesocket.recv(23)
			if len(data):
				print "Received: " + hexdump(data)
		except bluetooth.BluetoothError:
			pass
	receivesocket.close()
	controlsocket.close()
	status = "Disconnected"

# Send <data> to the Wiimote
def send(data):
	controlsocket.send(data)
	print "Sent:     " + hexdump(data)
        print "          " + data

# Wait <millis> milliseconds
def wait(millis):
	time.sleep(millis / 1000.0)

# Main program code
if len(sys.argv) >= 3:
	address = sys.argv[1]
	if address == "discover":
		# Try to discover a Wiimote instead of using a certain bluetooth address
		address = discover()
	if address != None:
		# Try to connect to Wiimote
		connect(address)
		if status == "Connected":
			# If attempt to connect was successfull, then process all commandline arguments
			for command in sys.argv[2:]:
				if command[0:5] == "wait:":
					# If commandline argument was of type "wait:nnnn", then wait for nnnn milliseconds
					wait(int(command[5:]))
				else:
					# Otherwise Convert commandline argument from hex string to byte array
					index = 0
					data = ""
					while index < len(command):
						if " ()".find(command[index]) >= 0:
							index = index + 1
						data += command[index:index+2].decode("hex")
						index = index + 2
					# Send this byte array to the Wiimote
					send(data)
					# Wait for about 100 milliseconds for an answer
					wait(100)
			# Disconnect from the Wiimote
			disconnect()
else:
	# Print usage information
	print "Usage:"
	print "wiimotecomm <bluetooth address or \"discover\"> <hex bytes to send or \"wait:nnnn\">"
	print
	print "The hexadecimal bytes to be sent can be given with (e.g. 501100) or without spaces (e.g. \"50 11 00\")"
	print
	print
	print "Examples:"
	print "wiimotecomm discover \"52 11 11\" wait:1000 \"52 11 40\" wait:1000"
	print "Search for a Wiimote; if one is found, enable the first LED and rumble, wait for 1000 milliseconds, turn rumble off again and enable the fourth LED, wait another 1000 milliseconds, then terminate"
	print
	print "wiimotecomm 00:19:1C:2B:C9:A3 521500"
	print "Connect to the Wiimote at bluetooth address 00:19:1C:2B:C9:A3 and query the controller status"
	print
	print "wiimotecomm 00:19:1C:2B:C9:A3 \"52 12 00 31\" wait:30000"
	print "Connect to the Wiimote at bluetooth address 00:19:1C:2B:C9:A3, choose report type 0x31 (Buttons and motion sensing), wait for 30 seconds before terminating (Any changes to button and motion status will be shown)"
