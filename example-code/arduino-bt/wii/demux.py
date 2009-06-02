#!/usr/bin/python

# sets str to '\xbf\xff\x00\x11SET CONTROL MUX 0\x00'
str = 'BFFF001153455420434f4e54524f4c204d5558203000'.decode("hex")

f = open('/dev/ttyUSB0', 'w')
f.write(str)
f.close()
