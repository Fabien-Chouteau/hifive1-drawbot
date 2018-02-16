#!/usr/bin/python

import sys
from pygcode.gcodes import *
from pygcode import *
from pygcode import Line

print """with Gcode; use Gcode;

package RISCV_Adacore_Gcode is

   Coords : constant array (Integer range <>) of Float_Position := (
"""
X=0.0
Y=0.0
Z=0.0
for line_text in sys.stdin.readlines():
    gline = Line(line_text)
    for test in gline.block.gcodes:
        if isinstance(test, GCodeLinearMove):
            if 'X' in test.get_param_dict():
                X = float(test.get_param_dict()['X'])
            if 'Y' in test.get_param_dict():
                Y = float(test.get_param_dict()['Y'])
            if 'Z' in test.get_param_dict():
                Z = float(test.get_param_dict()['Z'])

            print "(%s, %s, %s)," %(str(X), str(Y), str(Z))
            # print str(test.get_param_dict())
    # print "      new String'(\"" + line.strip() + "\"),"

print """
   (0.0, 0.0, 0.0));
end RISCV_Adacore_Gcode;
"""
