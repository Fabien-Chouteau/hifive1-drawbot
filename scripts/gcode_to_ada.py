#!/usr/bin/python

import sys

print """package RISCV_Adacore_Gcode is
   Gcode : constant array (Integer range <>) of access constant String := (
"""
for line in sys.stdin.readlines():
    print "      new String'(\"" + line.strip() + "\"),"

print """
      new String'(\"G28\"));
end RISCV_Adacore_Gcode;
"""
