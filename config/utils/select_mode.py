#!/usr/bin/env python
################################################################################
# apply substitutions on the given file to remove the unnecessary parts
# from each configuration. Thus, it is possible to keep only
# the part which describes the specific mode (e.g., dev production or test)
################################################################################

import re # regex
import os
import sys

mode = sys.argv[1]
input_path = sys.argv[2]
output_path = sys.argv[3]
regex = "\#( |\t|\n)*\<"+mode+"_MODE\>((.\+*\/*\-*\"*\,*|\n)[^\#])*\#( |\t|\n)*\<\/"+mode+"_MODE\>"
pattern = re.compile(regex)

with open(input_path, "r") as input_file, open(output_path, "w") as output_file:
   data = input_file.read()
   print >>output_file, pattern.sub('', data)