#!/usr/bin/env python
################################################################################
# inspect the current city configuration given as input to detect the number
# of districts which compose the city. Then it prints them as a list of strings
################################################################################
import os
import sys
import json

data = []
node_ids = []
input_path = sys.argv[1]

full_in_path = input_path + "/district.conf"
with open(full_in_path, "r") as input_file:
 data = json.load(input_file)

for node in data["nodes"]:
 for node_id in node:
   node_ids.append(str(node_id).encode("utf-8"))

print node_ids
