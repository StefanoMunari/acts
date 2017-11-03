#!/usr/bin/env python

##########
# generate the costs graph for the AI from a snapshot file
# ASSUMPTION: the environment variable CITY_ROOT is correctly set to the
#			  root directory of the project (i.e. backend/)
##########

import sys
import os
import json
import pprint

def get_global_infrastructure(json_data):
	nodes = json_data["nodes"]
	data = { "streets" : [] }
	for node in nodes:
		for k,v in node.iteritems():
			data["streets"] = data["streets"] + v["streets"]
	return data

def compute_dynamic_costs(costs, city_file, way_key):
	# open in read mode the snapshot of the district
	with open(city_file) as json_data:
		snapshot = get_global_infrastructure(json.load(json_data));
		# compute the dynamic costs from the number of travellers in each stretch
		for street in snapshot["streets"]:
			for way in street[way_key]:
				for lane in way["lanes"]:
					for stretch in lane["stretches"]:
						key = str(stretch["id"])
						for index in range(len(costs[key])):
							costs[key][index] += len(stretch["travellers"])
	return costs

def compute_static_costs(graph):
	for key in graph:
		for index in range(len(graph[key])):
			# convert infra graph to costs graph
			# unary cost (1 connection) + intersection cost
			graph[key][index] = 1 + int(round(len(graph[key])/2))
	return graph

def generate_costs(filename, city_file, ftype, infra_path):
	costs = None
	# open in read mode the current infrastructure graph
	with open(infra_path+filename) as json_data:
		infra_graph = json.load(json_data);
	return compute_dynamic_costs(\
		compute_static_costs(infra_graph),\
		city_file,
		ftype+"ways")

def write_to_file(costs, infra_path):
	postfix = "-costs.json"
	# for each way
	for key in costs:
		# open in write mode the new graph of costs
		with open(infra_path+key.upper()+postfix, "w") as json_file:
			json.dump(costs[key], json_file)

def main(city_file):
	road = "road"
	bike = "bike"
	foot = "foot"
	city_id = os.environ["CITY_NODE_ID"]
	target_dir = "/var/snapshot/ai/" + city_id + "/"
	prefix = os.environ["CITY_ROOT"]
	if "cds" not in prefix and "/app/backend" not in prefix:
		prefix = prefix + "/app/backend"
	postfix = "-topology.json"
	types = [road, bike, foot]
	costs = {}
	for ftype in types:
		costs[ftype] = generate_costs(\
			ftype.upper() + postfix, city_file, ftype,\
			prefix + target_dir)
	write_to_file(costs, prefix + target_dir)

main(str(sys.argv[1]))
