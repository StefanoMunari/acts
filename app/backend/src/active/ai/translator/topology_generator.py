#!/usr/bin/env python

##########
# generate the infrastructure graph for the AI from a snapshot file
# ASSUMPTION: the environment variable CITY_ROOT is correctly set to the
#			  root directory of the project (i.e. backend/)
##########

import sys
import os
import json
import pprint

def connect_straight(data, output):
	# connects straight stretches
	prev = ""
	for stretch_id in data:
		# connect prev stretch to the current one
		if prev:
			output[prev].append(str(stretch_id))
		if not str(stretch_id) in output:
			# init the current stretch connection to empty
			output[str(stretch_id)] = []
		# save the current stretch
		prev = str(stretch_id);
	return output

def extract_straights(roadway):
	straights = {}
	for k0,v0 in roadway.iteritems():
		for lane in v0["lanes"]:
			for k1,stretchl in lane.iteritems():
				straights = connect_straight(stretchl, straights)
	return straights

def lanes(data, lane_id):
	# stretch_list
	stretchl = []
	for stretch in data:
		stretchl.append(stretch["id"])
	return {lane_id : stretchl}

# encode roadways, bikeways, footways in output
def ways(data, output, street_id):
	for way in data:
		output[str(way["id"])] = {"lanes" : []}
		for lane in way["lanes"]:
			l = lanes(lane["stretches"], str(lane["id"]))
			output[str(way["id"])]["lanes"].append(l)

def streets(data, roadway, bikeway, footway):
	for street in data:
		ways(street["roadways"], roadway, str(street["id"]))
		ways(street["bikeways"], bikeway, str(street["id"]))
		ways(street["footways"], footway, str(street["id"]))

def get_last_stretch(laneId, way):
	for kw,vw in way.iteritems():
		for lane in vw["lanes"]:
			for kl,vl in lane.iteritems():
				if kl == str(laneId):
					return vl[len(vl)-1]

def connect_street(connections, way, result):
	for conns in connections:
		for conn in conns["connections"]:
			out_stretch = get_last_stretch(conn["out"], way)
			for conn_ins in conn["ins"]:
				in_wayId = str(conn_ins[2])
				if in_wayId in way:
					in_way = way[in_wayId]
					for in_lane in in_way["lanes"]:
						in_laneId = str(conn_ins[1])
						if in_laneId in in_lane:
							# take the first stretch of the lane
							# it is the IN stretch for other street lanes
							in_stretchId = in_lane[in_laneId][0]
							# connect the two stretches
							if not (str(out_stretch) in result):
								result[str(out_stretch)] = []
							# there are multiple possible destinations
							result[str(out_stretch)]\
							.append(str(in_stretchId))

def merge_straight_with_connections(way, connected):
	for k,v in way.iteritems():
		if k in connected:
			way[k]=way[k]+connected[k]
	return way

def connect_streets(connections, roadway, bikeway, footway):
	road_result = {}
	bike_result = {}
	foot_result = {}
	# compute connection between stretches
	connect_street(connections, roadway, road_result)
	connect_street(connections, bikeway, bike_result)
	connect_street(connections, footway, foot_result)
	# compute the complete graph for each road
	roadway = merge_straight_with_connections(\
		# connect each stretch of the single lanes
		extract_straights(roadway), road_result)
	bikeway = merge_straight_with_connections(\
		extract_straights(bikeway), bike_result)
	footway = merge_straight_with_connections(\
		extract_straights(footway), foot_result)
	return (roadway, bikeway, footway)

def get_global_infrastructure(json_data):
	nodes = json_data["nodes"]
	data = {"streets" : [], "intersections" : [], "connections" : []}
	for node in nodes:
		# only 1 object exists
		for k,v in node.iteritems():
			data["streets"] = data["streets"] + v["streets"]
			data["connections"] = data["connections"] + v["connections"]
	return data

def process_set_of_crossings(street, conn, crossings, direction, ways_set):
	ref_lane = street["roadways"][0]["lanes"][0]
	index_limit = len(ref_lane["stretches"]) - 1
	i = 0
	for crossing_index in crossings:
		compl_index = index_limit - crossing_index
		steps = {}
		for way in ways_set:
			ordinal = int(way["ordinal"])
			steps[ordinal] = []
			for lane in way["lanes"]:
				if lane["direction"] == direction:
					steps[ordinal].append(\
						lane["stretches"][crossing_index]["id"])
				else:
					steps[ordinal].append(\
						lane["stretches"][compl_index]["id"])
		stretch_from = None
		stretch_to = None
		for ordinal in sorted(steps, reverse=(i % 2 != 0)):
			if i % 2 != 0:
				steps[ordinal] = list(reversed(steps[ordinal]))
			for step in steps[ordinal]:
				if stretch_to == None:
					stretch_to = step
					continue
				stretch_from = stretch_to
				stretch_to = step
				if not (str(stretch_from) in conn):
					conn[str(stretch_from)] = []
				conn[str(stretch_from)].append(str(stretch_to))
		i = i + 1
	return conn

def process_crossings(streets, road, bike, foot):
	for street in streets:
		ref_lane = street["roadways"][0]["lanes"][0]
		ref_dir = ref_lane["direction"]
		roadway_stretches = ref_lane["stretches"]
		pedCrossings  = []
		bikeCrossings = []
		for i in xrange(0, len(roadway_stretches)):
			if roadway_stretches[i]["decorations"]["pedestrianCrossing"]:
				pedCrossings.append(i)
			if roadway_stretches[i]["decorations"]["bicycleCrossing"]:
				bikeCrossings.append(i)
		ways = street["roadways"] + street["bikeways"] + street["footways"]
		foot = process_set_of_crossings(street, foot, pedCrossings, ref_dir, ways)
		ways = street["roadways"] + street["bikeways"]
		bike = process_set_of_crossings(street, bike, bikeCrossings, ref_dir, ways)
	return (road, bike, foot)

def generate_infrastructure(filename, roadway, bikeway, footway):
	with open(filename) as json_data:
		json_data = get_global_infrastructure(json.load(json_data));
		# extract stretches from streets
		streets(json_data["streets"], roadway, bikeway, footway);
		# add intersections (i.e. intersections between extracted stretches)
		( roadway, bikeway, footway ) = \
			connect_streets(json_data["connections"], roadway, bikeway, footway)
		return process_crossings(json_data["streets"], roadway, bikeway, footway)

def write_infrastructure(data, file_type):
	try:
		if file_type is None:
			raise TypeError
		city_id = os.environ["CITY_NODE_ID"]
		target_dir = "/var/snapshot/ai/" + city_id + "/"
		postfix = "-topology.json"
		prefix = os.environ["CITY_ROOT"]
		if "cds" not in prefix and "/app/backend" not in prefix:
			prefix = prefix + "/app/backend"
		path = prefix + target_dir
		with open(path + file_type + postfix,"w") as json_file:
			json.dump(data, json_file)
	except TypeError:
		print("Invalid argument provided to translator")

def main(input_file):
	road_id = "ROAD"
	bike_id = "BIKE"
	foot_id = "FOOT"
	(road, bike, foot) = generate_infrastructure(input_file, {},{},{})
	# write results to file
	write_infrastructure(road, road_id);
	write_infrastructure(bike, bike_id);
	write_infrastructure(foot, foot_id);

main(str(sys.argv[1]))
