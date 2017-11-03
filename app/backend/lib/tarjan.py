#!/usr/bin/env python
# Check if a city is a SCC using tarjan algorithm
import sys
import os
import json

sys.path=['/usr/lib/python2.7','/usr/local/lib/python2.7/dist-packages/tarjan-0.2.3.2-py2.7.egg']

from tarjan.tc import tc

input_path = str(sys.argv[1])

print "~TARJAN CHECK~"
print input_path
with open(input_path, "r") as json_file:
  graph = json.load(json_file)
  num_nodes =  len(graph)
  scc = tc(graph)
  print len(scc.keys())
  for k,v in scc.iteritems():
    if len(v) != num_nodes:
      excluded = []
      for node in scc.keys():
        if not node in v:
          excluded.append(node)
      print "============================================"
      print "ERROR: the whole graph is not a single SCC!"
      print "The number of expected nodes = " + str(num_nodes)
      print "Differs from the number of actual nodes = " + str(len(v))
      print "The ISOLATED SCC is the following : "
      print v
      print "The key associated with this error is the following:"
      print k
      print "The excluded nodes are:"
      print excluded
      print "============================================"
      sys.exit(1)
print "============================================"
print "OK: The whole graph is a SCC"
print "============================================"