#!/usr/bin/env python
# A brutal script
# Connects to docker UDS to get labels info
# e.g. the node id
import sys
import json
import requests_unixsocket
## function definitions
def get_id(data, service, addrs):
   ids = [-1,-1]
   for container in data:
      ids[0] = str(container["Labels"]["com.docker.stack.namespace"])
      if (ids[0]).isdigit() \
         and container["NetworkSettings"]["Networks"]\
         [ids[0]+"_default"]["IPAddress"] in addrs:
         names = container["Labels"]["com.docker.swarm.task.name"]
         names = names.split(".")
         ids[1] = str(names[0])
         return names[0].replace(service, '')

## body
service_name = sys.argv[1]
IP_addresses = sys.argv[2:]
request =  "/containers/json?filters={%22volume%22:[%22/var/run/docker.sock%22]}"
session = requests_unixsocket.Session()

reply = session.get('http+unix://%2Fvar%2Frun%2Fdocker.sock'+request)
ids = str(get_id(reply.json(), service_name, IP_addresses))
# print "The container id (CID) is : "+cid
print ids