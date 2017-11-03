#!/usr/bin/env python
import pika
import sys
import json
import requests_unixsocket
import time

##################### config
# TODO: change hostname
host='brk_fe_rabbitmq'
exchange='broker_to_as'
delimiter="_"
state_file="/home/state.txt"

#################### body
connection = pika.BlockingConnection(pika.ConnectionParameters(host='brk_fe_rabbitmq'))
channel = connection.channel()
channel.exchange_declare(exchange=exchange,
                         exchange_type='topic')

result = channel.queue_declare(exclusive=True)
queue_name = result.method.queue

node_ids = []

with open("/home/swarmer.services.json", "r") as inputfile:
   data = json.load(inputfile)
   cities = int(data["no_cities"]);
   for city in range(0,cities):
      for service in data["services"]:
         node_ids.append(str(city)+"_"+str(service))

binding_keys = sys.argv[1:]
if not binding_keys:
    sys.stderr.write("Usage: %s [binding_key]...\n" % sys.argv[0])
    sys.exit(1)

for binding_key in binding_keys:
    channel.queue_bind(exchange=exchange,
                       queue=queue_name,
                       routing_key=binding_key)

def callback(ch, method, properties, body):
    data = json.loads(body)
    recipient = str(data["recipient"])
    city = recipient.split(delimiter)[0]
    if (str(data["topic"]) == str(binding_key)):
      print("Swarmer is executing...")
      with open(state_file, "a") as myfile:
        myfile.write(city)
      # let the daemon do its stuff
      time.sleep(30)
      # stop the daemon execution
      with open(state_file, "w") as myfile:
        myfile.write("")

print("Swarmer is ready...")

channel.basic_consume(callback,
                      queue=queue_name,
                      no_ack=True)

channel.start_consuming()