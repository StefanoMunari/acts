#!/bin/bash
################################################################################
# check if a docker registry is currently configured on this machine
# otherwise it creates and configures a docker registry
################################################################################

registry=$(docker service ls | grep registry)

if [ -z "${registry[0]}" ];
then
   printf "=> Creating the docker registry... \n";
   docker service rm registry
   docker service create --detach=false --name registry --publish 5000:5000 registry:latest
else
   printf "=> The docker registry is configured \n";
fi;