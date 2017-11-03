#!/bin/bash
################################################################################
# this daemon executes based on the content of a state.txt file
# if the file is empty then the daemon does nothing. otherwise
# it deploys all the declared stacks (in the file)
################################################################################
while true;
do
   sleep 7s
   ids=($(cat $CITY_ROOT/config/swarmer-src/state.txt))
   for id in "${ids[@]}";
   do
      docker stack rm $id
      compose_file="docker-compose.backend-mw-fe"$id".yml";
      docker stack deploy --compose-file $CITY_ROOT/config/$compose_file --with-registry-auth $id
   done
done