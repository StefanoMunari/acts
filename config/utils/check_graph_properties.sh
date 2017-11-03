#!/bin/bash
################################################################################
# create a temporary docker container which runs a tarjan algorithm
# implementation to check if the generated graphs (city configurations)
# are valid. Indeed, each graph should be a SCC to guarantee the preconditions
# of the AI. Thus, we can run the simulation successfully (e.g., no impasses)
################################################################################

docker run \
--rm \
--volume $CITY_ROOT/app:/home/app \
--name tarjan \
m0bius:tarjan \
/bin/bash /home/app/backend/sbin/be-scc-check.sh $1