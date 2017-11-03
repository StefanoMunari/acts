#!/bin/bash
################################################################################
# compiles 
################################################################################

docker run \
--rm \
--volume $CITY_ROOT/app:/home/app \
--volume $CITY_ROOT/mw:/home/mw \
--volume $CITY_ROOT/broker_to_as:/home/broker_to_as \
--name compiled_"$1" \
"$1" \
/bin/bash generate_all.sh