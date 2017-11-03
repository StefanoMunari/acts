#!/bin/bash
################################################################################
# create a temporary docker container runs the city generator.
# It creates a set of city configurations and puts them
# in the correct configuration folders of each macro component.
# The last step is achieved by mounting the local paths as temporary volumes
# inside the container.
################################################################################

docker run \
--rm \
--volume $CITY_ROOT/app:/home/app \
--volume $CITY_ROOT/mw:/home/mw \
--volume $CITY_ROOT/broker_to_as:/home/broker_to_as \
--name generator \
m0bius:city_generator \
/bin/bash generate_all.sh