#!/bin/bash

# export the correct NODE_ID
ARRAY_IDS=$(python $CITY_ROOT/libs/dockerAPI.py mw $(ip addr show | grep -A1 "link/ether" | grep inet | cut -dt -f2 | cut -d/ -f1))
CITY_NODE_ID=$(echo "${ARRAY_IDS}" | cut -d_ -f1)
CITY_DISTRICT_ID=$(echo "${ARRAY_IDS}" | cut -d_ -f2)
export CITY_NODE_ID=$CITY_NODE_ID
export CITY_DISTRICT_ID=$CITY_DISTRICT_ID
# append to .bashrc
echo "export CITY_NODE_ID="$CITY_NODE_ID >> $HOME/.bashrc
echo "export CITY_DISTRICT_ID="$CITY_DISTRICT_ID >> $HOME/.bashrc
# load .bashrc
# source $HOME/.bashrc

mix local.hex  --force
mix local.rebar  --force
mix deps.get
mix deps.update --all
mix compile

if [ "$MIX_ENV" == "prod" ]; then
  mix run --no-halt
elif [ "$MIX_ENV" == "test" ]; then
  mix test
elif [ "$MIX_ENV" == "dev" ]; then
  # do nothing
   echo "STATE:READY"
   echo "Usage: (log into <container_name>)"
   echo "      docker exec -it <container_name> /bin/bash"
  while true;
  do
    sleep 24h
  done
fi