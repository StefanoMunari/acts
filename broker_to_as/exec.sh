#!/bin/bash

# export the correct NODE_ID
ARRAY_IDS=$(python $CITY_ROOT/libs/dockerAPI.py broker $(ip addr show | grep -A1 "link/ether" | grep inet | cut -dt -f2 | cut -d/ -f1))
CITY_NODE_ID=$(echo "${ARRAY_IDS}" | cut -d_ -f1)
export CITY_NODE_ID=$CITY_NODE_ID

# append to .bashrc
echo "export CITY_NODE_ID="$CITY_NODE_ID >> $HOME/.bashrc
# load .bashrc
# source $HOME/.bashrc


mix local.hex  --force
mix local.rebar  --force
mix deps.get
mix deps.update --all

if [ "$MIX_ENV" == "prod" ]; then
  mix run --no-halt
elif [ "$MIX_ENV" == "test" ]; then
  mix test
elif [ "$MIX_ENV" == "dev" ]; then
  mix compile
  # do nothing
   echo "STATE:READY"
   echo "Usage: (log into <container_name>)"
   echo "      docker exec -it <container_name> /bin/bash"
  while true;
  do
    sleep 24h
  done
fi