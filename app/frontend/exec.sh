#!/bin/bash

npm install -g --force --unsafe-perm
mix local.hex  --force
mix local.rebar  --force
mix deps.get
mix deps.update --all
mix compile

if [ "$MIX_ENV" == "prod" ]; then
  mix phoenix.server --no-halt
elif [ "$MIX_ENV" == "test" ]; then
  mix test
  npm test
elif [ "$MIX_ENV" == "dev" ]; then
  # # install postgres client
  apt-get install postgresql-client -y
  mix compile
  # do nothing
   echo "STATE:READY"
   echo "Usage: "
   echo "     Login in <container_name>:"
   echo "       docker exec -it fe /bin/bash"
   echo "     Connect to the postgresql DB (after login):"
   echo "       psql -h fe_postgres -p 5432 -U cds -d fe_dev"
  while true;
  do
    sleep 24h
  done
fi