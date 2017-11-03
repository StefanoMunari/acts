#!/usr/bin/env bash
function server_pid {
  echo $(lsof -i:4002 | grep beam | awk '{ print $2 }')
}
function start_server {
  echo "Starting server..."

  # Let's wait for the server to start
  exec 3< <(mix s &)
  while read line; do
     case "$line" in
     *Running*)
        break
        ;;
     *)
        ;;
     esac
  done <&3
  exec 3<&-
}
function stop_server {
  trap - TERM ERR EXIT
  PID=$(server_pid)
  echo "Server PID=$PID"
  if [ -n $PID ]; then
    echo "Stopping server..."
    kill $PID
  fi
}

set -e
trap stop_server TERM ERR EXIT

DIR=$(pwd -P)
export MIX_ENV=teste2e

# let's get the database in shape
cd "$DIR/apps/domain"
mix ecto.reset

# run the e2e tests in the interface
cd "$DIR/apps/interface"
npm run webpack-test-e2e
start_server
node_modules/.bin/protractor
