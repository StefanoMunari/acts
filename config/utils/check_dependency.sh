#!/bin/bash
################################################################################
# check if all the necessary dependencies are currently installed on the
# host machine
################################################################################

function check_dependency
{
   # checks by inspecting the exit code
   $1 --v > /dev/null 2>&1
   exit_code=$?
   if [ "$exit_code" -eq 127 ];
   then
      echo "=>ERROR: $1 command not found. Check if $0 is installed"
      exit 1;
   fi
}

check_dependency $1