#!/bin/bash
################################################################################
# check for duplicated arguments given as input to the script
################################################################################

function check_flag
{
   if [ "$1" == "$2" ];
   then
      printf "=>ERROR: Multiple usage of the same argument (%s) not allowed\n" $3;
      exit 1;
   fi
}

check_flag $1 $2 $3