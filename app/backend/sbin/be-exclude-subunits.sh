#!/bin/bash
################################################################################
## Exclude sub-units from compilation process
################################################################################
# get pipe output as input array
list=($(cat))

for file in "${list[@]}"
do
   filename=$(find src -name $file)
   line=$(head -n 1 $filename)
   # filter subunits
   if [[ !($line = separate*) ]];
   then
       echo $file
   fi
done