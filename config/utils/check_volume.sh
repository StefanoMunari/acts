#!/bin/bash
################################################################################
# check if the AI has been downloaded as the latest valid version.
# otherwise it downloads the AI from a remote git repository
################################################################################

if [ ! -d "$CITY_ROOT/app/backend/path-finder" ];
then
   git clone http://github.com/StefanoMunari/path-finder $CITY_ROOT/app/backend/path-finder
   cd $CITY_ROOT/app/backend/path-finder
   git fetch --all
   git checkout -f tags/ada-latest -b city-ada
fi