#!/bin/bash

CITY_SRCDIR=${1:-}
CITY_OBJDIR=${2:-}
CITY_BINDIR=${3:-}
CITY_ENTRYPOINT=${4:-}
# CITY_DEBUG=${5:-}
CITY_AI_TARGET_PATH=$CITY_ROOT/src/active/ai/
CITY_STATIC_DEPS="-I/usr/share/ada/adainclude/gnatcoll/ gnatcoll-json-utility.adb gnatcoll-json.adb"
CITY_DYNAMIC_DEPS="-L/usr/include -ljsoncpp -pthread"

if [ -z "$CITY_ENTRYPOINT" ];
then
   CITY_ENTRYPOINT="init"
fi

function printer
{
   message=${1:-}
   echo "======================================================================================"
   echo "-----------------------------------$message------------------------------"
   echo "======================================================================================"
}

function error_handler
{
   err_code=${1:-};
   program_point=${2:-}
   echo "=>ERROR: $0::$program_point"
   exit $err_code;
}

function ai_compile
{
   printer "AI-COMPILE:START"
   if [ ! -d "$CITY_PATH_FINDER" ]; then
      error_handler 1 ${FUNCNAME[0]}
   fi
   obj_files=($(ls $CITY_PATH_FINDER/obj/ | grep -E "*.o"))
   if [ ${#obj_files[@]} -eq 0 ];
   then
      cd $CITY_PATH_FINDER
      make
      # CDBG=$CITY_DEBUG
      rm $CITY_PATH_FINDER/obj/main.o
      exit_code=$?
      if [ "$exit_code" -ne 0 ];
      then
         error_handler $exit_code ${FUNCNAME[0]}
      fi
      cd $CITY_ROOT
   fi
   printer "AI-COMPILE:COMPLETED"
}

function include_ai_interface
{
   cp $CITY_PATH_FINDER/src/bindings/ada/* $CITY_AI_TARGET_PATH
}

function generate_ada_spec
{
   cd $CITY_AI_TARGET_PATH
   g++ -c -fdump-ada-spec ai_interface.h
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      error_handler $exit_code ${FUNCNAME[0]}
   fi
   cd $CITY_ROOT
}

function city_compile
{
   printer "CITY-COMPILE:START"
   include_dirs=$(find $CITY_ROOT/$CITY_SRCDIR -type d | sed 's/.*/-I&/' | tr '\n' ' ');
   # | grep -v "reactive-infrastructure-build-facility_config_reader.adb"
   include_sources=$(ls -R $CITY_ROOT/$CITY_SRCDIR | grep -E "\.adb" | $CITY_ROOT/sbin/be-exclude-subunits.sh);
   gnatmake -x -g -c -D $CITY_ROOT/obj/ \
      $CITY_STATIC_DEPS \
      $include_dirs \
      $include_sources
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      error_handler $exit_code ${FUNCNAME[0]}
   fi
   printer "CITY-COMPILE:COMPLETED"
}

function city_bind
{
   printer "CITY-BIND:START"
   f_ext=".ali";
   # print command
   echo gnatbind $CITY_ROOT/$CITY_OBJDIR/$CITY_ENTRYPOINT$f_ext;
   # exec command
   gnatbind $CITY_ROOT/$CITY_OBJDIR/$CITY_ENTRYPOINT$f_ext;
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      error_handler $exit_code ${FUNCNAME[0]}
   fi
   printer "CITY-BIND:COMPLETED"
}

function city_link
{
   printer "CITY-LINK:START"
   f_ext=".ali"
   # print command
   echo gnatlink --LINK=g++ $CITY_ROOT/$CITY_OBJDIR/$CITY_ENTRYPOINT$f_ext \
   $(find $CITY_PATH_FINDER/obj -type f -name "*.o") \
   $CITY_DYNAMIC_DEPS \
   -o $CITY_BINDIR/$CITY_ENTRYPOINT;
   # exec command
   gnatlink --LINK=g++ $CITY_ROOT/$CITY_OBJDIR/$CITY_ENTRYPOINT$f_ext \
   $(find $CITY_PATH_FINDER/obj -type f -name "*.o") \
   $CITY_DYNAMIC_DEPS \
   -o $CITY_BINDIR/$CITY_ENTRYPOINT;
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      error_handler $exit_code ${FUNCNAME[0]}
   fi
   printer "CITY-LINK:COMPLETED"
}

function build
{
   if [ -e $CITY_ROOT/bin/init ];
   then
      echo "Executable already exists in "$CITY_ROOT/bin/init;
   else
      cd $CITY_ROOT;
      ai_compile;
      include_ai_interface;
      generate_ada_spec;
      city_compile;
      city_bind;
      city_link;
   fi;
}

build;