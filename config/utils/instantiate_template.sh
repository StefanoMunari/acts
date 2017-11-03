#!/bin/bash
################################################################################
# perform substitution on template parameters define in the docker-compose
# configuration files. These template are related to the MW and FE config files
################################################################################

function error_handler
{
   proc=${1:-}
   exit_code=${2:-}

   if [ "$exit_code" -ne 0 ];
   then
      echo "=>ERROR: $proc failed."
      exit 1;
   fi
}

function instantiate_template
{
   composition=${1:-}
   service=${2:-}
   environment=${3:-}
   # MIX template
   sed -i.bak -e "s~=$service.TEMPLATE.MIX~=$environment~g" $CITY_ROOT/config/docker-compose.$composition.yml
   # checks by inspecting the exit code
   exit_code=$?
   error_handler $1 $exit_code
   # DB template for FE
   sed -i.bak -e "s~$service.TEMPLATE.DB~fe_$environment~g" $CITY_ROOT/config/docker-compose.$composition.yml
   # checks by inspecting the exit code
   exit_code=$?
   error_handler $1 $exit_code
   # DB template for FE
   sed -i.bak -e "s~$service.TEMPLATE.DB~fe_$environment~g" $CITY_ROOT/config/fe_postgres/init-user-db.sh
   # checks by inspecting the exit code
   exit_code=$?
   error_handler $1 $exit_code
}

instantiate_template $1 $2 $3