#!/bin/bash
################################################################################
# Build a specific set of images (subsystem) using docker
# dependencies: [ docker, docker-compose, git ]
################################################################################
service=
services=()
mode=

function help
{
   exit_code=${1:-}
   echo "Usage: "$0" -s [service] -m [mode]"
   echo "   -s :     the name of the service to build (e.g.\"backend\", \"backend-mw\", \"backend-mw-fe\")"
   echo "   -m :     the mode of the service {\"d\" = dev, \"t\" = test, \"p\" = production}"
   echo "               * d|dev        : the container will keep running forever waiting for your login."
   echo "                              usage => (after run)"
   echo "                              (login)  docker exec -it [container_name] /bin/bash"
   echo "                              (stop)   docker stop [container_name]"
   echo "               * t|test       : the container will execute all the specified test."
   echo "               * p|production : the container will execute init following all the specified configuration options."
   echo "Example:"
   echo "   (builds backend service in test mode)"
   echo "   $0 -s backend -m t"
   echo ""

   if [ -z "$exit_code" ];
   then
      exit 0;
   fi
   exit $exit_code
}

if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ];
then
   help
fi

if [ $(ls $CITY_ROOT/config/ | grep -c docker-compose) -ne 0 ];
then
   rm -f $CITY_ROOT/config/docker-compose.*.yml
fi

### Set and check the necessary variables and deps

$CITY_ROOT/config/utils/check_dependency.sh "docker"
$CITY_ROOT/config/utils/check_dependency.sh "docker-compose"
$CITY_ROOT/config/utils/check_dependency.sh "git"

### Generate the configurations for the cities
cd $CITY_ROOT/config/city_generator
docker build -t m0bius:city_generator --rm .
cd $CITY_ROOT/config/
$CITY_ROOT/config/utils/generate_cities.sh

### Check the graph properties
cd $CITY_ROOT/config/tarjan
docker build -t m0bius:tarjan --rm .
cd $CITY_ROOT/config/
num_cities=$(ls $CITY_ROOT/app/backend/etc/init/ | grep "ai" | wc -l)
num_cities=$(( $num_cities - 1 ))

for city_id in $(seq 0 $num_cities);
do
   $CITY_ROOT/config/utils/check_graph_properties.sh "/home/app/backend/var/snapshot/"$city_id"/district.conf"
done

# get options
{
   s_flag=0;
   m_flag=0;
   while getopts s:m: name
   do
       case $name in
       s)   s_flag=$(( s_flag+1 ))
            $CITY_ROOT/config/utils/check_flag.sh "$s_flag" "2" "-s"
            service=${OPTARG:-};;
       m)   m_flag=$(( m_flag+1 ))
            $CITY_ROOT/config/utils/check_flag.sh "$m_flag" "2" "-m"
            mode=${OPTARG:-};;
       ?)   help "1";;
       esac
   done
   if [ -z "$service" ] || [ -z "$mode" ];
   then
      help
   fi
}

if [ -z ${CITY_ROOT+x} ];
then
   echo "CITY_ROOT environment variable is unset"
   exit 1
fi

if [ "$service" = "backend-mw" ] || [ "$service" = "backend-mw-fe" ];
then
   if [ ! -z $CITY_ROOT/config/templates/$service.template ];
   then
      rm -f $CITY_ROOT/config/templates/$service.template
   fi
   touch $CITY_ROOT/config/templates/$service.template
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      exit $exit_code
   fi
   ### template substitutions
   awk '/#<INTEGRATION>/,0' $CITY_ROOT/config/templates/backend.template >> $CITY_ROOT/config/templates/$service.template
   awk '/#<INTEGRATION>/,0' $CITY_ROOT/config/templates/mw.template >> $CITY_ROOT/config/templates/$service.template
   sed -i.bak "s/#<BE-MW>//g" $CITY_ROOT/config/templates/$service.template
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      exit $exit_code
   fi
   ## check docker registry (docker-swarm)
   $CITY_ROOT/config/utils/check_registry.sh
   ## create an array of services
   IFS='-' read -ra EL <<< "$service"
   for i in "${EL[@]}"; do
      services+=($i)
   done
fi

if [ "$service" = "backend-mw-fe" ];
then
   # awk '/#<INTEGRATION>/,0' $CITY_ROOT/config/templates/fe.template >> $CITY_ROOT/config/templates/$service.template
   cp $CITY_ROOT/config/templates/fe.template $CITY_ROOT/config/docker-compose.fe.yml
   sed -i.bak "s/#<BRK-FE>//g" $CITY_ROOT/config/docker-compose.fe.yml
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      exit $exit_code
   fi
fi

# copy template for fe
cp $CITY_ROOT/config/templates/init-user-db.sh.template $CITY_ROOT/config/fe_postgres/init-user-db.sh
chmod +x $CITY_ROOT/config/fe_postgres/init-user-db.sh

if [ "$service" = "backend" ] || [ "${services[0]}" = "backend" ];
then
   $CITY_ROOT/config/utils/check_volume.sh
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      exit $exit_code
   fi
   cp $CITY_ROOT/config/templates/dockerAPI.py $CITY_ROOT/app/backend/lib/
fi

if [ "$service" = "mw" ] || [ "${services[1]}" = "mw" ];
then
   $CITY_ROOT/config/utils/check_volume.sh
   exit_code=$?
   if [ "$exit_code" -ne 0 ];
   then
      exit $exit_code
   fi
   mkdir -p $CITY_ROOT/mw/libs/
   cp $CITY_ROOT/config/templates/dockerAPI.py $CITY_ROOT/mw/libs/
   mkdir -p $CITY_ROOT/broker_to_as/libs/
   cp $CITY_ROOT/config/templates/dockerAPI.py $CITY_ROOT/broker_to_as/libs/
fi

### Configure the service Dockerfile from template
{
   # copy the generated template
   cp $CITY_ROOT/config/templates/$service.template $CITY_ROOT/config/docker-compose.$service.yml
   # check the selected mode
   if [ "$mode" = "d" ] || [ "$mode" = "dev" ];
   then
      # if it is a composition of macro-components
      if [ ! -z "$services" ];
      then
         for s in "${services[@]}"
         do
            if [ ! -z "$s" ];
            then
               # remove <TEST_MODE>
               python $CITY_ROOT/config/utils/select_mode.py TEST $CITY_ROOT/config/$s/Dockerfile.template $CITY_ROOT/config/$s/Dockerfile.v0
               # remove <PROD_MODE>
               python $CITY_ROOT/config/utils/select_mode.py PROD $CITY_ROOT/config/$s/Dockerfile.v0 $CITY_ROOT/config/$s/Dockerfile
               if [ -e $CITY_ROOT/config/docker-compose.fe.yml ];
               then
                  # remove <TEST_MODE>
                  python $CITY_ROOT/config/utils/select_mode.py TEST $CITY_ROOT/config/fe/Dockerfile.template $CITY_ROOT/config/fe/Dockerfile.v0
                  # remove <PROD_MODE>
                  python $CITY_ROOT/config/utils/select_mode.py PROD $CITY_ROOT/config/fe/Dockerfile.v0 $CITY_ROOT/config/fe/Dockerfile
               fi
            fi
         done
      else # single service (e.g. mw || backend || fe)
         # remove <TEST_MODE>
         python $CITY_ROOT/config/utils/select_mode.py TEST $CITY_ROOT/config/$service/Dockerfile.template $CITY_ROOT/config/$service/Dockerfile.v0
         # remove <PROD_MODE>
         python $CITY_ROOT/config/utils/select_mode.py PROD $CITY_ROOT/config/$service/Dockerfile.v0 $CITY_ROOT/config/$service/Dockerfile
      fi
      # if it is mw or a service composition of services (all involve mw)
      # then apply template substitution (in place)
      if [ "$service" = "mw" ] || [ ! -z "$services" ];
      then
         $CITY_ROOT/config/utils/instantiate_template.sh $service "mw" "dev"
      fi
      # if it is fe or a service composition which involves fe
      # then apply template substitution (in place)
      if [ "$service" = "fe" ] || [ "$service" = "backend-mw-fe" ];
      then
         $CITY_ROOT/config/utils/instantiate_template.sh "fe" "fe" "dev"
      fi
   elif [ "$mode" = "p" ] || [ "$mode" = "production" ];
   then
      # if it is a composition of macro-components
      if [ ! -z "$services" ];
      then
         for s in "${services[@]}"
         do
            if [ ! -z "$s" ];
            then
               # remove <DEV_MODE>
               python $CITY_ROOT/config/utils/select_mode.py DEV $CITY_ROOT/config/$s/Dockerfile.template $CITY_ROOT/config/$s/Dockerfile.v0
               # remove <TEST_MODE>
               python $CITY_ROOT/config/utils/select_mode.py TEST $CITY_ROOT/config/$s/Dockerfile.v0 $CITY_ROOT/config/$s/Dockerfile
               if [ -e $CITY_ROOT/config/docker-compose.fe.yml ];
               then
                  # remove <DEV_MODE>
                  python $CITY_ROOT/config/utils/select_mode.py DEV $CITY_ROOT/config/fe/Dockerfile.template $CITY_ROOT/config/fe/Dockerfile.v0
                  # remove <TEST_MODE>
                  python $CITY_ROOT/config/utils/select_mode.py TEST $CITY_ROOT/config/fe/Dockerfile.v0 $CITY_ROOT/config/fe/Dockerfile
               fi
            fi
         done
      else # single service (e.g. mw || backend || fe)
         # remove <DEV_MODE>
         python $CITY_ROOT/config/utils/select_mode.py DEV $CITY_ROOT/config/$service/Dockerfile.template $CITY_ROOT/config/$service/Dockerfile.v0
         # remove <TEST_MODE>
         python $CITY_ROOT/config/utils/select_mode.py TEST $CITY_ROOT/config/$service/Dockerfile.v0 $CITY_ROOT/config/$service/Dockerfile
      fi
      # if it is mw or a service composition of services (all involve mw)
      # then apply template substitution (in place)
      if [ "$service" = "mw" ] || [ ! -z "$services" ];
      then
         $CITY_ROOT/config/utils/instantiate_template.sh $service "mw" "prod"
      fi
      # if it is fe or a service composition which involves fe
      # then apply template substitution (in place)
      if [ "$service" = "fe" ] || [ "$service" = "backend-mw-fe" ];
      then
         $CITY_ROOT/config/utils/instantiate_template.sh "fe" "fe" "prod"
      fi
   elif [ "$mode" = "t" ] || [ "$mode" = "test" ];
   then
      # if it is a composition of macro-components
      if [ -z "$services" ];
      then
         for s in "${services[@]}"
         do
            if [ ! -z "$s" ];
            then
               # remove <TEST_MODE>
               python $CITY_ROOT/config/utils/select_mode.py DEV $CITY_ROOT/config/$s/Dockerfile.template $CITY_ROOT/config/$s/Dockerfile.v0
               # remove <PROD_MODE>
               python $CITY_ROOT/config/utils/select_mode.py PROD $CITY_ROOT/config/$s/Dockerfile.v0 $CITY_ROOT/config/$s/Dockerfile
               if [ -e $CITY_ROOT/config/docker-compose.fe.yml ];
               then
                  # remove <DEV_MODE>
                  python $CITY_ROOT/config/utils/select_mode.py DEV $CITY_ROOT/config/fe/Dockerfile.template $CITY_ROOT/config/fe/Dockerfile.v0
                  # remove <PROD_MODE>
                  python $CITY_ROOT/config/utils/select_mode.py PROD $CITY_ROOT/config/fe/Dockerfile.v0 $CITY_ROOT/config/fe/Dockerfile
               fi
            fi
         done
      else # single service (e.g. mw || backend || fe)
         # remove <DEV_MODE>
         python $CITY_ROOT/config/utils/select_mode.py DEV $CITY_ROOT/config/$service/Dockerfile.template $CITY_ROOT/config/$service/Dockerfile.v0
         # remove <PROD_MODE>
         python $CITY_ROOT/config/utils/select_mode.py PROD $CITY_ROOT/config/$service/Dockerfile.v0 $CITY_ROOT/config/$service/Dockerfile
      fi
      # if it is mw or a service composition of services (all involve mw)
      # then apply template substitution (in place)
      if [ "$service" = "mw" ] || [ ! -z "$services" ];
      then
         $CITY_ROOT/config/utils/instantiate_template.sh $service "mw" "test"
      fi
      # if it is fe or a service composition which involves fe
      # then apply template substitution (in place)
      if [ "$service" = "fe" ] || [ "$service" = "backend-mw-fe" ];
      then
         $CITY_ROOT/config/utils/instantiate_template.sh "fe" "fe" "test"
      fi
   else
      help
   fi
   # if it is a composition of macro-components
   if [ ! -z "$services" ];
   then
      for s in "${services[@]}"
      do
         rm $CITY_ROOT/config/$s/Dockerfile.v0
      done
   else
      rm $CITY_ROOT/config/$service/Dockerfile.v0
   fi
}

### Configure the subsystem docker-compose file from template

if [ ! -f $CITY_ROOT/config/docker-compose.$service.yml ]; then
   cp $CITY_ROOT/config/templates/$service.template $CITY_ROOT/config/docker-compose.$service.yml
fi

sed -i.bak -e "s~K:CITY_ROOT~${CITY_ROOT}~g" $CITY_ROOT/config/docker-compose.$service.yml
if [ "$service" = "backend-mw-fe" ];
then
   sed -i.bak -e "s~K:CITY_ROOT~${CITY_ROOT}~g" $CITY_ROOT/config/docker-compose.fe.yml
fi

### Build the subsystem images

docker-compose -f $CITY_ROOT/config/docker-compose.$service.yml build
if [ "$service" = "backend-mw-fe" ];
then
   docker-compose -f $CITY_ROOT/config/docker-compose.fe.yml build
fi