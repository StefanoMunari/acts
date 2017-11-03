#!/bin/bash
################################################################################
# Run (create and execute) a specific subsystem previously built using the
# build script
# dependencies: [ docker, docker-compose ]
################################################################################
service=""
city_ids=""

function help
{
   exit_code=${1:-}
   echo "Usage: "$0" -s [service]"
   echo "   -s :      the name of the service to run (e.g., backend-mw-fe)"
   echo ""

   if [ -z "$exit_code" ];
   then
      exit 0;
   fi
   exit $exit_code
}

### Set and check the necessary variables and deps

$CITY_ROOT/config/utils/check_dependency.sh "docker";
$CITY_ROOT/config/utils/check_dependency.sh "docker-compose";

# get options
{
   s_flag=0;
   while getopts s:m: name
   do
       case $name in
       s)   s_flag=$(( s_flag+1 ))
            $CITY_ROOT/config/utils/check_flag.sh "$s_flag" "2" "-s"
            service=${OPTARG:-}
            echo "$service";;
       ?)    help "1";;
       esac
   done
   if [ -z "$service" ];
   then
      help
   fi
}

if [ -z ${CITY_ROOT+x} ];
then
   echo "CITY_ROOT environment variable is unset"
   exit 1
fi

##### Configure the subsystem docker-compose file from template
if [ ! -f $CITY_ROOT/config/docker-compose.$service.yml ];
then
   cp $CITY_ROOT/config/templates/$service.template $CITY_ROOT/config/docker-compose.$service.yml
   sed -i.bak -e "s~K:CITY_ROOT~${CITY_ROOT}~g" $CITY_ROOT/config/docker-compose.$service.yml
fi

##### pre-compile the services associated with the districts
# this enables to avoid overlapping compilations during the deployment
{
## pre-compile backend
  docker run \
  --rm \
  --volume $CITY_ROOT/app/backend:/home/cds/backend \
  --name compiled_backend \
  --entrypoint /tmp/compile.sh \
  127.0.0.1:5000/backend
}

##### assign an id to each district and to each city
# extract the part to replicate
{
  keys=(redis backend mw QWERTY UIOP ASDFG)
  extra=(backend_data redis:latest mw_data mw_brk_rabbitmq /backend: /mw:)
  placeholders=(AK AX AY AZ AA AB)
  extra_placeholders=(BK BX BY BZ UIOP ASDFG)
  target_file=$CITY_ROOT/config/docker-compose.$service.yml
  index=0
  for p in  "${placeholders[@]}"
  do
    element="${keys[$index]}"
    sed -i.bak -e "s~build: .\/${element}~build: .\/${p}~g" $target_file
    index=$(( index+1 ))
  done
  index=0
  for p in  "${extra_placeholders[@]}"
  do
    element="${extra[$index]}"
    sed -i.bak -e "s~${element}~${p}~g" $target_file
    index=$(( index+1 ))
  done
  for k in "${keys[@]}"
  do
    sed -i.bak -e "s~${k}:~${k}###<ID>:~g" $target_file
    sed -i.bak -e "s~container_name: ${k}~container_name: ${k}###<ID>~g" $target_file
    sed -i.bak -e "s~\- ${k}~\- ${k}###<ID>~g" $target_file
  done
  index=0
  for p in  "${placeholders[@]}"
  do
    element="${keys[$index]}"
    sed -i.bak -e "s~build: .\/${p}~build: .\/${element}~g" $target_file
    index=$(( index+1 ))
  done
  index=0
  for p in  "${extra_placeholders[@]}"
  do
    element="${extra[$index]}"
    sed -i.bak -e "s~${p}~${element}~g" $target_file
    index=$(( index+1 ))
  done

  be_services=$(sed -n '/^###<DISTRICT_BE>$/,/^###<\/DISTRICT_BE>$/p' $target_file)
  mw_services=$(sed -n '/^###<DISTRICT_MW>$/,/^###<\/DISTRICT_MW>$/p' $target_file)
  sed -i.bak '/###<DISTRICT_BE>/,/###<\/DISTRICT_BE>/d' $target_file
  sed -i.bak '/###<DISTRICT_MW>/,/###<\/DISTRICT_MW>/d' $target_file
  # define district instances
  city_ids=($(ls -I "ai" $CITY_ROOT/app/backend/var/snapshot))
  key=""
  value=""

  ##### CONFIGURE + DEPLOY THE CITIES
  file_prefix=$CITY_ROOT/config/docker-compose.$service
  file_postfix=.yml
  separator="_"
  cp $CITY_ROOT/config/templates/swarmer.services.template $CITY_ROOT/config/templates/swarmer.services.json
  echo ${#city_ids[@]}" }" >> $CITY_ROOT/config/templates/swarmer.services.json
  mv $CITY_ROOT/config/templates/swarmer.services.json $CITY_ROOT/config/swarmer-src/
  for city_id in "${city_ids[@]}"
  do
    district_ids=($(python $CITY_ROOT/config/utils/get_district_ids.py $CITY_ROOT/app/backend/var/snapshot/$city_id | tr -d "[],'"))
    city_file=$file_prefix$city_id$file_postfix
    echo "=>Creating the configuration "$city_file" for the city "$city_id" ..."
    cp $target_file $file_prefix$city_id$file_postfix
    for district_id in "${district_ids[@]}"
    do
      be_services_aux=$(sed -e "s~###<ID>:~${district_id}:~g" <<< "$be_services")
      mw_services_aux=$(sed -e "s~###<ID>:~${district_id}:~g" <<< "$mw_services")
      sed -e "s~###<ID>~${district_id}~g" <<< "$be_services_aux" >> $city_file
      sed -e "s~###<ID>~${district_id}~g" <<< "$mw_services_aux" >> $city_file
    done
    ##### ADD NETWORKS TO STACK SPECIFICATION
    ##### substitute the network definition template parameter in the config
    cat "$CITY_ROOT"/config/templates/"$service"_network.yml >> $city_file
    sed -i.bak -e "s~###networks:~networks:~g" $city_file
    sed -i.bak -e "s~###<NETWORK_NAME>~default~g" $city_file

    echo "=>Deploying the city "$city_id" ..."
    # count the services in the city-file
    no_services=$(grep -c "###service" $city_file)
    up_services=0
    # loop until all the services are up
    while [[ "$up_services" -lt "$no_services" ]] ;
    do
      docker stack deploy --compose-file $city_file --with-registry-auth $city_id
      up_services=$(docker stack services -q $city_id | wc -l)
      echo "=>The city "$city_id" has "$up_services" services deployed"
    done
    echo "=>The city "$city_id" has been deployed"
  done

  viewer_file=$CITY_ROOT/config/docker-compose.fe.yml
  fe_network_file=$CITY_ROOT/config/templates/fe_network.yml
  if [ -e  $viewer_file ];
  then
    echo "=>Configuring the networks ..."
    ##### ADD NETWORKS TO SERVICES SPECIFICATIONS
    ##### substitute the network definition template parameter in the config
    ## BROKER:
    # (rabbitmq) => configure networks
    # attach the broker to each city (which is inside another overlay net)
    value="      networks:\n&"
    sed -i.bak -e "s~###<BRK_NETWORK_DEF>~${value}~g" $viewer_file
    for city_id in "${city_ids[@]}"
    do
      value="        "$city_id"_default:\n&"
      sed -i.bak -e "s~###<BRK_NETWORK_DEF>~${value}~g" $viewer_file
    done
    ## BROKER:
    # (rabbitmq) => configure networks
    # attach the broker to the viewer overlay net (enable fe <-> rabbitmq)
    value="        default:\n&"
    sed -i.bak -e "s~###<BRK_NETWORK_DEF>~${value}~g" $viewer_file
    ## FE (aka the viewer)
    # (fe) => configure networks
    # configure the necessary networks for the fe container
    # NOTE:
    # this substitution contains also the declaration of the nets related to fe
    echo "" >> $viewer_file
    cat $fe_network_file >> $viewer_file
    ##### ADD NETWORKS TO STACK SPECIFICATION
    ##### substitute the network definition template parameter in the config
    ## BROKER:
    # (rabbitmq) => declare networks
    # declare broker (overlay) networks as external
    for city_id in "${city_ids[@]}"
    do
      value="  "$city_id"_default:\n&"
      sed -i.bak -e "s~###<BRK_NETWORK_DECL>~${value}~g" $viewer_file
      value="    external: true\n&"
      sed -i.bak -e "s~###<BRK_NETWORK_DECL>~${value}~g" $viewer_file
    done
    ##### DEPLOY THE VIEWER
    viewer="view"
    no_services=$(grep -c "###service" $viewer_file)
    up_services=0
    echo "=>Deploying the "$viewer" ..."
    while [[ "$up_services" -lt "$no_services" ]] ;
    do
      docker stack deploy --compose-file $viewer_file --with-registry-auth $viewer
      up_services=$(docker stack services -q $viewer | wc -l)
      echo "=>The viewer "$viewer" has "$up_services" services deployed"
    done
  fi
  # remove target file
  rm -f $target_file
  rm -f $target_file".bak"
  # set up for swarmer
  touch "$CITY_ROOT"/config/swarmer-src/state.txt
  bash "$CITY_ROOT"/config/swarmer-src/daemon.sh &
}