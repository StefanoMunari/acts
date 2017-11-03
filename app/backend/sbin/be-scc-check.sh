#!/bin/bash
################################################################################
## Generate the topology graphs used during the simulation and check
## if the graph is itself a strong connected component (SCC).
## If it is not an SCC, then abort.
## SCC is an important precondition that must hold for all the graphs used
## during the simulation. We do not want isolated districts
################################################################################
# Generate graphs
python $CITY_ROOT/app/backend/src/active/ai/translator/topology_generator.py $1

exit_code=$?
if [ "$exit_code" -ne 0 ];
then
  exit $exit_code
fi

graphs=($(ls $CITY_ROOT/app/backend/var/snapshot/ai/ | grep topology))
# Check SCC of each graph
for graph in "${graphs[@]}"
do
   python $CITY_ROOT/app/backend/lib/tarjan.py $CITY_ROOT/app/backend/var/snapshot/ai/$graph
   if [ "$exit_code" -ne 0 ];
   then
      exit $exit_code
   fi
done
