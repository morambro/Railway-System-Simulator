#!/bin/bash
 
#Set a default value for the $cell variable

LOG_LEVEL=
NAME_SERVER=
NODE_ADDR=


usage="
	\n
The following parameters must be specified:\n
  1) Log level, choosen from [ -i | -n | -d ] \n
  2) Name Server tcp Address [ -s ] (e.g. -ns tcp://localhost:9000) \n
  3) Node name identifier [ -o ] (e.g. -nn Node1) \n
"
 
#Check to see if at least one argument was specified
if [ $# -lt 3 ] ; then
   echo $usage
   exit 1
fi
 
# Process the arguments, o and s expects a parameter
while getopts indo:s: opt
do
   case "$opt" in
      i) 	LOG_LEVEL="i";;
      n) 	LOG_LEVEL="n";;
      d) 	LOG_LEVEL="d";;
      s)	NAME_SERVER=$OPTARG;;
      o)	NODE_ADDR=$OPTARG;;
      \?)	echo $usage;exit 1;
   esac
done

# Check weather a parameter was not specified
if [ -z $LOG_LEVEL ] || [ -z $NAME_SERVER ] || [ -z $NODE_ADDR ]; then
	echo "Not all parameters specified!"
	exit 1
fi

# All parameters setted, proceed with trainsimulation start
./trainsimulation -$LOG_LEVEL $NAME_SERVER

