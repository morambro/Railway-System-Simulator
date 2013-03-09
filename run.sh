#!/bin/bash
 
#Set a default value for the $cell variable
cell="test"

usage="
	\n
The following parameters must be specified:\n
  1) Log level, choosen from [ -i | -n | -d ] \n
  2) Name Server tcp Address [ -ns ] (e.g. -ns tcp://localhost:9000) \n
  3) Node name identifier [ -nn ] (e.g. -nn Node1) \n
"
 
#Check to see if at least one argument was specified
if [ $# -lt 1 ] ; then
   echo "You must specify at least 1 argument."
   exit 1
fi
 
#Process the arguments
while getopts c:hin opt
do
   case "$opt" in
      c) 	cell=$OPTARG;;
      h) 	echo $usage;;
      i) 	info="yes";;
      n)	name=$OPTARG;;
      \?)	echo $usage;;
   esac
done
