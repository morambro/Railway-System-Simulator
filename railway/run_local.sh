# ./trainsimulation $@
./trainsimulation -d -ns tcp://localhost:1234 -nn $1 -na $2 -ct tcp://localhost:9999 -cc tcp://localhost:8888 -lpd 5 -hpd 5
