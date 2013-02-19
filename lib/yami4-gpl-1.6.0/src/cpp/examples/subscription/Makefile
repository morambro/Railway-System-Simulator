INCLUDE_DIRS = -I../../../../include -I../common_utils/posix
LIB_DIRS = -L../../../../lib
LIBS = -lyamicpp -lyamicore -lpthread

build : publisher subscriber

publisher : publisher.cpp
	g++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

subscriber : subscriber.cpp
	g++ $? -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)

publisher_with_stats : publisher_with_stats.cpp
	g++ $? -g -o $@ $(INCLUDE_DIRS) $(LIB_DIRS) $(LIBS)


clean :
	rm -f publisher subscriber
