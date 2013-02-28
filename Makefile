GNAT=gnatmake
FLAGS=-P
PROJECT_NAME=trainproject

default:
	clear
	$(GNAT) $(FLAGS) $(PROJECT_NAME)
	#$(GNAT) $(FLAGS) server
clean:
	rm -rf out/*
	rm -rf trainsimulation
	clear
