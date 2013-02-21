GNAT=gnatmake
FLAGS=-P
PROJECT_NAME=trainsimulation

default:
	$(GNAT) $(FLAGS) $(PROJECT_NAME)
	$(GNAT) $(FLAGS) server
clean:
	rm -rf out/*
	rm -rf trainsimulation
