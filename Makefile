GNAT=gnatmake
FLAGS=-P
PROJECT_NAME=trainsimulation

default:
	$(GNAT) $(FLAGS) $(PROJECT_NAME)
clean:
	rm -rf out/*
	rm -rf trainsimulation
