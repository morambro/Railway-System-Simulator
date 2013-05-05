default:
	clear
	scalac -d out -classpath ./out/:../lib/json-smart-1.1.1.jar src/JSONScala.scala
	scalac -d out -classpath ./out/ src/CentralControllerView.scala
	scalac -d out -classpath ./out/:../lib/yami4.jar src/CentralController.scala
fast:
	fsc -d out -classpath ./out/:../lib/json-smart-1.1.1.jar src/JSONScala.scala
	fsc -d out -classpath ./out/ src/CentralControllerView.scala
	fsc -d out -classpath ./out/:../lib/yami4.jar src/CentralController.scala
clean:
	rm -f out/*.class
