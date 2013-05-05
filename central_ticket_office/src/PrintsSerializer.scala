import scala.actors._

object PrintsSerializer extends Actor {

	def printSerializerLoop() : Unit = react {
		case Print(message) => {
			println(message)
			printSerializerLoop
		}
		case Stop => 			
	}

	def act() = printSerializerLoop()
}
