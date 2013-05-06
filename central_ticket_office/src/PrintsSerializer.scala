import scala.actors._

case class Print(message:String)
case class StopPrint()

object PrintsSerializer extends Actor {

	def printSerializerLoop() : Unit = react {
		case Print(message) => {
			println(message)
			printSerializerLoop
		}
		case StopPrint() => println("Print Serializer is shutting down...")		
	}

	def act() = printSerializerLoop()
}
