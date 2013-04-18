import scala.actors._
import com.inspirel.yami._

case class Event (description : String) 


class Publisher(val address : String) extends Actor {
	
	val publisherValue	= new ValuePublisher
	val publisherAgent	= new Agent
	
	// Setting address
	publisherAgent.addListener(address)
	
	publisherAgent.registerValuePublisher("events",publisherValue)
	

	def controller_loop() {
		react {
			case Event(d) => {
				val content = new Parameters
				println ("Publishing event " + d)
				content.setString("event",d)
				publisherValue.publish(content)
				controller_loop
			}
			case "stop" => println("controller Stops")
		}
	}

	def act() {
		controller_loop
	}
	
}

class Receiver(val controller : Publisher) extends IncomingMessageCallback {
	
	var serverAgent = new Agent
	
	def addHandler (address : String) {
		val add = serverAgent.addListener(address);
		println("Central Controller listening on address " + address)
		serverAgent.registerObject("central_controller",this)
	}
	
	def call(im : IncomingMessage) {
		im.getMessageName match {
			case "event" => {
				controller ! Event(im.getParameters.getString("event"))
				println("Received event " + im.getParameters.getString("event"))
			}
			case _ => {
				print("Invalid event")
			}
		}
	}
	
	def close {
		serverAgent.close
	}
}


object ControllerMain extends App {
	
	var controller 	: Publisher = null
	var receiver 	: Receiver = null
	
	def waitExit {
		readLine() match {
			case "q" | "Q" => {
				controller ! "stop"
				receiver.close
				println ("Bye!")
			}
			case a:String => {
				controller ! Event(a)
				waitExit
			}
		}
	}
	
	override def main(args : Array[String]) {
		
		if (args.length < 1) {
			println ("ERROR: Controller tcp address must be specified");
			return;
		}
		controller = new Publisher("tcp://localhost:2222")
		controller.start		
		receiver = new Receiver(controller)
		receiver.addHandler(args(0));
		
		waitExit
		
	} 
	
	
}
