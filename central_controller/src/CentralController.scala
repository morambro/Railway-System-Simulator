import scala.actors._
import com.inspirel.yami._

case class Event (description 	: String) 

object Event {
	def createFromJson (Json_String : String) : Event = Event(Json_String)
}

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

class Subscriber(ID : Int) extends Actor with IncomingMessageCallback{
	
	val subscriberAgent = new Agent

	def subscriber_loop {
		react {
			case "init" => {
				println ("init!")
				subscriberAgent.registerObject("update_handler", this);
				val params : Parameters = new Parameters
				params.setString("destination_object", "update_handler");
				subscriberAgent.sendOneWay("tcp://localhost:2222","events", "subscribe", params);
				subscriber_loop
			}
		}		
	}

	def act() {
		subscriber_loop
	}
	
	def call (incomingMessage : IncomingMessage) {
		try {
			val content = incomingMessage.getParameters()

		    val event = content.getString("event")
		    
		    println("Subscriber " + ID + " Received event : " + event)
		}catch{
			case e => e.printStackTrace 
		}
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
				controller ! Event.createFromJson(im.getParameters.getString("content"))
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
			case _ => {
				println ("Type [q|Q] to quit")
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
		
		val s = new Subscriber(1)
		s.start
		s ! "init"
		
		val s2 = new Subscriber(2)
		s2.start
		s2 ! "init"
		
		waitExit
		
	} 
	
	
}
