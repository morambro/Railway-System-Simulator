import scala.actors._
import com.inspirel.yami._

case class Event(description:String) 
case class Stop()
case class DistributedStop()


class Publisher(val address : String) extends Actor {
	
	val publisherValue	= new ValuePublisher
	val publisherAgent	= new Agent
	
	// Setting address
	publisherAgent.addListener(address)
	
	publisherAgent.registerValuePublisher("events",publisherValue)
	

	/**
	 * Loads the json file containg the location of regional ticket offices
	 *
	 * @return A list of couples (name,address)
	 */ 
	def getMap(json : String) : Map[String,String] = {
		var res : Map[String,String] = Map()
		JSON.parseJSON(json).nodes.foreach(node => {res += node.name.toString -> node.address.toString})
		
		res.keys.foreach(k => println(k + "," + res(k)))
		
		res	
	}


	def controllerLoop() {
		react {
			case Event(d) => {
				val content = new Parameters
				//println ("Publishing event " + d)
				content.setString("event",d)
				publisherValue.publish(content)
				controllerLoop
			}
			case DistributedStop() => {
				// Ask Name server for the list of Nodes
				val agent = new Agent
				
				ViewAgent ! Write("Asking Name Server for Nodes addresses at " + ControllerMain.NAME_SERVER_ADDRESS);
				
				try {
					
					val message : OutgoingMessage = agent.send(
						ControllerMain.NAME_SERVER_ADDRESS,
						"name_server", 
						"list", 
						new Parameters);
					
					ViewAgent ! Write("Waiting For Name Server response...")				
					message.waitForCompletion
				
					// Received the response by name server
					message.getState match {
						case OutgoingMessage.MessageState.REPLIED => {
			
							message.getReply.getString("response") match {
								case "OK" => {
									val map = getMap(message.getReply.getString("list"))
							
									ViewAgent ! Write("Node Addresses Gained!")
									if (map == Map()) println("WARNING: No Nodes to terminate!") 
									// Once the map is gained, ask each Node to Terminate:
									
									
									map.keys.foreach( k => {
										
										println(map(k))
										
										val message : OutgoingMessage = agent.send(
											map(k),
											"message_handler", 
											"terminate", 
											new Parameters)
								
										message.waitForCompletion
								
										message.getState match {
											case OutgoingMessage.MessageState.REPLIED => {
												println("Node "+k+" Received termination request.")
											}
											case OutgoingMessage.MessageState.REJECTED => 
												println("ERROR: The message has been rejected by node "+ k +" : " + message.getExceptionMsg)
											case _ => 
												println("ERROR: The message has been abandoned by node " + k)
										}
									})
								}
								case _ => println("Name Server Sent ERROR")
							}
						
							ViewAgent ! Write("Central Controller can be termianted writing 'q' or 'Q'!")
							controllerLoop
						
						} 
						case OutgoingMessage.MessageState.REJECTED => {
							println("ERROR: The message has been rejected: " + message.getExceptionMsg)
							controllerLoop
						}
						case _ => {
							println("ERROR: The message has been abandoned.")
							controllerLoop
						}
					}
				} catch {
					case e : com.inspirel.yami.YAMIIOException => {
						println("ERRORE: Connessione rifiutata all'indirizzo " + ControllerMain.NAME_SERVER_ADDRESS)
						controllerLoop
					}
				}
			}
			case Stop() => println("controller Stops")
		}
	}

	def act() {
		controllerLoop
	}
	
}

case class Write(m:String)
case class Init(c : Actor)
object ViewAgent extends Actor {
	
	var controller : Actor = null
	
	val view = new CentralControllerView
	
	def viewAgentLoop() : Unit = react {
		case Write(m:String) => {
			view.write(m)
			viewAgentLoop()
		}
		case Stop() => 
	}
	
	def act() = react {
		case Init(c:Actor) => {
			controller = c
			view.setStopOperation(_ => controller ! DistributedStop())
			viewAgentLoop()	
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
				controller ! Event(im.getParameters.getString("event"))
			}
			case other => {
				print("ERROR : Invalid EVENT "+ other.toString )
			}
		}
	}
	
	def close {
		serverAgent.close
	}
}

object ControllerMain extends App {
	
	var NAME_SERVER_ADDRESS = "";
	
	var controller 	: Publisher = null
	var receiver 	: Receiver = null
//	var viewAgent 	: ViewAgent = null
	
	def waitExit {
		readLine() match {
			case "q" | "Q" => {
				controller ! Stop()
				receiver.close
				println ("Bye!")
			}
			case a : String => {
				controller ! Event(a)
				waitExit
			}
		}
	}
	
	override def main(args : Array[String]) {
		
		if (args.length < 2) {
			println ("ERROR: Controller and Name Server tcp addresses must be specified");
			return;
		}
		controller = new Publisher("tcp://localhost:2222")
		controller.start		
		receiver = new Receiver(controller)
		receiver.addHandler(args(0))
		
		ViewAgent.start
		ViewAgent ! Init(controller)
		
		NAME_SERVER_ADDRESS = args(1)
		
		waitExit
		
	} 
}
