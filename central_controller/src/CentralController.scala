import scala.actors._
import com.inspirel.yami._

case class Event(description:String) 
case class Stop()
case class DistributedStop()
case class Proceed()
case class NodeTerminated(nodeName : String)

class Publisher(val address : String) extends Actor {
	
	val publisherValue	= new ValuePublisher
	val publisherAgent	= new Agent
	
	/**
	 * Nodes asked to terminate
	 */
	var askedNodes = List[String]()
	
	/**
	 * Terminated Nodes
	 */
	var	terminatedNodes = List[String]()
	
	
	// Set the Address from witch the publisher will be ready 
	publisherAgent.addListener(address)
	
	// Pub-Sub service definition
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
	
	def getNodesList : Map[String,String] = {
		val agent = new Agent
		// Ask Name Server for the List of Addresses
		val message : OutgoingMessage = agent.send(
			ControllerMain.NAME_SERVER_ADDRESS,
			"name_server", 
			"list", 
			new Parameters);

		ViewAgent ! Write("Waiting for Nodes list from Name Server...")				
		
		message.waitForCompletion
		
		val result = message.getState match {
			case OutgoingMessage.MessageState.REPLIED => message.getReply.getString("response") match {
					case "OK" => getMap(message.getReply.getString("list"))
					case _ => null
			}
			case OutgoingMessage.MessageState.REJECTED => {
				println("ERROR: The message has been rejected: " + message.getExceptionMsg)
				null
			}
			case _ => {
				println("ERROR: The message has been abandoned.")
				null
			}
		}
		
		result
	}
	
	def sendMarker : Boolean = {
		val agent = new Agent
		val message = agent.send(
			ControllerMain.CENTRAL_TICKET_OFFICE_ADDRESS,
			"central_ticket_server", 
			"marker", 
			new Parameters);

		message.waitForCompletion
		var received = false

		message.getState match {
			case OutgoingMessage.MessageState.REPLIED => {
				println("Central Ticket Office Received the Marker")
				received = true
			}
			case OutgoingMessage.MessageState.REJECTED => println("ERROR: The message has been rejected by Central Ticket Office")
			case _ => println("ERROR: The message has been abandoned by Central Ticket Office")
		}
		agent.close
		
		received
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
				ViewAgent ! Write("Requested Distributed Termination")
				
				sendMarker match {
					case true => // do nothing
					case false => ViewAgent ! Write("ERROR: Central Ticket Office did not receive the Marker!")
				}
				
				controllerLoop()
			}
			
			case Proceed() => {
			
				println("PROCEED")
				try {
					getNodesList match {
						case map : Map[_,_] => {
							if (map == Map()){
								ViewAgent ! Write("WARNING: No Nodes to terminate!")
								ViewAgent ! Write("Sending the second Marker to Central Ticket Office...")
								if (sendMarker) {
									ViewAgent ! Write("Central Controller can be now termianted!")
									this ! Stop()
								}
							}else {
								val agent = new Agent
								map.keys.foreach( k => {
									val message : OutgoingMessage = agent.send(
										map(k),
										"message_handler", 
										"terminate", 
										new Parameters)
									message.waitForCompletion
									message.getState match {
										case OutgoingMessage.MessageState.REPLIED => {
											ViewAgent ! Write("Node "+k+" Received termination request.")
											// Add the node to asked list
											askedNodes ::= k
										}
										case OutgoingMessage.MessageState.REJECTED => 
											ViewAgent ! Write("ERROR: The message has been rejected by node "+ k +
													" : " +	message.getExceptionMsg)
										case _ => 
											ViewAgent ! Write("ERROR: The message has been abandoned by node " + k)
									}
								})
								agent.close
							}
						}
						case null => 
					}
					controllerLoop()
				} catch {
					case e : com.inspirel.yami.YAMIIOException => {
						println("ERRORE: Connessione rifiutata all'indirizzo " + ControllerMain.NAME_SERVER_ADDRESS)
						controllerLoop()
					}
				}
			}
			
			case NodeTerminated(nodeName) => {
				ViewAgent ! Write("Node "+nodeName+" confirmed Termination")
				askedNodes = askedNodes.filter(_ != nodeName)
				if (askedNodes.size == 0) {
					// All nodes terminated, so we can send another Marker to Central Ticket Office to 
					// Shut it down!
					if(sendMarker) {
						ViewAgent ! Write("Central Controller can be now termianted!")
						this ! Stop()
					}
				}else{
					// If there are > 0 nodes to wait, loop again.
					controllerLoop
				}
			}
			
			case Stop() => {
				ViewAgent ! DisableButtons()
				ViewAgent ! Write("Close the Window to quit central controller")
				ViewAgent ! Stop()
			}
		}
	}

	def act() {
		controllerLoop
	}
	
}

case class Write(m:String)
case class Init(c : Actor)
case class DisableButtons()
/**
 * Object used to wrap the View.
 **/
object ViewAgent extends Actor {
	
	var controller : Actor = null
	
	val view = new CentralControllerView
	
	def viewAgentLoop() : Unit = react {
		case Write(m:String) => {
			view.write(m)
			viewAgentLoop()
		}
		case DisableButtons() => {
			view.disableButtons()
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

/**
 *
 * Defines a Message Receiver agent
 *
 **/
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
			
			case "central_office_ack" => {
				println("RECEIVED ACK FROM TICKET OFFICE")
				
				im.reply(new Parameters)
				
				// Now that we received the ack, we can proceed with the termiation of the nodes				
				controller ! Proceed()
				
			}
			
			case "node_terminated" => {
				
				controller ! NodeTerminated(im.getParameters.getString("node_name"))
				
				val replyPar = new Parameters
				replyPar.setString("response","OK")
				im.reply(replyPar)
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
	
	var NAME_SERVER_ADDRESS = ""
	var CENTRAL_TICKET_OFFICE_ADDRESS = ""
	
	var controller 	: Publisher = null
	var receiver 	: Receiver = null
	
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
		
		if (args.length < 3) {
			println ("""
ERROR: Following parameters MUST be specifed:
	1) Controller address;
	2) Name Server address;
	3) Central Ticket Office address""");
			return;
		}
		
		val controllerAddress = args(0)
		val nameServerAddress = args(1)
		val centralTicketOfficeAddress = args(2)
		
		// Controller creation,
		controller = new Publisher("tcp://localhost:2222")
		controller.start		
		
		// Receiver creation
		receiver = new Receiver(controller)
		receiver.addHandler(controllerAddress)
		
		// Initialization of the View Agent
		ViewAgent.start
		ViewAgent ! Init(controller)
		
		NAME_SERVER_ADDRESS = nameServerAddress
		CENTRAL_TICKET_OFFICE_ADDRESS = centralTicketOfficeAddress
		
		waitExit
		
	} 
}
