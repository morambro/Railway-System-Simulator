import scala.actors._
import com.inspirel.yami._
import scala.util.parsing.json._

case class Stop()
case class Resolve(startNode:String,from:String,to:String)
case class Path(path : String)
case class Error(message:String)


class RegionalStation(parId : String, parAddress : String) {
	
	val id = parId;
	
	val address = parAddress;
	
}

object PathResolver {
	
	val SERVICE_NAME = "ticket_creation";
	
	var NAME_SERVER_ADDRESS = "";
	
	/**
	 * Loads the json file containg the location of regional ticket offices
	 *
	 * @return A list of couples (name,address)
	 */ 
	def getMap(json : String) : Map[String,String] = {
		
		//val json = scala.io.Source.fromFile(fileName).mkString

		val json_elements = JSON.parseFull(json)
	
		var res : Map[String,String] = Map()

		json_elements match {
			case Some(el) => el match {
				case map : Map[String,List[_]] => {
					map.get("nodes") match {
						case Some(v : List[Map[String,String]]) 	=> {
							v.foreach( office => {
								val name : String = office("name")
								val address : String = office("address")
								res = res + Tuple2(name,address)
							})
						}
						case Some(_) 	=> println("Error") 
						case None 		=> println("Error")
					}
				}
				case _ => println("ciao")
			}
			case None 	=> println("Parsing Error")
		}
		
		res	
	}
	
	def load(fileName : String) : Map[(String,String),List[(String,String)]] = {
		
		val json = scala.io.Source.fromFile(fileName).mkString
		
		var res : Map[(String,String),List[(String,String)]] = Map()
		
		JSON.parseFull(json) match {
			case Some(map) => map match {
				case m : Map[String,_] => {
					m get "links" match {
						case Some(el) => el match {
							case l : List[_] => {
								l.foreach( elem => elem match {
									case m : Map[String,_] => {
										var reg1 : String = ""
										var reg2 : String = ""
										var path : List[(String,String)] = List()
										m get "region1" match {
											case Some(r1) => r1 match {
												case r1s : String => reg1 = r1s
											}
											case None => println("Parsing ERROR!")
										}
										m get "region2" match {
											case Some(r2) => r2 match {
												case r2s : String => reg2 = r2s
											}
											case None => println("Parsing ERROR!")
										}
										
										m get "paths" match {
											case Some(p) => p match {
												case l : List[_] => {
													l.foreach (item => item match {
														case m : Map[String,String] => {
															path = (m("node"),m("station")) :: path
														} 
													})
												}
											}
											case None => println("Parsing ERROR!")
										}
										 
										res = res + Tuple2((reg1,reg2),path)
									}
								})
							}
						}
						case None => println("Parsing ERROR!")
					
					}
				}
				case _ => println("Parsing ERROR!")
			}
			case None => println("Parsing ERROR!")			
		}
		
		res
	}
	
}

class PathResolver(fileName : String) extends Actor {
	
	// The list of couples <regional office,address> 
	var nodesAddresses	: Map[String,String] = null
	// Map containg for each region the list of regions to go through to reach it 
	var regionsMap 		: Map[(String,String),List[(String,String)]] = 
		PathResolver.load("../../railway/res/links.json")
	
	val agent = new Agent
	
	def getNodesAddressList : Map[String,String] =  {
		val message : OutgoingMessage = agent.send(
			PathResolver.NAME_SERVER_ADDRESS,
		    "name_server", 
		    "all", 
		    new Parameters);
		    
		message.waitForCompletion
		
		// Received the response by name server
		message.getState match {
			case OutgoingMessage.MessageState.REPLIED => {
			
		    	val reply = message.getReply();
			
				val nodes = reply.getString("result")
		
				PathResolver.getMap(nodes)
		    } 
		    case OutgoingMessage.MessageState.REJECTED => {
				println("The message has been rejected: " + message.getExceptionMsg)
				null
			}
			case _ => {
				println("The message has been abandoned.")
				null
			}
		}
	}

	/**
	 *	For each couple (Node_Name,Address) search for destination <To>. 
	 *  	@return the name of the node containg the station <To>
	 **/
	def ask(to : String) : String = {
		nodesAddresses.keys.foreach(k => {
			
			val p = new Parameters
			
			p.setString("station",to) 
			
			val message : OutgoingMessage = agent.send(
				nodesAddresses(k),
				"message_handler", 
				"is_present", 
				p);
		    
			message.waitForCompletion
			
			message.getState match {
				case OutgoingMessage.MessageState.REPLIED => {
			
					val reply = message.getReply();
			
					val found = reply.getString("result")
		
					if (found == "TRUE") return k
				} 
				case OutgoingMessage.MessageState.REJECTED => {
					println("The message for node " +k+ " has been rejected: " + message.getExceptionMsg)
				}
				case _ => {
					println("The message for node " +k+ " has been abandoned.")
				}
			}
		})
		null
	}
	
	
	def ask (node:String,from:String,to:String) {

		val p = new Parameters

		p.setString("from",from)
		p.setString("to",to)
		
		val message : OutgoingMessage = agent.send(
			nodesAddresses(node),
			"message_handler", 
			"ticket_creation", 
			p)
		
		message.waitForCompletion
		
		message.getState match {
			case OutgoingMessage.MessageState.REPLIED => {
			
		    	val reply = message.getReply();
			
				val ticket = reply.getString("ticket")
				
				println(ticket)
				
		    } 
		    case OutgoingMessage.MessageState.REJECTED => {
				println("The message has been rejected: " + message.getExceptionMsg)
			}
			case _ => {
				println("The message has been abandoned.")
			}
		}
	}

	def resolverLoop() {
		react {
			case Resolve(startNode,from,to) => {
				println("I have to resolve form " + from + " to " + to)
				
				if (nodesAddresses == null)
					nodesAddresses = getNodesAddressList
				if (nodesAddresses == null) 
					println("ERRORE!!!")
				else {
					
					val region = ask(to)
				
					println("Station " + to + " is in Region " + region)
				
					// Now we have startNode and region to reach. We have to find
					// the gateways to cross to build a path					
					regionsMap get (startNode,region) match {
						case Some(item) => {
							var node = startNode
							var f = from
							item match {
								case l : List[_] => {
									l.foreach(c => {
										println("Search from "+f+" to "+c._2+" region " +  node)
										ask(node,f,c._2)
										node = c._1
										f = c._2
									})
									println("Search from "+f+" to "+to+" region " +  node)
									ask(node,f,to)
								}
							}
						}
						case None => reply {
							Error("No path to reach " + region + " from " + startNode)
						}
					}
				}
				resolverLoop
			}
			case Stop() => {
				println("Tearing down Path resolver")
			}
		}
	}

	def act() = resolverLoop()
	

}

class RequestReceiver(address : String, resolver : Actor) extends Actor with IncomingMessageCallback{
	
	val serverAgent : Agent = new Agent;
	
	val resolvedAddress = serverAgent.addListener(address);
	
	serverAgent.registerObject("cantral_ticket_server", this);
	
		
	
	def receiverLoop() {
		react {
			case Stop()	=> println("Bye")
			case Path(path) => println("The path is " + path)
		}
	}
		
	
	def act() = receiverLoop()
	
	def call(im : IncomingMessage) {
		im.getMessageName match {
		
			case "resolve"	=>	{
				
				
				val start 		= im.getParameters.getString("start")
				val startNode 	= im.getParameters.getString("start_node")
				val destination = im.getParameters.getString("destination")			
				
				val ticket 		= resolver !? Resolve(startNode,start,destination) 
				
				
				// SEND RESPONSE TO CLIENT!!
			}
			
		}
	}
}

object Main extends App {
	override def main(argv : Array[String]) {
		if (argv.length < 2) {
			println("Please insert the json environment description and the name server address")
			return;
		}
		val resolver = new PathResolver(argv(0))
		resolver.start
		
		resolver ! Resolve("Node_1","1","H")
		
		PathResolver.NAME_SERVER_ADDRESS = argv(1)
	
	
		val receiver = new RequestReceiver("tcp://localhost:9999",resolver)
		receiver.start
	}
}


