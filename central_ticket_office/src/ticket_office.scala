import scala.actors._
import com.inspirel.yami._
import net.minidev.json._

case class Stop()
case class Resolve(startNode:String,from:String,to:String)
case class Path(path : String)
case class Error(message:String)

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

		var res : Map[String,String] = Map()


		JSONValue.parseStrict(json) match {
			case o : JSONObject => o.get("nodes") match {
				case arr : net.minidev.json.JSONArray => {
					for(i <- 0 until arr.size) {
						arr get (i) match {
							case el : net.minidev.json.JSONObject => {
								val name 	: String = el.get("name").toString
								val address : String = el.get("address").toString
								res = res + Tuple2(name,address)
							}
						}
					}
				}
			}
		}
		
		res	
	}
	
	def load(fileName : String) : Map[(String,String),List[(String,String)]] = {
		
		val json = scala.io.Source.fromFile(fileName).mkString
		
		var res : Map[(String,String),List[(String,String)]] = Map()
		
		JSONValue.parseStrict(json) match {
			case o : JSONObject => o.get("links") match {
				case links : JSONArray => {
					for (i <- 0 until links.size) {
						links.get(i) match {
							case el : JSONObject => {
								val reg1 : String = el.get("region1").toString
								val reg2 : String = el.get("region2").toString
								var pathsList : List[(String,String)] = List()
								el.get("paths") match {
									case paths : JSONArray => {
										for (j <- 0 until paths.size()) {
											paths.get(j) match {
												case item : JSONObject => {
													pathsList = pathsList :+ Tuple2(
														item.get("node").toString,
														item.get("station").toString)
												}
											}
										}
									}
								}
								res = res + Tuple2(Tuple2(reg1,reg2),pathsList)
							}
						}
					}
				}
			}
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
	 *  
	 *	@return the name of the node containg the station <To>
	 *
	 **/
	def ask(to : String) : String = {
		nodesAddresses.keys.foreach(k => {
			
			val p = new Parameters
			
			p.setString("station",to) 
			
			try{
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
			} catch {
				case e : com.inspirel.yami.YAMIIOException => {
					println("ERRORE: Connessione rifiutata all'indirizzo " + nodesAddresses(k))
				}
			} 
		})
		null
	}
	
	/**
	 * Ask a node to create a ticket from [form] to [to]
	 **/
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
				
					if (region == null) {
						reply  {
							Error("ERROR : No region found for station " + to)
						}
						// ...
					} else {
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

		PathResolver.NAME_SERVER_ADDRESS = argv(1)
			
				
		resolver !? Resolve("Node_1","1","H") match {
			case Error(msg) => println(msg)
			case _ => println("ok")
		}
		

		val receiver = new RequestReceiver("tcp://localhost:9999",resolver)
		receiver.start
	}
}


