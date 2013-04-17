import scala.actors._
import com.inspirel.yami._
import net.minidev.json._

case class Stop()
case class Resolve(startNode:String,from:String,to:String,traveler_index:String)
case class Path(path : String)
case class Error(message:String)

object Ticket {
	
	def mergeTickets (T1:Ticket,T2:Ticket) : Ticket =  {
		val ticket = new Ticket(T1.next)
		if ((T1.stages.last.destinationPlatform == T2.stages(0).startPlatform) && (T1.stages.last.trainId == T2.stages(0).trainId)) {
			ticket.stages = T1.stages.drop(1) ++ ( new Ticket_Stage(
				T1.stages.last.startStation,
				T2.stages(0).nextStation,
				T1.stages.last.trainId,
				T1.stages.last.startPlatform,
				T2.stages(0).destinationPlatform,
				T2.stages(0).nextRegion) :: T2.stages.tail
			)
		} else {
			ticket.stages = T1.stages ++ T2.stages
		}
		ticket
	}
	
	implicit def stringToTicket(json : String) : Ticket = {
		var ticket : Ticket = null
		JSONValue.parseStrict(json) match {
			case o : JSONObject => {
				o.get("next") match {
					case n : java.lang.Integer => {
						ticket = new Ticket(n)
					}
				}
				if (ticket != null) {
					o.get("ticket") match {
						case stages : JSONArray => {
							for (i <- 0 until stages.size) {
								stages.get(i) match {
									case stage : JSONObject => {
										val startStation : Int = stage.get("start_station") match {
											case s : java.lang.Integer => s
										}
										val nextStation : Int = stage.get("next_station") match {
											case s : java.lang.Integer => s
										}
										val trainId : Int = stage.get("train_id") match {
											case s : java.lang.Integer => s
										}
										val startPlatform : Int = stage.get("start_platform_index") match {
											case s : java.lang.Integer => s
										}
										val destinationPlatform : Int = 
											stage.get("destination_platform_index") match {
											case s : java.lang.Integer => s
										}
										val nextRegion : String = 
											stage.get("region") match {
											case s : String => s
										}
										ticket.stages = ticket.stages :+ new Ticket_Stage(
											startStation,
											nextStation ,
											trainId,
											startPlatform, 
											destinationPlatform,
											nextRegion)
									}
								} 
							}
						}
					}
				}
			}
		}
		ticket
	}
	
	implicit def ticket2Json(T:Ticket) : String = {
		var json : String = ""
		
		json += "{"
			
		json += """ "next" : """ + T.next + ","
		
		json += """ "ticket" : ["""
		
		var i = 0
		T.stages.foreach ( stage => {
			json += "{"
			
			json += """ "start_node" : """ + stage.startStation + ","
			json += """ "next_node" : """ + stage.nextStation + ","
			json += """ "train_id" : """ + stage.trainId + ","
			json += """ "start_platform_index" : """ + stage.startPlatform + ","
			json += """ "destination_platform_index" : """ + stage.destinationPlatform + ","
			json += """ "region" : """ + "\"" + stage.nextRegion + "\""
			
			json += "}"
			if (i < T.stages.length - 1) json += ","
			i += 1
		})
		
		json += "]"
		json += "}"
		
		json
	}
}

class Ticket (n : Int){
	val next : Int = n
	var stages : List[Ticket_Stage] = List()
	
	def print {
		println ("Ticket")
		println ("next = "+next)
		stages.foreach( stage =>
			stage.print
		)
	}
}

class Ticket_Stage (
	val startStation : Int,
	val nextStation : Int,
	val trainId : Int,
	val startPlatform  : Int, 
	val destinationPlatform  : Int,
	val	nextRegion : String) {
	
	def print {
		println ("start station  : " + startStation)
		println ("next station   : " + nextStation)
		println ("train id       : " + trainId)
		println ("start platform : " + startPlatform)
		println ("dest platform  : " + destinationPlatform)
		println ("next region    : " + nextRegion)
	}
}

object PathResolver {
	
	val SERVICE_NAME = "ticket_creation";
	
	var NAME_SERVER_ADDRESS = "";
	
	// Map containg for each region the list of regions to go through to reach it 
	var regionsMap 		: Map[(String,String),List[(String,String)]] = PathResolver.load("../../railway/res/links.json")
	
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
	def ask (node:String,from:String,to:String) : Ticket = {

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
				
				reply.getString("response") match {
					case "ERROR" => {
						println(reply.getString("type"))
					}
					case "RECEIVED" => {
						println("Ticket Found!")
						println(reply.getString("ticket"))
						return reply.getString("ticket")
					}
				}
				
		    } 
		    case OutgoingMessage.MessageState.REJECTED => {
				println("The message has been rejected: " + message.getExceptionMsg)
			}
			case _ => {
				println("The message has been abandoned.")
			}
		}
		null
	}

	def resolverLoop() {
		react {
			case Resolve(startNode,from,to,traveler_index) => {
				println("I have to resolve from " + from + " to " + to)
				
				if (nodesAddresses == null)
					nodesAddresses = getNodesAddressList
				if (nodesAddresses == null) 
					println("ERRORE!!!")
				else {
					
					val region = ask(to)
				
					if (region == null) {
						println("ERROR : No region found for station " + to)
						// ...
					} else {
						println("Station " + to + " is in Region " + region)
				
						// Now we have startNode and region to reach. We have to find
						// the gateways to cross to build a path					
						
						var tickets : List[Ticket] = List()
						
						PathResolver.regionsMap get (startNode,region) match {
							case Some(item) => {
								var node = startNode
								var f = from
								item match {
									case l : List[_] => {
										l.foreach(c => {
											println("Search from "+f+" to "+c._2+" region " +  node)
											tickets = tickets :+ ask(node,f,c._2)
											node = c._1
											f = c._2
										})
										println("Search from "+f+" to "+to+" region " +  node)
										tickets = tickets :+ ask(node,f,to) 
									}
								}
							}
							case None => {
								println("No path to reach " + region + " from " + startNode)
							}
						}
						
						tickets.foreach ( t => t.print)
						
						var result_ticket : Ticket = tickets(0)
						
						for (i <- 1 until tickets.length) {
							result_ticket = Ticket.mergeTickets(result_ticket,tickets(i))
						}
						
						result_ticket.print
						
						// Send the Ticket Back to the Node 
						
						print(Ticket.ticket2Json(result_ticket))
						
						val p = new Parameters

						p.setString("traveler_index",traveler_index)
						p.setString("ticket",Ticket.ticket2Json(result_ticket))
		
						val message : OutgoingMessage = agent.send(
							nodesAddresses(startNode),
							"message_handler", 
							"ticket_ready", 
							p)
		
						message.waitForCompletion
		
						message.getState match {
							case OutgoingMessage.MessageState.REPLIED => {
								println("Message Received")
							} 
							case OutgoingMessage.MessageState.REJECTED => {
								println("The message has been rejected: " + message.getExceptionMsg)
							}
							case _ => {
								println("The message has been abandoned.")
							}
						}
						
					}
				}
				resolverLoop
			}
			case Stop() => {
				println("Tearing down Path resolver")
				agent.close
			}
		}
	}

	def act() = resolverLoop()
	

}

class RequestReceiver(address : String,fileName:String) extends Actor with IncomingMessageCallback{
	
	val resolvers : Array[Actor] = Array.fill(10) {new PathResolver(fileName)}
	
	resolvers.foreach(a => a.start)
	
	var index = 0
	
	val serverAgent : Agent = new Agent;
	
	val resolvedAddress = serverAgent.addListener(address);
	
	println("Central Ticket Office listening to : " + address)
	
	serverAgent.registerObject("central_ticket_server", this);
	
	
	def receiverLoop() {
		react {
			case Stop()	=> {
				serverAgent.close
				resolvers.foreach(a => a ! Stop())
				println("Bye")
			}
		}
	}
		
	
	def act() = receiverLoop()
	
	def call(im : IncomingMessage) {
		im.getMessageName match {
		
			case "resolve"	=>	{
				
				val traveler_index	= im.getParameters.getString("traveler_index")
				val from 			= im.getParameters.getString("from")
				val startNode 		= im.getParameters.getString("start_node")
				val to				= im.getParameters.getString("to")			
				
				resolvers(index) ! Resolve(startNode,from,to,traveler_index) 
		
				index += (index + 1) % 10
		
				var replyPar : Parameters = new Parameters
				
				replyPar.setString("result","OK");
				
				im.reply(replyPar)
			}
			
			case "terminate" => {
				println("terminate")
				this ! Stop()
			}
		}
	}
}

object Main extends App {
	override def main(argv : Array[String]) {
		if (argv.length < 3) {
			println("Please insert the json environment description, the name server address and the ticket server address")
			return;
		}

		PathResolver.NAME_SERVER_ADDRESS = argv(1)

		val receiver = new RequestReceiver(argv(2),argv(0))
		receiver.start
	}
}


