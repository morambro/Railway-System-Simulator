import scala.actors._
import com.inspirel.yami._
import net.minidev.json._

class NoRouteFoundException extends Exception

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
	
	/** 
	 * The list of couples <regional office,address> 
	 */
	var nodesAddresses	: Map[String,String] = null
	
	/** 
	 * Agent used to send back to the Node the created Ticket, and to perform requests.
	 */
	val agent = new Agent
	
	/**
	 * Requests the list of all the nodes registered to the Name Server.
	 * 
	 * @return : A table which contains for each node the address where it is located.
	 */
	def getNodesAddressList : Map[String,String] =  {
		val message : OutgoingMessage = agent.send(
			PathResolver.NAME_SERVER_ADDRESS,
		    "name_server", 
		    "list", 
		    new Parameters);
		    
		message.waitForCompletion
		
		// Received the response by name server
		message.getState match {
			case OutgoingMessage.MessageState.REPLIED => {
			
		    	val reply = message.getReply();
			
				var nodes : String = null
				if (reply.getString("response") == "OK") {
					nodes = reply.getString("list")
				}
		
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
			
						val found = reply.getString("response")
		
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
	 * Makes a synchronous call to node [node] in order to create a ticket from [form] to [to]
	 *
	 * @return the created ticket, or null if no ticket is created. 
	 *
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
					case _ => println("ERROR")
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

	/**
	 * Method used to send an Error message to the Node.
	 */
	def sendError(dest : String, traveler_index:String) {
		val p = new Parameters

		p.setString("response","ERROR")
		p.setString("traveler_index",traveler_index)
		
		
		
		val message : OutgoingMessage = agent.send(
			dest,
			"message_handler", 
			"ticket_ready", 
			p)
		
		message.waitForCompletion
		
		message.getState match {
			case OutgoingMessage.MessageState.REPLIED => {
		    	println("Error message has been received");
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
			case Resolve(startNode,from,to,traveler_index,requestTime) => {
				println("I have to resolve from " + from + " to " + to)
				
				try {
				
					if (nodesAddresses == null)
						nodesAddresses = getNodesAddressList
					if (nodesAddresses == null) 
						throw new NoRouteFoundException
					
					val region = ask(to)
	
					if (region == null) {
						println("ERROR : No region found for station " + to)
						throw new NoRouteFoundException
					} 
					
					println("Station " + to + " is in Region " + region)

					// Now we have startNode and region to reach. We have to find
					// the gateways to cross to build a path					
					var tickets : List[Ticket] = List()
					
					// ****************** COLLECT PARTIAL TICEKTS FROM NODES *****************
					
					// Retrieve from regionsMap the path to go from [from] to [to]
					PathResolver.regionsMap get (startNode,region) match {
						case Some(item) => {
							var node 	= startNode
							var f 		= from
							item match {
								case l : List[_] => {
									l.foreach(c => {
										println("Search from "+f+" to "+c._2+" region " +  node)
										val askedTicket = ask(node,f,c._2)
										if (askedTicket != null)
											tickets = tickets :+ askedTicket
										else
											throw new NoRouteFoundException	
										node = c._1
										f = c._2
									})
									println("Search from "+f+" to "+to+" region " +  node)
									val askedTicket = ask(node,f,to) 
									if (askedTicket != null)
										tickets = tickets :+ askedTicket
									else
										throw new NoRouteFoundException	
								}
							}
						}
						case None => {
							println("No path to reach " + region + " from " + startNode)
							throw new NoRouteFoundException			
						}
					}
		
					// *********************** VALIDATE COLLECTED TICKETS *************
					
					val result = BookingManager !? Validate(tickets,requestTime)
					result match {
						case false => {
							println("ERROR: The ticket can not be assigned")
							throw new NoRouteFoundException
						}
					}
					
					// If we are here, we have a valid ticket
					var result_ticket : Ticket = tickets(0)
		
		
					// *********************** MERGE TICKETS *************************
					
					for (i <- 1 until tickets.length) {
						result_ticket = Ticket.mergeTickets(result_ticket,tickets(i))
					}
		
					println("\n** Final Ticket is:")
					result_ticket.print
		
					// ******************** Send the Ticket Back to the Node ********* 
		
					//print(Ticket.ticket2Json(result_ticket))
		
					// Finally send the ticket back to the Node 
					val p = new Parameters
					
					p.setString("response","OK")
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
			
				} catch {
					case e : NoRouteFoundException => {
						// If there was an error, send a notification to the requesting node.
						sendError(nodesAddresses(startNode),traveler_index)
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

