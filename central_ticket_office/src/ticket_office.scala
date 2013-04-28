import scala.actors._
import com.inspirel.yami._
import net.minidev.json._
import java.util.Date._

case class Stop()
case class Resolve(startNode:String,from:String,to:String,traveler_index:String)
case class Path(path : String)
case class Error(message:String)

class NoRouteFoundException extends Exception

class RouteStage {
	
	var startStation 	: Int = 1
	var nextStation 	: Int = 1
	var startPlatform 	: Int = 1
	var nextPlatform 	: Int = 1
	var nextSegment 	: Int = 1
	var nodeName 		: String = ""
	var leaveAction 	: String = ""
}

class Train(val id:Int,val routeIndex:Int,val sitsNumber:Int)

object Route {
	
	/**
	 * Loads the Routes from configuration file [fileName]
	 * in the list [routes].
	 */ 
	def loadRoutes(fileName : String) : Array[Route] = {
		
		val jsonRoutes = scala.io.Source.fromFile(fileName).mkString
		
		var routesList : List[Route] = List()
		
		JSONValue.parseStrict(jsonRoutes) match {
			case o : JSONObject => o.get("routes") match {
				case allRoutes : JSONArray => {
					// for each Route found
					for (i <- 0 until allRoutes.size) {
						allRoutes.get(i) match {
							case r : JSONObject => {
								var route = new Route
								// Set the id field
								route.id = r.get("id") match {
									case id : java.lang.Integer => id
								}
								// set the stages filed
								r.get("route") match {
									case stagesArray : JSONArray => {
										for ( j <- 0 until stagesArray.size) {
											stagesArray.get(j) match {
												case s : JSONObject => {
													var stage = new RouteStage
													stage.startStation = s.get("start_station") match {
														case ss : java.lang.Integer => ss.intValue
													}									
													stage.nextStation = s.get("next_station") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.startPlatform = s.get("start_platform") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.nextPlatform = s.get("platform_index") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.nextSegment = s.get("next_segment") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.nodeName = s.get("node_name") match {
														case ss : String => ss
													}
													stage.nodeName = s.get("leave_action") match {
														case ss : String => ss
													}
													stage.nodeName = s.get("enter_action") match {
														case ss : String => ss
													}
													// Now add the created stage to the current route
													route.stages = route.stages :+ stage
												}
											}
										}
									}
								}
								
								// Add the route to routes List
								routesList = routesList :+ route
								
							}
						}
					}
				}
			}
		}
		
		
		routesList.toArray
	}
	
	/** 
	 * Loads from file a Map with FB Trains information
	 */
	def loadTrainsRoutesMap(fileName : String) : Map[Int,Train] = {
		
		// Now load for each train its route.
		val jsonTrain = scala.io.Source.fromFile(fileName).mkString
		
		var trainRouteMap : Map[Int,Train] = Map()
		
		JSONValue.parseStrict(jsonTrain) match {
			case o : JSONObject => o.get("trains") match {
				case trains : JSONArray => {
					for (i <- 0 until trains.size) {
						trains.get(i) match {
							case train : JSONObject => {
								train.get("type") match {
									case "fb" => {
										val id = train.get("id") match {
											case i : java.lang.Integer => i.intValue 
										}
										val routeIndex = train.get("route_index") match {
											case i : java.lang.Integer => (i.intValue - 1)
										}
										val sitsNumber = train.get("sits_number") match {
											case i : java.lang.Integer => i.intValue
										}
										trainRouteMap = trainRouteMap + Tuple2(id,new Train(id,routeIndex,sitsNumber))
									}
									case _ => // DO NOTHING
								}
							}
						}
					}
				}
			}
		}
		// trainRouteMap.keys.foreach( k => {println("trainID = " + k + " , route = " + trainRouteMap(k))})
		trainRouteMap
	}
	
	
}

/**
 * This class represents a route.
 */
class Route {
	var id : Int = 0
	var stages : List[RouteStage] = List()
	
	def print () {
		println("ID = " + this.id)
		this.stages.foreach( s => 
			println("start startStation = " + s.startStation)
		)
	}
}


object Ticket {
	
	/**
	 * Given two tickets, this function returns a new ticket merging them.
	 * 
	 * @return the new Ticket object
	 */
	def mergeTickets (T1:Ticket,T2:Ticket) : Ticket =  {
		val ticket = new Ticket(T1.next)
		if ((T1.stages.last.destinationPlatform == T2.stages(0).startPlatform) && (T1.stages.last.trainId == T2.stages(0).trainId)) {
			ticket.stages = T1.stages.dropRight(1) ++ ( new Ticket_Stage(
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
	
	/**
	 * Performs implicit conversion from String to Ticket.
	 */
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
			
			json += """ "start_station" : """ + stage.startStation + ","
			json += """ "next_station" : """ + stage.nextStation + ","
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


case class Validate(ticket : Ticket);
/**
 * This object is responsible of serializing the validation requests.
 */
object BookingManager extends Actor {
	
	/**
	 * All FB Trains 
	 */
	var trainRouteMap : Map[Int,Train] = Route.loadTrainsRoutesMap("../../railway/res/trains.json")
	
	/**
	 * All the routes
	 */
	var routes : Array[Route] = Route.loadRoutes("../../railway/res/routes.json")
	
	/**
	 * An Array where for each route contains the number of free sits.
	 */
	var bookingSits : Array[List[Int]] = Array.fill(routes.size)(List())

	// bookingSits initialization
	trainRouteMap.keys.foreach( trainID => {
		val routeIndex = trainRouteMap(trainID).routeIndex 
		val size = routes(routeIndex).stages.size
		for (i <- 0 until size) 
			bookingSits(routeIndex) = bookingSits(routeIndex) :+ trainRouteMap(trainID).sitsNumber
	})
	
	import java.text.SimpleDateFormat
	
	class RouteTimeTable(val routeIndex:Int,val runs:Int,val span:Int,var table :Array[List[java.util.Date]]) {
		var run 	: Int = 0
		
		if (table == null)  {
			table = Array.fill(runs)(List())
		
			for (i <- 0 until runs) {
				for (j <- 0 until routes(routeIndex).stages.size) {
					table(i) = table(i) :+ new java.util.Date()
				}
			}
		}
		
		def printDate () {
			println("Runs for route : " + routeIndex )
			table.foreach(list => {
				list.foreach(date => {
					print(new StringBuilder( new SimpleDateFormat("HH:mm:ss").format(date) ) + " , ")})
				println
			})
		}
		
	}
	
	
	var timeTables : Array[RouteTimeTable] = new Array(routes.size)
	
	val jsonTimeTable = scala.io.Source.fromFile("../../railway/res/time_table.json").mkString
	
	val ref = new java.util.Date() 
	
	/**
	 * Loading time table
	 */ 
	JSONValue.parseStrict(jsonTimeTable) match {
		case o : JSONObject => o.get("time_table") match {
			case timeTable : JSONArray => {
				println(timeTable.size)
				for (j <- 0 until timeTable.size) {
					timeTable.get(j) match {
						case runTable : JSONObject => {
							val routeIndex : Int = runTable.get("route") match {
								case o : java.lang.Integer => o.intValue
							}
							val span : Int = runTable.get("restart_span") match {
								case o : java.lang.Integer => o.intValue
							}
							
							runTable.get("time") match {
								case runs : JSONArray => {
									var table : Array[List[java.util.Date]] = Array.fill(runs.size)(List())
									for (k <- 0 until runs.size) {
										runs.get(k) match {
											case run : JSONArray => {
												for (h <- 0 until run.size) {
													run.get(h) match {
														case i : java.lang.Integer => {
															table(k) = table(k) :+ new java.util.Date(ref.getTime+(i*60000))
														}
													}
												}
											}
										}
									}
									timeTables(j) = new RouteTimeTable((routeIndex-1),5,span,table)
									timeTables(j).printDate
								}
							}
							
						}
					}
				}
			} 
		}
	}
	
	/** 
	 * Main loop for the Actor.
	 */
	def bookingLoop() {
		react {
		
			// Handles a Validate request.
			case Validate(ticket) => {
				println ("Validating ticket: ")
				
				var valid : Boolean = true
				
				ticket.stages.foreach ( ticketStage => {
					
					var firstIndex 	= -1
					var secondIndex = -1
					
					// Retrieve the route index
					var routeIndex = trainRouteMap(ticketStage.trainId).routeIndex 
					
					println("\n Curren index = " + routeIndex)
					println("startStation = " + ticketStage.startStation)
					println("nextStation = " + ticketStage.nextStation)
					
					// match in first half
					
					for (i <- 0 until routes(routeIndex).stages.size/2) {
						if (routes(routeIndex).stages(i).startStation == ticketStage.startStation) {
							firstIndex = i 
						}
						if (routes(routeIndex).stages(i).nextStation == ticketStage.nextStation) {
							secondIndex = i
						}
					}
					if (firstIndex > secondIndex || firstIndex == -1 || secondIndex == -1) {
						// If we haven't a match in the first half, search in the second half.
						for (i <- routes(routeIndex).stages.size/2 until routes(routeIndex).stages.size) {
							if (routes(routeIndex).stages(i).startStation == ticketStage.startStation) {
								firstIndex = i 
							}
							if (routes(routeIndex).stages(i).nextStation == ticketStage.nextStation) {
								secondIndex = i
							}
						}
					}
					// At this point firstIndex will be the first index of the route,
					// secondIndex the last
					for (i <- firstIndex to secondIndex) {
						valid = bookingSits(routeIndex)(i) > 0
					}
									
				})
				
				// Send the response back
				if (valid) sender ! true
				else sender ! false
				
				bookingLoop
			}
			case Stop => {
				println("Validator shutted down")
			}
		}
	}
	
	def act = bookingLoop
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
			case Resolve(startNode,from,to,traveler_index) => {
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
		
					tickets.foreach ( t => {
						BookingManager ! Validate(t)
						receive {
							case false => {
								println("ERROR: The ticket can not be assigned")
								throw new NoRouteFoundException
							}
						}
					})
					
					// If we are here, we have a valid ticket
					var result_ticket : Ticket = tickets(0)
		
					// Merge the tickets
					for (i <- 1 until tickets.length) {
						result_ticket = Ticket.mergeTickets(result_ticket,tickets(i))
					}
		
					println("\n** Final Ticket is:")
					result_ticket.print
		
					// Send the Ticket Back to the Node 
		
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
		
				index = (index + 1)% 10
				
				println("INDEX = " + index)
		
				var replyPar : Parameters = new Parameters
				
				replyPar.setString("response","OK");
				
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
			println("""Please insert:
   1) The ticket server address;
   2) The name server address;
   3) The json environment description.""")
			return
		}
		
		println(argv(0))
		println(argv(1))
		println(argv(2))
		
		
		val receiver = new RequestReceiver(argv(0),argv(2))
		receiver.start
		
		PathResolver.NAME_SERVER_ADDRESS = argv(1)
		
		BookingManager.start
		
	}
}


