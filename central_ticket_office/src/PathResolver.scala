import scala.actors._
import com.inspirel.yami._
import com.inspirel.yami.OutgoingMessage.MessageState._

class NoRouteFoundException extends Exception

trait Response
case class Error() extends Response
case class Ok(map:Map[String,String]) extends Response

object TicketCreator {
	
	val SERVICE_NAME = "ticket_creation";
	
	var NAME_SERVER_ADDRESS = "";
	
	// Map containg for each region the list of regions to go through to reach it 
	var regionsMap : Map[(String,String),List[(String,String)]] = TicketCreator.loadLinks("../../configuration/links.json")
	
	/**
	 * Loads the json file containg the location of regional ticket offices
	 *
	 * @return A table of entries (name,address)
	 */ 
	def jsonNodesListToMap(json : String) : Map[String,String] = {
		
		var res : Map[String,String] = Map()

		JSON.parseJSON(json).nodes.foreach(node => {
			res += node.name.toString -> node.address.toString
		})
	
		res	
	}
	
	/**
	 * This method loads a Map containing, for each couple of Regions, the list of 
	 * Regions and Gateway Stations to go from the first to the second one.
	 */ 
	def loadLinks(fileName : String) : Map[(String,String),List[(String,String)]] = {
		
		val json = scala.io.Source.fromFile(fileName).mkString
		
		var res : Map[(String,String),List[(String,String)]] = Map()
		
		val tree = JSON.parseJSON(json)
		
		for(i <- 0 until tree.links.size) {
			var pathsList = scala.collection.mutable.ListBuffer[(String,String)]()
			tree.links(i).paths.foreach(path => {pathsList += ((path.node.toString,path.station.toString))})
			res += ((tree.links(i).region1.toString,tree.links(i).region2.toString)) -> pathsList.toList
		}		

		res
	}
	
}

/**
 * Class TicketCreator is an Actor type allowing to create a Ticket where source and destination stations are
 * on different Regions.
 **/ 
class TicketCreator(messagesReceiver : Actor) extends Actor {
	
	/** 
	 * The list of couples (region,address)
	 */
	var nodesAddresses	: Map[String,String] = null
	
	/** 
	 * Yami agent used to send back to the Node the created Ticket, and to perform requests.
	 */
	val agent = new Agent
	
	/**
	 * A cache containing for each station the Region where it is located. 
	 */
	var pathRegionCache : Map[String,String] = Map() 
	
	
	def sendMessage(address:String,destObject:String,service:String,params:Map[String,String]) : Response = {
		var result 	= Map[String,String]()
		var ok 		= false
		try {
			val p = new Parameters
			
			if (params != null) params.keys.foreach(k => p.setString(k,params(k)))
			
			val message = agent.send(address,destObject,service,p)
			
			message.waitForCompletion()
			
			message.getState match {
				case REPLIED 	=> {
					PrintsSerializer ! Print("Message REPLIED from remote agent [" + 
									  destObject + "] service [" +service+ "] at " + address)
					val replyIterator = message.getReply.iterator
					while(replyIterator.hasNext) {
						val next = replyIterator.next()
						result += next.name -> next.getString()
					}
					ok = true
				}
				case REJECTED 	=> PrintsSerializer ! Print("ERROR: Message REJECTED from remote agent [" + 
													 destObject + "] service [" +service+ "] at " + address)
				case ABANDONED 	=> PrintsSerializer ! Print("ERROR: Message ABANDONED from remote agent [" + 
													 destObject + "] service [" +service+ "] at " + address)
				case _ 			=> 
			}
			
			
			
		} catch {
			case e : Throwable => PrintsSerializer ! Print("ERROR: Can not reach remote agent [" + destObject + "] service [" +service+ "] at " + address)
		}
		if (ok) Ok(result)
		else Error()
	}
	
	
	/**
	 * Requests the list of all the nodes registered to the Name Server.
	 * 
	 * @return : A table which contains for each node the address where it is located.
	 */
	def getNodesAddressList : Map[String,String] =  {
		sendMessage(TicketCreator.NAME_SERVER_ADDRESS,"name_server","list", Map()) match {
			case Ok(map) => TicketCreator.jsonNodesListToMap(map("list"))
			case Error() => null
		}
	}

	/**
	 *	For each couple (Node_Name,Address) search for destination <To>. 
	 *  
	 *	@return the name of the node containg the station <To>
	 *
	 **/
	def getRegionContaining(to : String) : String = {
		
		pathRegionCache get (to) match {
			case Some(region) => {
				// Cache contains a region for the Station, so return it.
				return region
			}
			case None => {
				val p = Map("station" -> to)
					
				// Region not in cache, so search for it.
				nodesAddresses.keys.foreach( node => {
					sendMessage(nodesAddresses(node),"message_handler","is_present",p) match {
						case Ok(map) => map get "response" match {
							case Some(s) => s match {
								case "TRUE" => {
									pathRegionCache += to -> node
									return node
								}
								case _ => 
							}
							case None 	=> 
						}
						case Error() => 
					}
				})
			}
		}
		null
	}
	
	/**
	 * Makes a synchronous call to node [node] in order to create a ticket from [form] to [to]
	 *
	 * @return the created ticket, or null if no ticket is created. 
	 *
	 **/
	def createTicket(node:String,from:String,to:String) : Ticket = {
		
		sendMessage(nodesAddresses(node),"message_handler","ticket_creation",Map("from"->from,"to"->to)) match {
			case Ok(map) => map get "response" match {
				case Some(s) => s match {
					case "RECEIVED" => map("ticket")
					case _ 	=> null
				}
				case None 	=> null
			}
			case Error() => null
		}
	}

	/**
	 *
	 * Method used to send an Error message to the Node which requested a ticket.
	 *
	 **/
	def sendError(dest : String, travelerIndex:String) {
		sendMessage(
			dest,
			"message_handler",
			"ticket_ready",
			Map(
				"response"->"ERROR",
				"traveler_index"->travelerIndex))
		// Tell MessagesReceiver that the Creation Request have been done
		messagesReceiver ! CreationRequestResolved()
	}
	
	/**
	 * Method used to send the created ticket back to the Client. If connection error
	 * occours, it simply discards the Ticket.
	 * 
	 **/
	def sendTicket(dest:String,travelerIndex:String,resultTicket:Ticket) {
	
		sendMessage(
			dest,
			"message_handler",
			"ticket_ready",
			Map(
				"response"->"OK",
				"traveler_index"->travelerIndex,
				"ticket"->Ticket.ticket2Json(resultTicket)))
		// Tell MessagesReceiver that the Creation Request have been done
		messagesReceiver ! CreationRequestResolved()
	}

	/**
	 * Main Loop for the resolver
	 */
	def resolverLoop() {
		react {
			case Resolve(startNode,from,to,travelerIndex,requestTime) => {
				PrintsSerializer ! Print("I have to resolve from " + from + " to " + to)
				
				try {
					
					// Check if node addresses list is null
					if (nodesAddresses == null) {
						nodesAddresses = getNodesAddressList
						// If the list is still null, send Error
						if (nodesAddresses == null) throw new NoRouteFoundException
					}
					
					// Obtain the Region containing the Station [To]
					getRegionContaining(to) match {
						case null => {
							PrintsSerializer ! Print("ERROR : No region found for station " + to)
							throw new NoRouteFoundException
						}
						
						case region : String => {
							// Region Found!
							PrintsSerializer ! Print("Station " + to + " is in Region " + region)

							// Now we have startNode and region to reach. We have to find
							// the gateways to cross to build a path					
							var ticketsBuffer = scala.collection.mutable.ListBuffer[Ticket]()
					
							// ****************** COLLECT PARTIAL TICEKTS FROM NODES *****************
					
							// Retrieve from regionsMap the path to go from [from] to [to]
							TicketCreator.regionsMap get (startNode,region) match {
								case Some(item) => {
									var node 	= startNode
									var f 		= from
									
									item match {
										case l : List[(String,String)] => {
											
											l.foreach(c => {
												PrintsSerializer ! Print("Search from "+f+" to "+c._2+" region " +  node)
												createTicket(node,f,c._2) match {
													case ticket : Ticket => ticketsBuffer += ticket
													case _ => throw new NoRouteFoundException	
												}
												node = c._1
												f = c._2
											})
											PrintsSerializer ! Print("Search from "+f+" to "+to+" region " +  node)
											
											createTicket(node,f,to) match {
												case ticket : Ticket => ticketsBuffer += ticket
												case _ => throw new NoRouteFoundException	
											}

										}
										case _ => throw new NoRouteFoundException
									}
								}
								case None => {
									PrintsSerializer ! Print("No path to reach " + region + " from " + startNode)
									throw new NoRouteFoundException			
								}
							}
		
							// *********************** VALIDATE COLLECTED TICKETS *************
							
							val tickets = ticketsBuffer.toList
					
							// Synchronous request to BookingManager, to verify whether the Ticket can be released or not
							BookingManager !? Validate(tickets,requestTime) match {
								case false => {
									PrintsSerializer ! Print("ERROR: The ticket can not be assigned")
									throw new NoRouteFoundException
								}
								case _ => // Do nothing,all ok!
							}
		
							// *********************** MERGE TICKETS *************************
							var resultTicket = tickets.reduceLeft((t1,t2) => Ticket.mergeTickets(t1,t2))
							//PrintsSerializer ! Print("\n** Final Ticket is:")
							resultTicket.print
		
							// ******************** Send the Ticket Back to the Node ********* 
						
							// Finally send the ticket back to the Node 
							sendTicket(nodesAddresses(startNode),travelerIndex,resultTicket)
							
						}
					}
			
				} catch {
					case e : NoRouteFoundException => {
						// If there was an error, send a notification to the requesting node.
						println(nodesAddresses)
						sendError(nodesAddresses(startNode),travelerIndex)
					}
				}	
				
				messagesReceiver ! RequestAsynchTask()
				
				resolverLoop
			}
			case Stop() => {
				println("Tearing down Path resolver")
				agent.close
			}
		}
	}
	
	/**
	 * Start the main loop
	 **/
	def act() = resolverLoop()
}

