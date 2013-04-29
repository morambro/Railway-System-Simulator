import scala.actors._
import com.inspirel.yami._
import net.minidev.json._

/**
 * This class represents an actor responsible for keeping requests and dispatching them correctly.
 *
 **/ 
class RequestReceiver(address : String,fileName:String) extends Actor with IncomingMessageCallback{
	
	/**
	 * Actors Pool used to execute Ticket Resolution requests. 
	 */
	val ticketResolutionHandlers 	: Array[Actor] = Array.fill(10) {new PathResolver(fileName)}
	
	// Resolvers initialization 
	ticketResolutionHandlers.foreach(a => a.start)
	
	val validationHandlers 			: Array[Actor] = Array.fill(5)(new ValidationHandler)
	
	validationHandlers.foreach(a => a.start)
	
	/**
	 * The index of the next Resolver to be used
	 */
	var ticketResolutionIndex = 0
	
	/**
	 * The index of the next Resolver to be used
	 */
	var validationIndex = 0
	
	/**
	 * Yami Agent from which receive messages
	 */
	val serverAgent : Agent = new Agent;
	
	// initialization
	val resolvedAddress = serverAgent.addListener(address);
	
	println("Central Ticket Office listening to : " + address)
	
	// Register remote object "central_ticket_server"
	serverAgent.registerObject("central_ticket_server", this);
	
	/**
	 * Main loop. It is used to keep the Actor alive until Stop message is received
	 */
	def receiverLoop() {
		react {
			case Stop()	=> {
				serverAgent.close
				ticketResolutionHandlers.foreach(a => a ! Stop())
				validationHandlers.foreach(a => a ! Stop())
				println("Central Ticket Office is shutting down")
			}
		}
	}
		
	
	def act() = receiverLoop()
	
	
	def call(im : IncomingMessage) {
		// Case on the requested service. 
		im.getMessageName match {
		
			case "resolve"	=>	{
				
				// Retrieve all the parameters from the request
				val traveler_index	= im.getParameters.getString("traveler_index")
				val from 			= im.getParameters.getString("from")
				val startNode 		= im.getParameters.getString("start_node")
				val to				= im.getParameters.getString("to")
				val requestTime		= im.getParameters.getString("request_time")
				
				ticketResolutionHandlers(ticketResolutionIndex) ! Resolve(startNode,from,to,traveler_index,requestTime) 
		
				ticketResolutionIndex = (ticketResolutionIndex + 1)% ticketResolutionHandlers.size
				
				println("INDEX = " + ticketResolutionIndex)
		
				var replyPar : Parameters = new Parameters
				
				replyPar.setString("response","OK");
				
				im.reply(replyPar)
			}
			
			// First Call, returns the Entire Time Tables array 
			case "get_time_table" => {
				
				BookingManager ! GetTimeTable()
				
				var replyPar : Parameters = new Parameters
				
				receive {
					case ("time_tables",tt:String) => {
						replyPar.setString("response","OK")
						replyPar.setString("time_tables",tt)
					}
				}
				
				im.reply(replyPar)
				
			}
			
			// Service called by train to update the current run value.
			case "update_run" => {
				// ...
				val trainID 	= im.getParameters.getInteger("train_id").intValue
				val routeIndex 	= im.getParameters.getInteger("route_index").intValue
				val current_run	= im.getParameters.getInteger("current_run").intValue
				
				// Ask to update the route timeTable
				BookingManager ! UpdateRun(trainID,routeIndex,current_run)
				
				var replyPar : Parameters = new Parameters
				
				receive {
					case "updated" => replyPar.setString("response","OK")
				
					case ("new_time_table",timeTable:String) => {
						replyPar.setString("response","OK")
						replyPar.setString("new_time_table",timeTable)
					}
					
					case ("error",message:String) => {
						replyPar.setString("response","ERROR")
						replyPar.setString("message",message)
					}
				}
				
				im.reply(replyPar)
			}
			
			case "validate" => {
				
				val ticket 		= im.getParameters.getString("ticket")
				val requestTime = im.getParameters.getString("request_time") 
				
				
				// Delegate Validation request to a ValidationHandler actorr
				validationHandlers(validationIndex) ! HandleValidation(ticket,requestTime,im)
				
				validationIndex += 1
				validationIndex = validationIndex % validationHandlers.size
				
				//println("validation delegated")

			}
			
			case "terminate" => {
				println("terminate")
				this ! Stop()
			}
			
			case other => {
				println("ERROR: Invalid Service \""+other+"\"")
			}
		}
	}
}
