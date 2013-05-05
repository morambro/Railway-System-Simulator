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
	
	/**
	 * Actors Pool used to Handle Synchronous requests
	 */
	val synchReqHandlers 			: Array[Actor] = Array.fill(5)(new SynchRequestsHandler)
	
	// Resolvers initialization 
	ticketResolutionHandlers.foreach(a => a.start)
	// Start synch handlers
	synchReqHandlers.foreach(a => a.start)
	
	/**
	 * The index of the next Resolver to be used
	 */
	var ticketResolutionIndex = 0
	
	/**
	 * The index of the next Resolver to be used
	 */
	var synchHandlersIndex = 0
	
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
				synchReqHandlers.foreach(a => a ! Stop())
				println("Central Ticket Office is shutting down")
			}
		}
	}
		
	
	def act() = receiverLoop()
	
	
	def call(im : IncomingMessage) {
		// Case on the requested service. 
		im.getMessageName match {
		
			case "resolve"	=>	{
				
				println("Received RESOLVE request")
				
				// Retrieve all the parameters from the request
				val traveler_index	= im.getParameters.getString("traveler_index")
				val from 			= im.getParameters.getString("from")
				val startNode 		= im.getParameters.getString("start_node")
				val to				= im.getParameters.getString("to")
				val requestTime		= im.getParameters.getString("request_time")
				
				// Delegate Ticket Creation
				ticketResolutionHandlers(ticketResolutionIndex) ! Resolve(startNode,from,to,traveler_index,requestTime) 
		
				ticketResolutionIndex = (ticketResolutionIndex + 1)% ticketResolutionHandlers.size
				
				println("INDEX = " + ticketResolutionIndex)
			
				// Return immediately
				var replyPar : Parameters = new Parameters
				
				replyPar.setString("response","OK");
				
				im.reply(replyPar)
			}
			
			// First Call, returns the Entire Time Tables array 
			case "get_time_table" => {
				
				println("Received GET_TIME_TABLE request")
				
				// Dispatch to Synch request handler
				synchReqHandlers(synchHandlersIndex) ! HandleSynchRequest(GetTimeTable(),im)
				
				synchHandlersIndex += 1
				synchHandlersIndex = synchHandlersIndex % synchReqHandlers.size
				
			}
			
			// Service called by train to update the current run value.
			case "update_run" => {
				
				println("Received UPDATE_RUN request")
				
				val routeIndex 	= im.getParameters.getInteger("route_index").intValue
				val currentRun	= im.getParameters.getInteger("current_run").intValue
				
				// Ask to update the route timeTable
				synchReqHandlers(synchHandlersIndex) ! HandleSynchRequest(UpdateRun(routeIndex,currentRun),im)
				
				synchHandlersIndex += 1
				synchHandlersIndex = synchHandlersIndex % synchReqHandlers.size

			}
			
			case "validate" => {
				
				println("Received VALIDATE request")
				
				val ticket 		= im.getParameters.getString("ticket")
				val requestTime = im.getParameters.getString("request_time") 
				
				// Delegate Validation request to a ValidationHandler actorr
				synchReqHandlers(synchHandlersIndex) ! HandleSynchRequest(Validate(ticket::List(),requestTime),im)
				
				synchHandlersIndex += 1
				synchHandlersIndex = synchHandlersIndex % synchReqHandlers.size

			}
			
			case "terminate" => {
				println("Received TERMINATE request")
				this ! Stop()
			}
			
			case other => {
				println("ERROR: Invalid Service \""+other+"\"")
			}
		}
	}
}
