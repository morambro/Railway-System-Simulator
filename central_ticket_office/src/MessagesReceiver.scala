import scala.actors._
import com.inspirel.yami._

/**
 * This class represents an actor responsible for keeping requests and dispatching them correctly.
 *
 **/ 
object MessagesReceiver extends Actor with IncomingMessageCallback{
	
	/**
	 * This boolean value tells if the first Marker message was received
	 */
	private var markerReceived : Boolean = false
	
	/**
	 * Actors Pool used to execute Ticket Resolution requests. 
	 */
	private val ticketResolutionHandlers : Array[Actor] = Array.fill(10) {new TicketCreator(this)}
	
	/**
	 * Actors Pool used to Handle Synchronous requests
	 */
	private val synchReqHandlers : Array[Actor] = Array.fill(5)(new SynchRequestsHandler)
	
	// Resolvers initialization 
	ticketResolutionHandlers.foreach(a => a.start)
	// Start synch handlers
	synchReqHandlers.foreach(a => a.start)
	
	/**
	 * The index of the next Resolver to be used
	 */
	private var ticketResolutionIndex = 0
	
	/**
	 * The index of the next Resolver to be used
	 */
	private var synchHandlersIndex = 0
	
	/**
	 * Yami Agent from which receive messages
	 */
	private val serverAgent : Agent = new Agent;
	
	
	private var creationRequests : Int = 0;
	
	
	def sendControllerAck() {
		val agent = new Agent
		val message : OutgoingMessage = agent.send(
			"tcp://localhost:8888",
			"central_controller", 
			"central_office_ack", 
			new Parameters)

		message.waitForCompletion

		message.getState match {
			case OutgoingMessage.MessageState.REPLIED => PrintsSerializer ! Print("Message Received")
			case OutgoingMessage.MessageState.REJECTED => 
				PrintsSerializer ! Print("The message has been rejected: " + message.getExceptionMsg)
			case _ => PrintsSerializer ! Print("The message has been abandoned.")
		}
		agent.close
	}
	
	/**
	 * Main loop. It is used to keep the Actor alive until Stop message is received
	 */
	def receiverLoop() {
		react {
			case Stop()	=> {
				// Stop all the Actors used
				PrintsSerializer ! Print("Central Ticket Office is shutting down...")
//				serverAgent.close
				ticketResolutionHandlers.foreach(a => a ! Stop())
				synchReqHandlers.foreach(a => a ! Stop())
				BookingManager ! Stop()
				PrintsSerializer ! StopPrint()
			}
			
			case CreationRequestResolved() => {
				creationRequests -= 1
				if (markerReceived && creationRequests == 0) {
					sendControllerAck()
				}
				receiverLoop()
			}
			
			case ("resolve",im:IncomingMessage) => {
				PrintsSerializer ! Print("Received RESOLVE request")
				
				creationRequests += 1
				
				// Retrieve all the parameters from the request
				val traveler_index	= im.getParameters.getString("traveler_index")
				val from 			= im.getParameters.getString("from")
				val startNode 		= im.getParameters.getString("start_node")
				val to				= im.getParameters.getString("to")
				val requestTime		= im.getParameters.getString("request_time")
				
				if(markerReceived) { 
					// If the First marker have been received, we have to buffer all the requests!
					queue.enqueue(Resolve(startNode,from,to,traveler_index,requestTime))
				}else {
					// Delegate Ticket Creation
					ticketResolutionHandlers(ticketResolutionIndex) ! Resolve(startNode,from,to,traveler_index,requestTime) 
		
					ticketResolutionIndex = (ticketResolutionIndex + 1)% ticketResolutionHandlers.size
				}
				// Return immediately
				var replyPar : Parameters = new Parameters
				
				replyPar.setString("response","OK");
				
				im.reply(replyPar)
				receiverLoop()
			}
			
		}
	}
	
	val queue = scala.collection.mutable.Queue[AsynchRequest]()
	
	def act() = react {
		case Init(address) => {
			// initialization
			val resolvedAddress = serverAgent.addListener(address);
	
			PrintsSerializer ! Print("Central Ticket Office listening to : " + address)
	
			// Register remote object "central_ticket_server"
			serverAgent.registerObject("central_ticket_server", this);
			receiverLoop()
		}
	}
	
	
	def call(im : IncomingMessage) {
		// Case on the requested service. 
		im.getMessageName match {
		
			case "start" => {
				// Initialize the booking manager.
				BookingManager ! InitBookingManager()
				im.reply(new Parameters)
			}
		
			case "resolve"	=>	{
				this ! ("resolve",im)				
			}
			
			// First Call, returns the Entire Time Tables array 
			case "get_time_table" => {
				
				PrintsSerializer ! Print("Received GET_TIME_TABLE request")
				
				// Dispatch to Synch request handler
				synchReqHandlers(synchHandlersIndex) ! HandleSynchRequest(GetTimeTable(),im)
				
				synchHandlersIndex += 1
				synchHandlersIndex = synchHandlersIndex % synchReqHandlers.size
				
			}
			
			// Service called by train to update the current run value.
			case "update_run" => {
				
				PrintsSerializer ! Print("Received UPDATE_RUN request")
				
				val routeIndex 	= im.getParameters.getInteger("route_index").intValue
				val currentRun	= im.getParameters.getInteger("current_run").intValue
				
				// Ask to update the route timeTable
				synchReqHandlers(synchHandlersIndex) ! HandleSynchRequest(UpdateRun(routeIndex,currentRun),im)
				
				synchHandlersIndex += 1
				synchHandlersIndex = synchHandlersIndex % synchReqHandlers.size

			}
			
			case "validate" => {
				
				PrintsSerializer ! Print("Received VALIDATE request")
				
				val ticket 		= im.getParameters.getString("ticket")
				val requestTime = im.getParameters.getString("request_time") 
				
				// Delegate Validation request to a ValidationHandler actorr
				synchReqHandlers(synchHandlersIndex) ! HandleSynchRequest(Validate(ticket::List(),requestTime),im)
				
				synchHandlersIndex += 1
				synchHandlersIndex = synchHandlersIndex % synchReqHandlers.size

			}
			
			case "marker" => {
				// Now the receiver knows it have to buffer all asynchronous requests, such as ticket creation requests
				PrintsSerializer ! Print("Received MARKER request")
				
				// Return immediately
				var replyPar : Parameters = new Parameters
				
				replyPar.setString("response","OK");
				
				im.reply(replyPar)
				
				if (markerReceived) {
					// Terminate!
					// Save to file (?)
					// Tear down all Actors!
					PrintsSerializer ! Print("Marker received twice")
					this ! Stop()
				}else{
					PrintsSerializer ! Print("Setting markerReceived = TRUE")
					markerReceived = true
					// If when the marker first arrived, there are no 
					// pending creation requests, send the ack immefiately 
					if (creationRequests == 0) {
						PrintsSerializer ! Print("Send Ack to the controller")
						sendControllerAck()
					}
				}
			}
			
			case other => {
				PrintsSerializer ! Print("ERROR: Invalid Service \""+other+"\"")
			}
		}
	}
}
