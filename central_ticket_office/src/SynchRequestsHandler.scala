import scala.actors._
import com.inspirel.yami._

/**
 * Actor class used to handle Synchronous Requests
 */
class SynchRequestsHandler extends Actor {
	
	def mainLoop {
		react {
			// Syncronous request received
			case HandleSynchRequest(request,incomingMessage) => {
				
				var replyPar : Parameters = new Parameters
				
				request match {
					
					// Validate request handling
					case validateRequest : Validate => {
						val result = BookingManager !? validateRequest//Validate(ticket::List(),requestTime)
				
						result match {
							case (true,ticketList:List[_]) => {
								replyPar.setString("response","TRUE")
								ticketList(0) match {
									case t:Ticket => replyPar.setString("ticket",t) 
								}
							}
							case false => replyPar.setString("response","FALSE")
							case ("error",msg:String) => {
								replyPar.setString("response","ERROR")
								replyPar.setString("message",msg)
							}
						}
					}
				
					// Uodate request handling
					case updateRequest : UpdateRun => {
						
						val result = BookingManager !? updateRequest
						
						result match {
							case ("updated",run_id:Int) => {
								replyPar.setString("response","OK")
								replyPar.setInteger("run_id",run_id.intValue)
							}
							case ("new_time_table",timeTable:String) => {
								replyPar.setString("response","UPDATED")
								replyPar.setString("new_time_table",timeTable)
							}
					
							case ("error",message:String) => {
								replyPar.setString("response","ERROR")
								replyPar.setString("message",message)
							}
						}
					}
					
					// Get Time Table Request handling
					case getTimeTableRequest : GetTimeTable => {
						
						val timeTable = BookingManager !? GetTimeTable
						
						replyPar.setString("response","OK")
						timeTable match {
							case tt : String => {
								replyPar.setString("time_tables",tt);println(tt)
							}

						}
					}
					
				}
				
				// Send response back to client
				incomingMessage.reply(replyPar)
				
				// Loop
				mainLoop
			}
			
			case Stop() => {
				println("Shutting down SynchREquestsHandler")
			}
			
			case _ => {
				println("ERROR: Invalid request Received")
				mainLoop
			}
		}
	}
	
	def act() = mainLoop
}
