import scala.actors._
import com.inspirel.yami._

/**
 * Actor class used to handle Validation Requests
 */
class ValidationHandler extends Actor {
	
	def validationLoop {
		react {
			case HandleValidation(ticket,requestTime,message) => {
				
				val result = BookingManager !? Validate(ticket::List(),requestTime)
				
				var replyPar : Parameters = new Parameters
				
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
				
				message.reply(replyPar)
				
				validationLoop
			}
			
			case Stop() => {
				println("Shutting down ValidationHandler")
			}
		}
	}
	
	def act() = validationLoop
}
