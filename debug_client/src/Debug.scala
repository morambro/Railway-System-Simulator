import scala.actors._
import com.inspirel.yami._

case class Event(a : String)
case class Stop()

class DebugSender(val addr : String) extends Actor{
	
	val agent : Agent = new Agent
	
	def debugLoop() {
		react {
		
			case Event(message) => {
				val params : Parameters = new Parameters()
				params.setString("station","5")
				params.setString("platform","1")
				params.setString("traveler_index","1")
				params.setString("traveler","""
					{
						"traveler": {
						    "id": 1,
						    "name": "Gero",
						    "surname": "Caccamo"
						},
						"next_operation": 1,
						"destination": 2
					}
					
				""")
				params.setString("ticket","""
					
					{
						"next" 	 : 1,
						"ticket" : [
							{
								"start_station"				: 2,
								"next_station" 				: 3,
								"train_id"					: 2222,
								"region"					: "Node_1",
								"start_platform_index"		: 2,
								"destination_platform_index": 1
							},
							{
								"start_station"				: 3,
								"next_station" 				: 4,
								"train_id"					: 2222,
								"region"					: "Node_1",
								"start_platform_index"		: 1,
								"destination_platform_index": 1
							}
						]
					}
				""")
				println("Sending message to " + addr + " : " + message)
				agent.sendOneWay(addr,"central_ticket_server", "terminate", params);
				
				debugLoop
			}
		
			case Stop => println ("Bye!!")
		}
	}
	
	def act = debugLoop()
}


object DebugMain extends App {
	
	var sender : DebugSender = null; 
	
	def waitExit {
		readLine() match {
			case "q" | "Q" => {
				sender ! Stop()
				println ("Bye!")
			}
			case a:String => {
				sender ! Event(a)
				waitExit
			}
		}
	}
	
	override def main(args : Array[String]) {
		if (args.length < 1) {
			println ("ERROR: Controller tcp address must be specified");
			return;
		}
		
		println("Will send to " + args(0))
		
		sender = new DebugSender(args(0))
		sender.start
		
		waitExit
		
	} 
	
	
}
