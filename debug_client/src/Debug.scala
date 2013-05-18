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
//				if (message == "d") {
//					params.setString("request_time","2013-05-01 16:59:02")//""+new StringBuilder(new java.text.SimpleDateFormat("YYYY-MM-dd HH:mm:ss").format(new java.util.Date)))
//					params.setString("ticket","""
//					
//						{
//							"next" 	 : 1,
//							"ticket" : [
//								{
//									"start_station"				: 2,
//									"next_station" 				: 5,
//									"train_id"					: 1111,
//									"region"					: "Node_1",
//									"start_platform_index"		: 2,
//									"destination_platform_index": 1
//								}
//							]
//						}
//					""")
//					println("Sending message to " + addr + " : " + message)
//					agent.sendOneWay(addr,"central_ticket_server", "validate", params);
//				} else {
//					params.setInteger("route_index",1.intValue)
//					params.setInteger("current_run",4.intValue)
//					agent.sendOneWay(addr,"central_ticket_server", "update_run", params);
//				}
				params.setString("node_name","Node_1")
				params.setString("address","ciaociao")
				agent.sendOneWay(addr,"name_server", "remove", params);
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
