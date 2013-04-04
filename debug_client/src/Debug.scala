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
				params.setString("station","5");
				params.setString("train",message);
				println("Sending message to " + addr + " : " + message)
				agent.sendOneWay(addr,"message_handler", "train_transfer", params);
				
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
