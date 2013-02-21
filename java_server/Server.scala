import com.inspirel.yami._
import scala.actors._
import scala.util.control.Breaks._

class Printer_Callback extends IncomingMessageCallback {
	def call(im : IncomingMessage){
		println("Message : " + im.getParameters.getString("content"));
	}
}

class Event(val kind:String,val param : String)

class ServerAgent extends Actor{
	def act(){
		loop{
			react {
			
				case e : Event => {
					val serverAgent : Agent = new Agent

					val resolvedAddress = serverAgent.addListener(e.param);

					println("The server is listening on " + resolvedAddress);

					serverAgent.registerObject("printer", new Printer_Callback);

				}
				case "stop" => {
					break
				}	
			
			}
		}
	}
}

object Main extends App{
	override def main(args : Array[String]){
		if (args.length != 1) {
		    println("expecting one parameter: server destination")
		    return
		}
		
		val serverAddress : String = args(0)
		
		val agent = new ServerAgent
		agent . start
		agent ! new Event("start",serverAddress);
	}
}
