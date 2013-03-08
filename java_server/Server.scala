import com.inspirel.yami._
import scala.actors._
import scala.util.control.Breaks._
import java.util.Scanner; 


case class Start(val param : String)
case class Stop()

class ServerAgent extends Actor with IncomingMessageCallback{
	
	var serverAgent : Agent = null;
	var addresses	: Map[String,String] = Map();
	
	def act() {
		my_loop
	}
	
	def my_loop : Unit = react {
			
		case e : Start => {
			serverAgent  = new Agent
			val resolvedAddress = serverAgent.addListener(e.param);
			println("The server is listening on " + resolvedAddress);
			serverAgent.registerObject("name_server", this);
			my_loop

		}
		case e : Stop => {
			println ("Bye")
			serverAgent.close
		}	
	}
	
	def call(im : IncomingMessage) {
		
		im.getMessageName match {
			
			case "add" => {
				val name 	= im.getParameters.getString("name")
				val address = im.getParameters.getString("address")
		
				addresses += (name -> address)
		
				println("Total entries : ")
				for( key <- addresses.keys )
					(addresses get key) match{
						case Some(x) => println("entry " + key + "  =>  " + x)
						case None => println("")
					}
				println
			
				val replyParams : Parameters = new Parameters;

				replyParams.setString("address","ok");
			
				im.reply(replyParams)
			}
			
			case "get" => {
				val key = im.getParameters.getString("name")
				val replyParams : Parameters = new Parameters;
				addresses get key match {
					case Some(x) => {
						println("Found entry : " + key + "  =>  " + x)
						replyParams.setString("address",x);
					}
					case None => {
						println("Entry " + key + " not found")
						replyParams.setString("address","_");
					}
				}
				
				im.reply(replyParams)
			}
			
			case _ => println("Invalid operation " + im.getMessageName)
			
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
		agent.start
		agent ! new Start(serverAddress);
		
		val serverAgent : Agent = new Agent
		
		for( ln <- io.Source.stdin.getLines){
			if (ln == "quit") {
				agent ! Stop;
				break
			}
		}
		
	}
}
