import com.inspirel.yami._
import scala.actors._
import scala.util.control.Breaks._
import java.util.Scanner 

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
			println("The Name Server is listening at " + resolvedAddress);
			serverAgent.registerObject("name_server", this);
			my_loop

		}
		case e : Stop => {
			println ("Bye")
			serverAgent.close
		}	
	}

	def call(im : IncomingMessage) {
	
		println("Received Message "+im.getMessageName)
	
		im.getMessageName match {
		
			case "add" => {
				val node_name 	= im.getParameters.getString("node_name") 
				val address 	= im.getParameters.getString("address")
	
				addresses += (node_name -> address)
	
				println("Total entries : ")
				for( key <- addresses.keys )
					(addresses get key) match{
						case Some(x) => println("entry " + key + "  =>  " + x)
						case None => println("")
					}
				println
		
				val replyParams : Parameters = new Parameters;

				replyParams.setString("response","ok");
		
				im.reply(replyParams)
			}
		
			case "get" => {
				val key 	= im.getParameters.getString("node_name")
				
				val replyParams : Parameters = new Parameters;
				addresses get key match {
					case Some(x) => {
						println("Found entry : " + key + "  =>  " + x)
						replyParams.setString("response",x);
					}
					case None => {
						println("Entry " + key + " not found")
						replyParams.setString("response","_");
					}
				}
			
				im.reply(replyParams)
			}
		
			case "all" => {
				
				val replyParams : Parameters = new Parameters;
			
				var response = """{ "nodes" : [ """
				
				var i = 0
				addresses.keys.foreach( k => {
					response += "{"
					response += """ "name"    : """ + "\""+ k +"\","
					response += """ "address" : """ + "\""+ addresses(k) +"\""
					response += "}"
					if (i < addresses.keys.size-1)
						response += ","
					i += 1
				})
				
				response += "]}"
				
				println(response)
				
				replyParams.setString("result",response)
				
				im.reply(replyParams)
				
				
			}
			case _ => println("Invalid operation " + im.getMessageName)
		
		}
	
	}
}


object Main extends App{
	override def main(args : Array[String]){
		if (args.length != 1) {
		    println("ERROR: expecting server destination as parameter")
		    return
		}
		
		val serverAddress : String = args(0)
		
		val agent = new ServerAgent
		agent.start
		agent ! new Start(serverAddress);
		
		for( ln <- io.Source.stdin.getLines){
			if (ln == "quit") {
				agent ! Stop;
				break
			}
		}
		
	}
}
