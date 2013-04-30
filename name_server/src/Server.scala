import com.inspirel.yami._
import scala.actors._
import scala.util.control.Breaks._
import java.util.Scanner 

case class Start(val param : String)
case class Stop()

class ServerAgent extends Actor with IncomingMessageCallback{

	var serverAgent : Agent = null;
	var addresses	: Map[String,String] = Map();

	def act() =	myLoop

	def myLoop : Unit = react {
		
		case Start(param) => {
			serverAgent  = new Agent
			val resolvedAddress = serverAgent.addListener(param);
			println("The Name Server is listening at " + resolvedAddress);
			serverAgent.registerObject("name_server", this);
			myLoop

		}
		case Stop() => {
			println ("closing yami agent...")
			serverAgent.close
		}
	}

	def call(im : IncomingMessage) {
	
		println("Received Message "+im.getMessageName)
	
		im.getMessageName match {
		
			case "bind" => {
				
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
		
			case "resolve" => {
				
				val key 	= im.getParameters.getString("node_name")
				
				val replyParams : Parameters = new Parameters
				
				// Try to resolve the Name.
				addresses get key match {
					case Some(x) => {
						println("Found entry : " + key + "  =>  " + x)
						// Set Ok response
						replyParams.setString("response","OK");
						// Add the resolved address
						replyParams.setString("address",x);
					}
					case None => {
						println("Entry " + key + " not found")
						replyParams.setString("response","ERROR");
					}
				}
			
				im.reply(replyParams)
			}
		
			case "list" => {
				
				val replyParams : Parameters = new Parameters;
			
				var list = """{ "nodes" : [ """
				
				var i = 0
				addresses.keys.foreach( k => {
					list += "{"
					list += """ "name"    : """ + "\""+ k +"\","
					list += """ "address" : """ + "\""+ addresses(k) +"\""
					list += "}"
					if (i < addresses.keys.size-1)
						list += ","
					i += 1
				})
				
				list += "]}"
				
				replyParams.setString("response","OK")
				replyParams.setString("list",list)
				
				im.reply(replyParams)
				
				
			}
			case "remove" => {
				val node_name = im.getParameters.getString("node_name") 
				
				addresses -= (node_name)
				
				val replyParams : Parameters = new Parameters;
				
				replyParams.setString("response","OK")
				im.reply(replyParams)
				
			}
			case _ => println("Invalid operation " + im.getMessageName)
		
		}
	
	}
}


object Main extends App{

	var agent : ServerAgent = null;

	def readInput() {
		readLine() match {
			case "q" | "Q" | "Quit" | "quit" | "QUIT" => {
				println("Quit");
				agent ! Stop()
			}
			case _ => readInput
		}
	}

	override def main(args : Array[String]){
		if (args.length != 1) {
		    println("ERROR: expecting server destination as parameter")
		    return
		}
		agent = new ServerAgent;
		val serverAddress : String = args(0)
		agent.start
		agent ! Start(serverAddress);

		readInput		
		
	}
}
