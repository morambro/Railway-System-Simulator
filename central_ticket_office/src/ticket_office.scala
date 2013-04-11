import scala.actors._
import com.inspirel.yami._
import scala.util.parsing.json._

case class Stop()
case class Resolve(from:String,to:String)
case class Path(path : String)


class RegionalStation(parId : String, parAddress : String) {
	
	val id = parId;
	
	val address = parAddress;
	
}

object PathResolver {
	
	/**
	 * Loads the json file containg the location of regional ticket offices
	 *
	 * @return A list of couples (name,address)
	 */ 
	def load() : List[(String,String)] = {
		
		val json = scala.io.Source.fromFile("../env.json").mkString

		val json_elements = JSON.parseFull(json)
	
		var res : List[(String,String)] = List()

		json_elements match {
			case Some(el) => el match {
				case map : Map[String,List[_]] => {
					map.get("regional_offices") match {
						case Some(v : List[Map[String,String]]) 	=> {
							v.foreach( office => {
								val name : String = office("name")
								val address : String = office("address")
								res = (name,address) :: res
							})
						}
						case Some(_) 	=> println("Error") 
						case None 		=> println("Error")
					}
				}
				case _ => println("ciao")
			}
			case None 	=> println("Parsing Error")
		}
		
		res	
	}
	
	def ask(office : (String,String), station : String) : (String,String) = {
		if (office._1 == "office_2") return office
		return null
	}
	
}

class PathResolver extends Actor {
	
	// The list of couples <regional office,address> 
	var regional_offices	: List[(String,String)] = PathResolver.load();
	// Map containg for each region the list of regions to go through to reach it 
	var regions_map 		: Map[String,List[(String,String)]] = null

	def resolverLoop() {
		react {
			case Resolve(from,to) => {
				println("I have to resolve form " + from + " to " + to)
				
				var region : (String,String) = null;
				for {
					o <- regional_offices;
					if(region == null)   
				}{
					region = PathResolver.ask(o,to)
				}
				
				println("Station " + to + " is in Region " + region._1)
				
				sender ! Path("ciao")
				
				reply {
					Path("ciao")
				}
				resolverLoop
			}
			case Stop() => {
				println("Tearing down Path resolver")
			}
		}
	}

	def act() = resolverLoop()
	

}

class RequestReceiver(address : String, resolver : Actor) extends Actor with IncomingMessageCallback{
	
	val serverAgent : Agent = new Agent;
	val resolvedAddress 	= serverAgent.addListener(address);
	serverAgent.registerObject("cantral_ticket_server", this);
	
	def receiverLoop() {
		react {
			case Stop()	=> println("Bye")
			case Path(path) => println("The path is " + path)
		}
	}
		
	
	def act() = receiverLoop()
	
	def call(im : IncomingMessage) {
		im.getMessageName match {
		
			case "resolve"	=>	{
				
				
				val start 		= im.getParameters.getString("start")
				val destination = im.getParameters.getString("destination")			
				
				resolver ! Resolve(start,destination) 
				
			}
			
		}
	}
}

object Main extends App {

	val resolver = new PathResolver;resolver.start
	
	val receiver = new RequestReceiver("tcp://localhost:9999",resolver)
	receiver.start
	
}


