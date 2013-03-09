import scala.util.control.Breaks._
import java.util.Scanner 

object Main extends App{
	
	import it.name_server._
	
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
