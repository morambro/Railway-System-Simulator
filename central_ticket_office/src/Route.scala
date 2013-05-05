import scala.collection.mutable.ListBuffer

class Train(val id:Int,val routeIndex:Int,val sitsNumber:Int)

class RouteStage {
	
	var startStation 	: Int = 1
	var nextStation 	: Int = 1
	var startPlatform 	: Int = 1
	var nextPlatform 	: Int = 1
	var nextSegment 	: Int = 1
	var nodeName 		: String = ""
	var leaveAction 	: String = ""
	var enterAction 	: String = ""
	
	def print {
		PrintsSerializer ! Print("startStation = " + startStation)
		PrintsSerializer ! Print("nextStation = " + nextStation)
		PrintsSerializer ! Print("startPlatform = " + startPlatform)
		PrintsSerializer ! Print("nextPlatform = " + nextPlatform)
		PrintsSerializer ! Print("nextSegment = " + nextSegment)
		PrintsSerializer ! Print("nodeName = " + nodeName)
		PrintsSerializer ! Print("leaveAction = " + leaveAction)
		PrintsSerializer ! Print("enterAction = " + enterAction)
	}
}

object Route {
	
	/**
	 * Loads the Routes from configuration file [fileName]
	 * in the list [routes].
	 */ 
	def loadRoutes(fileName : String) : Array[Route] = {
		
		val jsonRoutes = scala.io.Source.fromFile(fileName).mkString
		
		var routesList : ListBuffer[Route] = ListBuffer()
		
		JSON.parseJSON(jsonRoutes).routes.foreach(route => {
			val stages = ListBuffer[RouteStage]()
			var routeObj = new Route
			routeObj.id = route.id.toInt
			route.route.foreach(stage => {
				val stageObj = new RouteStage
				stageObj.startStation = stage.start_station.toInt
				stageObj.nextStation = stage.next_station.toInt
				stageObj.startPlatform = stage.start_platform.toInt
				stageObj.nextPlatform = stage.platform_index.toInt
				stageObj.nextSegment = stage.next_segment.toInt
				stageObj.nodeName = stage.node_name.toString
				stageObj.leaveAction = stage.leave_action.toString
				stageObj.enterAction = stage.enter_action.toString
				
				stages += stageObj
			})
			routeObj.stages = stages.toList
			routesList = routesList :+ routeObj
		})
		
		routesList.toArray
	}
	
	/** 
	 * Loads from file a Map with FB Trains information
	 */
	def loadTrainsRoutesMap(fileName : String) : Map[Int,Train] = {
		
		// Now load for each train its route.
		val jsonTrain = scala.io.Source.fromFile(fileName).mkString
		
		var trainRouteMap : Map[Int,Train] = Map()
		
		JSON.parseJSON(jsonTrain).trains.foreach(jsonTrain => {
			jsonTrain.selectDynamic("type").toString match {
				case "fb" => {
					trainRouteMap += jsonTrain.id.toInt -> new Train(
						jsonTrain.id.toInt,
						jsonTrain.route_index.toInt - 1,
						jsonTrain.sits_number.toInt)
				}
				case _ =>
			}
		})
		
		
		trainRouteMap
	}
	
	
}

/**
 * This class represents a route.
 */
class Route {
	
	var id : Int = 0
	var stages : List[RouteStage] = List()
	
	def print () {
		PrintsSerializer ! Print("Route ID = " + this.id)
		this.stages.foreach( s => 
			s.print
		)
		PrintsSerializer ! Print
	}
}
