import net.minidev.json._

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
}

object Route {
	
	/**
	 * Loads the Routes from configuration file [fileName]
	 * in the list [routes].
	 */ 
	def loadRoutes(fileName : String) : Array[Route] = {
		
		val jsonRoutes = scala.io.Source.fromFile(fileName).mkString
		
		var routesList : List[Route] = List()
		
		JSONValue.parseStrict(jsonRoutes) match {
			case o : JSONObject => o.get("routes") match {
				case allRoutes : JSONArray => {
					// for each Route found
					for (i <- 0 until allRoutes.size) {
						allRoutes.get(i) match {
							case r : JSONObject => {
								var route = new Route
								// Set the id field
								route.id = r.get("id") match {
									case id : java.lang.Integer => id
								}
								// set the stages filed
								r.get("route") match {
									case stagesArray : JSONArray => {
										for ( j <- 0 until stagesArray.size) {
											stagesArray.get(j) match {
												case s : JSONObject => {
													var stage = new RouteStage
													stage.startStation = s.get("start_station") match {
														case ss : java.lang.Integer => ss.intValue
													}									
													stage.nextStation = s.get("next_station") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.startPlatform = s.get("start_platform") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.nextPlatform = s.get("platform_index") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.nextSegment = s.get("next_segment") match {
														case ss : java.lang.Integer => ss.intValue
													}
													stage.nodeName = s.get("node_name") match {
														case ss : String => ss
													}
													stage.leaveAction = s.get("leave_action") match {
														case ss : String => ss
													}
													stage.enterAction = s.get("enter_action") match {
														case ss : String => ss
													}
													// Now add the created stage to the current route
													route.stages = route.stages :+ stage
												}
											}
										}
									}
								}
								
								// Add the route to routes List
								routesList = routesList :+ route
								
							}
						}
					}
				}
			}
		}
		
		
		routesList.toArray
	}
	
	/** 
	 * Loads from file a Map with FB Trains information
	 */
	def loadTrainsRoutesMap(fileName : String) : Map[Int,Train] = {
		
		// Now load for each train its route.
		val jsonTrain = scala.io.Source.fromFile(fileName).mkString
		
		var trainRouteMap : Map[Int,Train] = Map()
		
		JSONValue.parseStrict(jsonTrain) match {
			case o : JSONObject => o.get("trains") match {
				case trains : JSONArray => {
					for (i <- 0 until trains.size) {
						trains.get(i) match {
							case train : JSONObject => {
								train.get("type") match {
									case "fb" => {
										val id = train.get("id") match {
											case i : java.lang.Integer => i.intValue 
										}
										val routeIndex = train.get("route_index") match {
											case i : java.lang.Integer => (i.intValue - 1)
										}
										val sitsNumber = train.get("sits_number") match {
											case i : java.lang.Integer => i.intValue
										}
										trainRouteMap = trainRouteMap + Tuple2(id,new Train(id,routeIndex,sitsNumber))
									}
									case _ => // DO NOTHING
								}
							}
						}
					}
				}
			}
		}
		// trainRouteMap.keys.foreach( k => {println("trainID = " + k + " , route = " + trainRouteMap(k))})
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
		println("ID = " + this.id)
		this.stages.foreach( s => 
			println("start startStation = " + s.startStation)
		)
	}
}
