import scala.actors._

case class Validate(ticket : Ticket);
/**
 * This object is responsible of serializing the validation requests.
 */
object BookingManager extends Actor {
	
	/**
	 * All FB Trains 
	 */
	var trainRouteMap : Map[Int,Train] = Route.loadTrainsRoutesMap("../../railway/res/trains.json")
	
	/**
	 * All the routes
	 */
	var routes : Array[Route] = Route.loadRoutes("../../railway/res/routes.json")
	
	/**
	 * An Array where for each route contains the number of free sits.
	 */
	var bookingSits : Array[List[Int]] = Array.fill(routes.size)(List())

	// bookingSits initialization
	trainRouteMap.keys.foreach( trainID => {
		val routeIndex = trainRouteMap(trainID).routeIndex 
		val size = routes(routeIndex).stages.size
		for (i <- 0 until size) 
			bookingSits(routeIndex) = bookingSits(routeIndex) :+ trainRouteMap(trainID).sitsNumber
	})
	
	var timeTables : Array[List[List[]]]
	
	/** 
	 * Main loop for the Actor.
	 */
	def bookingLoop() {
		react {
		
			// Handles a Validate request.
			case Validate(ticket) => {
				println ("Validating ticket: ")
				
				var valid : Boolean = true
				
				ticket.stages.foreach ( ticketStage => {
					
					var firstIndex 	= -1
					var secondIndex = -1
					
					// Retrieve the route index
					var routeIndex = trainRouteMap(ticketStage.trainId).routeIndex 
					
					println("\n Curren index = " + routeIndex)
					println("startStation = " + ticketStage.startStation)
					println("nextStation = " + ticketStage.nextStation)
					
					// match in first half
					
					for (i <- 0 until routes(routeIndex).stages.size/2) {
						if (routes(routeIndex).stages(i).startStation == ticketStage.startStation) {
							firstIndex = i 
						}
						if (routes(routeIndex).stages(i).nextStation == ticketStage.nextStation) {
							secondIndex = i
						}
					}
					if (firstIndex > secondIndex || firstIndex == -1 || secondIndex == -1) {
						// If we haven't a match in the first half, search in the second half.
						for (i <- routes(routeIndex).stages.size/2 until routes(routeIndex).stages.size) {
							if (routes(routeIndex).stages(i).startStation == ticketStage.startStation) {
								firstIndex = i 
							}
							if (routes(routeIndex).stages(i).nextStation == ticketStage.nextStation) {
								secondIndex = i
							}
						}
					}
					// At this point firstIndex will be the first index of the route,
					// secondIndex the last
					for (i <- firstIndex to secondIndex) {
						valid = bookingSits(routeIndex)(i) > 0
					}
									
				})
				
				// Send the response back
				if (valid) sender ! true
				else sender ! false
				
				bookingLoop
			}
			case Stop => {
				println("Validator shutted down")
			}
		}
	}
	
	def act = bookingLoop
}
