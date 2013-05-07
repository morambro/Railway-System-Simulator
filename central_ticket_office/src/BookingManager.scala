import scala.actors._
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone

/**
 * This object is responsible of serializing the validation requests.
 */
object BookingManager extends Actor {

	/**
	 * Default date formatter
	 */
	val dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
	// Set timezone to GMT, to print UTC-0 time 
	dateFormatter.setTimeZone(TimeZone.getTimeZone("GMT"))
	
	/**
	 * All FB Trains 
	 */
	var trainRouteMap : Map[Int,Train] = Route.loadTrainsRoutesMap("../../railway/res/trains.json")
	
	/**
	 * All the routes
	 */
	var routes : Array[Route] = Route.loadRoutes("../../railway/res/routes.json")
	
	
	
	/**
	 * This class represents a Timetable for a Route. It specifies the index of the route, the number of runs 
	 * covered by the time table and the span used to update the time table.
	 */ 
	class RouteTimeTable(val routeIndex:Int,val runs:Int,val span:Int) {
		
		/*
		 * The run index.
		 */
		var current_run 	: Int = 0
		
		/*
		 * It identifies uniquely the run. It will be incremented at each run update
		 */
		var current_run_id 	: Int = 0
		
		/**
		 * The timeTable
		 */
		var table : Array[Array[Date]] = new Array(runs)
		
		/*
		 * This Array contains the list of spans, useful to populate and update the timeTable
		 */
		var spans_table : Array[Array[Int]] = new Array(runs)
		
		/**
		 * Simple debug method to print the Time Table.
		 */
		def printDate {
			PrintsSerializer ! Print("Time Table for route : " + routeIndex )
			PrintsSerializer ! Print("Runs number : " + runs )
			PrintsSerializer ! Print("Current Run : " + current_run )
			table.foreach(list => {
				list.foreach(date => print(new StringBuilder(dateFormatter.format(date) + " , ")))
				PrintsSerializer ! Print("")
			})
		}
		
		/**
		 * Returns a JSON representation of the RouteTimeTable object.
		 */
		def toJSON : String = {
			var timeTable = "{" 
			timeTable += "\"route_index\" : " + (routeIndex+1) + ","
			//timeTable += "\"restart_span\" : " + span + ","
			timeTable += "\"current_run\" : " + (current_run+1) + ","
			timeTable += "\"current_run_id\" : " + (current_run_id+1) + ","
			timeTable += "\"time\" : ["
			var i : Int = 1
			for (i<- 0 until table.size) {
				timeTable += "["
				var j = 0
				table(i).foreach(date => {
					timeTable += "\"" + new StringBuilder(dateFormatter.format(date)) + "\""
					if (j < table(i).size - 1) timeTable += ","
					j += 1
				})
				timeTable += "]"
				if (i < table.size - 1) timeTable += ","
			
			}
			
			timeTable += "]}"
			timeTable
		}
		
	}
	
	/**
	 * The time table for each route.
	 */
	private var timeTables : Array[RouteTimeTable] = new Array(routes.size)
	
	// load the time table JSON file.
	val jsonTimeTable = scala.io.Source.fromFile("../../railway/res/time_table.json").mkString
	
	// reference time from which build the time table.
	val ref = new Date 
	
	val timeTable = JSON.parseJSON(jsonTimeTable).time_table
	
	// Loads time tables for each run
	for (k <- 0 until timeTable.size) {
		val tt = timeTable(k)
		val r = new RouteTimeTable(tt.route.toInt-1,tt.time.size,tt.restart_span.toInt)
		
		for (i <- 0 until tt.time.size) {
			
			r.spans_table(i) = new Array(tt.time(i).size)
			r.table(i) 		 = new Array(tt.time(i).size)
			
			for (j <- 0 until tt.time(i).size) {
				val time = tt.time(i)(j).toInt
				// Add an element to the current Time table
				r.table(i)(j) = new Date(ref.getTime+(time*1000))
				// Save also the span
				r.spans_table(i)(j) = (time*1000)
			}
			timeTables(k) = r
		}
		
	}
	
	/**
	 * An Array where for each route contains the number of free sits.
	 */
	var bookingSits : Map[Int,Array[Array[Int]]] = Map()

	// Initialization. Create an entry for each Train in trainRouteMap. This is done
	// because only booking info for FB Trains is needed.
	trainRouteMap.keys.foreach(trainId => {
		val t : Train = trainRouteMap(trainId)
		val timeTable = timeTables(t.routeIndex)
		var bookingSitsElem : Array[Array[Int]] = new Array(timeTable.table.size)
		for(i <- 0 until bookingSitsElem.size) {
			bookingSitsElem(i) = Array.fill(timeTable.table(i).size)(t.sitsNumber)
		}
		
		bookingSits += t.routeIndex -> bookingSitsElem 
	})
	
	
	// Debug print  
	def printBookingSits {
		bookingSits.keys.foreach(k => {
			bookingSits(k).foreach(array => {
				array.foreach(el => print(el+ " , "))
				PrintsSerializer ! Print("")
			})
		
		})
	}
	
	
	/**
	 * Returns a JSON string representation of the all the time tables
	 *
	 * @return the time table in JSON
	 **/
	def timeTablesToJSON : String = {
		var jsonTimeTables = "{"
		jsonTimeTables += "\"time_table\" : ["
		for (i <- 0 until timeTables.size) {
			jsonTimeTables += timeTables(i).toJSON
			if (i < timeTables.size-1) jsonTimeTables += ","
		}
		jsonTimeTables += "]}"
		jsonTimeTables
	}
	
	
	/** 
	 * Main loop for the Actor.
	 */
	def bookingLoop() {
		react {
			
			
			case GetTimeTable => {
				// Simply return the JSON representation of the Time Table.
				reply{timeTablesToJSON}
				bookingLoop
			}
			
			
			// Updates the current run index. It is done keeping always at least 2 route time tables,
			// so if the updates reguardes 
			case UpdateRun(routeIndex,current_run) => {
				// Update the RouteTimeTable element in position [routeIndex] with 
				// the given current_run value.
				// Check if the given routeIndex is a valid one
				if ((routeIndex-1) >= timeTables.size) reply(("error","Invalid routeIndex " + routeIndex))
				// Check if the current_run is a valid one
				else if ((current_run-1) >= timeTables(routeIndex-1).table.size) reply(("error","Invalid current_run " + current_run))
				else {		
					// All ok, update the run	
					timeTables(routeIndex-1).current_run = (current_run-1)
					// Increase the identificator of the run by one.
					timeTables(routeIndex-1).current_run_id += 1
					
					// If the current run is the last, update the entire 
					// table and give it back to the sender.
					if ((current_run-1) == timeTables(routeIndex-1).table.size-1) {
						
						val newTable : Array[Array[Date]]= new Array(timeTables(routeIndex-1).runs)
						// Put last two elements on the first two positions
						newTable(0) = timeTables(routeIndex-1).table(timeTables(routeIndex-1).table.size-1)
						//newTable(1) = timeTables(routeIndex-1).table(timeTables(routeIndex-1).table.size-1)
						
						bookingSits get (routeIndex-1) match {
							case Some(t) => t match {
								case table : Array[_] => {
									table(0) = table(table.size-1)
									//table(1) = table(table.size-1)
									for(j <- 1 until table.size)  {
										table(j) = Array.fill(table(0).size)(5)
									}
									
								}
								case _ => PrintsSerializer ! Print("ERROR!!")
							}
							case None => 
						}
						
						// Copy all other Time Tables, adding new span!
						for(i <- 0 until timeTables(routeIndex-1).runs-1) {
							newTable(i+1) = timeTables(routeIndex-1).table(i)
							for (j<- 0 until newTable(i+1).size) {
								newTable(i+1)(j) = new Date(newTable(0).last.getTime + timeTables(routeIndex-1).spans_table(i)(j)) 
							}
							
						}
						// Finally update the timeTable!
						timeTables(routeIndex-1).table = newTable
						// Set current_run to 0 .
						timeTables(routeIndex-1).current_run = 0
						
						PrintsSerializer ! Print("New Time Table for route " + (routeIndex-1) + ":\n" + timeTables(routeIndex-1).toJSON)
						
						PrintsSerializer ! Print("Current run = " + timeTables(routeIndex-1).current_run + 
								", run id = " + timeTables(routeIndex-1).current_run_id)
						
						PrintsSerializer ! Print("booking table :")
						bookingSits get (routeIndex-1) match {
							case Some(t) => t match {
								case table : Array[_] => {
									table.foreach ( el => {
										el.foreach(i => print(" | "+i))
										PrintsSerializer ! Print("")
									})
								}
							}
							case None => 
						}		
												
						// Give it back to Sender
						reply(("new_time_table",timeTables(routeIndex-1).toJSON))
						 
						
					}else{
						PrintsSerializer ! Print("Current Run for route " + (routeIndex-1) + " updated : " + (current_run-1))	
						reply(("updated",timeTables(routeIndex-1).current_run_id+1))
					}
				}
				bookingLoop
			}
			
			// Validates a list of ticket. They have to be for a unique Region, otherwise
			// it would not be able to validate.
			case Validate(ticketList,requestTime) => {
				
				// Create Date from String
				val requestTimeDate = dateFormatter.parse(requestTime)
				
				// Map used to keep track of the sits to edit once the sit is booked.
				var bookingSitsToUpdate : List[Map[Int,(Int,Int,Int)]] = List()
				
				var valid : Boolean = true
				
				var routeMap : Map[Int,(Int,Int,Int,Int)] = Map()
				
				PrintsSerializer ! Print(ticketList.size+"")
				
				for (i <- 0 until ticketList.size;if (valid)) {
					
					var ticket = ticketList(i)
					
					PrintsSerializer ! Print(ticket.stages.size+"")
					
					//var routeMap : Map[Int,(Int,Int,Int)] = Map()
					
					// Run over all the stages
					for (j <- 0 until ticket.stages.size;if (valid)) {
					
						var ticketStage = ticket.stages(j)
						
						trainRouteMap get (ticketStage.trainId) match {
							// Only if the train is a FB Train the ticket must be validated.
							case Some(map) => {
								// Retrieve the route index
								val routeIndex = map.routeIndex
							
								var firstIndex 	= -1
								var secondIndex = -1		
								PrintsSerializer ! Print("\nCurren index = " + routeIndex)
								PrintsSerializer ! Print("startStation = " + ticketStage.startStation)
								PrintsSerializer ! Print("nextStation  = " + ticketStage.nextStation)
					
								// check in first half
								for (i <- 0 until routes(routeIndex).stages.size/2) {
									PrintsSerializer ! Print("Start Station = " + routes(routeIndex).stages(i).startStation)
									PrintsSerializer ! Print("Ticket Start = " + ticketStage.startStation)
									PrintsSerializer ! Print("NN = " +routes(routeIndex).stages(i).nodeName)
									PrintsSerializer ! Print("Next = " + ticketStage.nextRegion)
									if (routes(routeIndex).stages(i).startStation 	== ticketStage.startStation && 
										routes(routeIndex).stages(i).nodeName 		== ticketStage.nextRegion) {
										firstIndex = i 
									}
									PrintsSerializer ! Print("Next Station = " + routes(routeIndex).stages(i).nextStation)
									PrintsSerializer ! Print("Ticket Next = " + ticketStage.nextStation+"\n")
									if (routes(routeIndex).stages(i).nextStation 	== ticketStage.nextStation &&
										routes(routeIndex).stages(i).nodeName 		== ticketStage.nextRegion) {
										secondIndex = i
									}
									
								}
								
								if (firstIndex > secondIndex || firstIndex == -1 || secondIndex == -1) {
									// If we haven't a match in the first half, search in the second half.
									PrintsSerializer ! Print("")
									PrintsSerializer ! Print("SECOND HALF")
									PrintsSerializer ! Print("")
									
									
									for (i <- routes(routeIndex).stages.size/2 until routes(routeIndex).stages.size) {
										
										PrintsSerializer ! Print("Start Station = " + routes(routeIndex).stages(i).startStation)
										PrintsSerializer ! Print("Ticket Start = " + ticketStage.startStation)
										PrintsSerializer ! Print("NN = " +routes(routeIndex).stages(i).nodeName)
										PrintsSerializer ! Print("Next = " + ticketStage.nextRegion)	
											
										if (routes(routeIndex).stages(i).startStation	== ticketStage.startStation&& 
											routes(routeIndex).stages(i).nodeName 		== ticketStage.nextRegion) {
											firstIndex = i 
										}
										PrintsSerializer ! Print("Next Station = " + routes(routeIndex).stages(i).nextStation)
										PrintsSerializer ! Print("Ticket Next = " + ticketStage.nextStation)
										if (routes(routeIndex).stages(i).nextStation 	== ticketStage.nextStation&& 
											routes(routeIndex).stages(i).nodeName 		== ticketStage.nextRegion) {
											secondIndex = i
										}
									}
								}
								
								PrintsSerializer ! Print(firstIndex+"")
								PrintsSerializer ! Print(secondIndex+"")
								
								// decide weather to search in the current run or in the next one
								val (selected_run,selected_run_id) = {
									val current_run = timeTables(routeIndex).current_run
									// If the time at witch the train leaves in the current run is after the
									// time at witch the request was made, consider the current run, otherwise the next
									PrintsSerializer ! Print("after ? " + (timeTables(routeIndex).table(current_run)(firstIndex).after(requestTimeDate)))
									
									if (routeMap.contains(routeIndex))
										(routeMap(routeIndex)._3,routeMap(routeIndex)._4)
									else if (timeTables(routeIndex).table(current_run)(firstIndex).after(requestTimeDate)) 
										(timeTables(routeIndex).current_run,timeTables(routeIndex).current_run_id)
									else
										(timeTables(routeIndex).current_run + 1,timeTables(routeIndex).current_run_id + 1)
								}
								
								PrintsSerializer ! Print("selected_run = " + selected_run)
								PrintsSerializer ! Print("selected_run_id = " + selected_run_id)
								
								// At this point firstIndex will be the first index of the route,
								// secondIndex the last. 
								// Check if there are enougth sits to assign to the Traveler
								for (i <- firstIndex to secondIndex; if (valid)) {
									valid = valid && bookingSits(routeIndex)(selected_run)(i) > 0
								}
								
								// Add to routeMap, to be updated if the ticket will result validated
								if (valid) {
									routeMap get (routeIndex) match { 
										// In case no entry for routeIndex was already in the map, add new
										case None => routeMap += routeIndex -> (firstIndex,secondIndex,selected_run,selected_run_id)
										// Simply updates it
										case Some(el) => routeMap += routeIndex -> (el._1,secondIndex,el._3,el._4)
									}
									// memorize the run id on the ticket
									ticketStage.run_number = (selected_run_id + 1)
								}
							}
							case None => PrintsSerializer ! Print("Train " + ticketStage.trainId + " not a FB train ")
						}
					}
				}
				
				
				if (!valid) {
					// Send negative response in case the ticket could not be booked
					reply(false)
				}else{
					// If we arrived here we have all sits needed. We can perform the update
					// Finally, send the reply	
					routeMap.keys.foreach( routeIndex => {
						val firstIndex 		= routeMap(routeIndex)._1
						val secondIndex 	= routeMap(routeIndex)._2
						val selected_run 	= routeMap(routeIndex)._3
						for (i <- firstIndex to secondIndex) {
							bookingSits(routeIndex)(selected_run)(i) -= 1
						}
						PrintsSerializer ! Print(routeIndex +","+firstIndex+ ","+secondIndex)
					})
					reply ((true,ticketList))
				}
				
				// debug print
				printBookingSits
				
				bookingLoop
			}
			
			
			case Stop() => {
				println("BookingManager shutted down")
			}
		}
	}
	
	def act = bookingLoop
}

