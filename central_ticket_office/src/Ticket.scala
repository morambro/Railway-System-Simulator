import net.minidev.json._

class TicketStage (
	val startStation : Int,
	val nextStation : Int,
	val trainId : Int,
	val startPlatform  : Int, 
	val destinationPlatform  : Int,
	var run_number 	: Int = 0,
	val	nextRegion : String) {

	def print {
		println ("start station  : " + startStation)
		println ("next station   : " + nextStation)
		println ("train id       : " + trainId)
		println ("start platform : " + startPlatform)
		println ("dest platform  : " + destinationPlatform)
		println ("next region    : " + nextRegion)
		println ("run number     : " + run_number)
	}
}


object Ticket {
	
	/**
	 * Given two tickets, this function returns a new ticket merging them.
	 * 
	 * @return the new Ticket object
	 */
	def mergeTickets (T1:Ticket,T2:Ticket) : Ticket =  {
		val ticket = new Ticket(T1.next)
		if (
			// Destination platform and start platform are the same
			(T1.stages.last.destinationPlatform == T2.stages(0).startPlatform) && 
			// The same TrainID
			(T1.stages.last.trainId == T2.stages(0).trainId) &&
			// They have the same run number, otherwise they will be two separate stages
			(T1.stages.last.run_number == T2.stages(0).run_number)
			) {
			ticket.stages = T1.stages.dropRight(1) ++ ( new TicketStage(
				T1.stages.last.startStation,
				T2.stages(0).nextStation,
				T1.stages.last.trainId,
				T1.stages.last.startPlatform,
				T2.stages(0).destinationPlatform,
				T1.stages.last.run_number,
				T2.stages(0).nextRegion) :: T2.stages.tail
			)
		} else {
			ticket.stages = T1.stages ++ T2.stages
		}
		ticket
	}
	
	/**
	 * Performs implicit conversion from String to Ticket.
	 */
	implicit def stringToTicket(json : String) : Ticket = {
		var ticket : Ticket = null
		JSONValue.parseStrict(json) match {
			case o : JSONObject => {
				o.get("next") match {
					case n : java.lang.Integer => {
						ticket = new Ticket(n)
					}
				}
				if (ticket != null) {
					o.get("ticket") match {
						case stages : JSONArray => {
							for (i <- 0 until stages.size) {
								stages.get(i) match {
									case stage : JSONObject => {
										val startStation : Int = stage.get("start_station") match {
											case s : java.lang.Integer => s
										}
										val nextStation : Int = stage.get("next_station") match {
											case s : java.lang.Integer => s
										}
										val trainId : Int = stage.get("train_id") match {
											case s : java.lang.Integer => s
										}
										val startPlatform : Int = stage.get("start_platform_index") match {
											case s : java.lang.Integer => s
										}
										val destinationPlatform : Int = 
											stage.get("destination_platform_index") match {
											case s : java.lang.Integer => s
										}
										val nextRegion : String = 
											stage.get("region") match {
											case s : String => s
										}
										ticket.stages = ticket.stages :+ new TicketStage(
											startStation,
											nextStation ,
											trainId,
											startPlatform, 
											destinationPlatform,
											0,
											nextRegion)
									}
								} 
							}
						}
					}
				}
			}
		}
		ticket
	}
	
	implicit def ticket2Json(T:Ticket) : String = {
		var json : String = ""
		
		json += "{"
			
		json += """ "next" : """ + T.next + ","
		
		json += """ "ticket" : ["""
		
		var i = 0
		T.stages.foreach ( stage => {
			json += "{"
			
			json += """ "start_station" : """ + stage.startStation + ","
			json += """ "next_station" : """ + stage.nextStation + ","
			json += """ "train_id" : """ + stage.trainId + ","
			json += """ "start_platform_index" : """ + stage.startPlatform + ","
			json += """ "destination_platform_index" : """ + stage.destinationPlatform + ","
			json += """ "current_run" : """ + stage.run_number + ","
			json += """ "region" : """ + "\"" + stage.nextRegion + "\""
			
			json += "}"
			if (i < T.stages.length - 1) json += ","
			i += 1
		})
		
		json += "]"
		json += "}"
		
		json
	}
}

class Ticket (n : Int) {
	
	val next : Int = n
	var stages : List[TicketStage] = List()
	
	def print {
		println ("Ticket")
		println ("next = "+next)
		stages.foreach( stage =>
			stage.print
		)
	}
}

