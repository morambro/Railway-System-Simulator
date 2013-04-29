import com.inspirel.yami._

case class Validate(ticket : List[Ticket],requestTime:String)
case class UpdateRun(trainID:Int,routeIndex:Int,current_run:Int)
case class GetTimeTable()

case class Stop()
case class Resolve(startNode:String,from:String,to:String,traveler_index:String,requestTime:String)
case class Error(message:String)

case class HandleValidation(ticket:Ticket,requestTime:String,im:IncomingMessage)
