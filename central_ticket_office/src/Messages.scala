import com.inspirel.yami._

case class Stop()
case class Error(message:String)

trait AsynchRequest
case class Resolve(startNode:String,from:String,to:String,traveler_index:String,requestTime:String) extends AsynchRequest

trait SynchRequest
case class Validate(ticket : List[Ticket],requestTime:String) extends SynchRequest
case class UpdateRun(routeIndex:Int,current_run:Int) extends SynchRequest
case class GetTimeTable() extends SynchRequest

case class HandleSynchRequest(request:SynchRequest,incomingMessage:IncomingMessage)

case class Init(address:String)
case class CreationRequestResolved()

case class InitBookingManager()

