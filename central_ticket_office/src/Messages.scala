import com.inspirel.yami._

case class Stop()
case class Error(message:String)

trait Request
trait AsynchRequest extends Request
trait SynchRequest extends Request

case class Resolve(startNode:String,from:String,to:String,traveler_index:String,requestTime:String) extends AsynchRequest


case class Validate(ticket : List[Ticket],requestTime:String) extends SynchRequest
case class UpdateRun(routeIndex:Int,current_run:Int) extends SynchRequest
case class GetTimeTable() extends SynchRequest

case class HandleSynchRequest(request:SynchRequest,incomingMessage:IncomingMessage) extends SynchRequest

case class Init(address:String)
case class CreationRequestResolved()

case class InitBookingManager()

