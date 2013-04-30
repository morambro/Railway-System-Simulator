package models

import akka.actor._
import scala.concurrent.duration._

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._

import akka.util.Timeout
import akka.pattern.ask

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

import com.inspirel.yami._

case class Init()
case class Join(username: String)
case class Quit(username: String)
case class Talk(username: String, text: String)
case class NotifyJoin(username: String)

case class Connected(enumerator:Enumerator[String])
case class CannotConnect(msg: String)


class Subscriber(ID : Int, default : ActorRef) extends Actor with IncomingMessageCallback{
	
	val subscriberAgent = new Agent


	def receive = {
		case Init() => {
			println ("init!")
			subscriberAgent.registerObject("update_handler", this);
			val params : Parameters = new Parameters
			params.setString("destination_object", "update_handler");
			subscriberAgent.sendOneWay("tcp://localhost:2222","events", "subscribe", params);
		}
		case _ => println("Invalid parameter")
	}
	
	def call (incomingMessage : IncomingMessage) {
		try {
			val content = incomingMessage.getParameters()

		    val event = content.getString("event")
		    
		    default ! Talk("Central Controller",event)
		    
		    println("Subscriber " + ID + " Received event : " + event)
		}catch{
			case e : Exception => e.printStackTrace 
		}
		
	}
	
}


object ChatHandler {
	
	// Implicit Timeout for the request
	implicit val timeout = Timeout(1 second)
	
	// Create the Actor responsible of keep track of all the incoming 
	// connections and to push notifications via message Talk(...)
	lazy val default = Akka.system.actorOf(Props[ChatHandler])
	
  	// Creation and initialization of the Actor used to listen for incoming 
  	// events from the central controller
  	val subscriber = {
  		val a = Akka.system.actorOf(Props(new Subscriber(1,default)))
  		a ! Init()
  		println("Actor created")
  		a
  	}
	
	
	def join(username:String):scala.concurrent.Future[(Iteratee[String,_],Enumerator[String])] = {
		
		(default ? Join(username)).map {
      
			case Connected(enumerator) => 
			  
				// Create an Iteratee to consume the feed
				val iteratee = Iteratee.foreach[String] { event =>
					default ! Talk(username, event)
				}.mapDone { _ =>
					default ! Quit(username)
				}
				// Returns the iteratee and the enumerator to the client
				(iteratee,enumerator)
			
			case CannotConnect(error) => 
		  
				// Connection error

				// A finished Iteratee sending EOF
				val iteratee = Done[String,Unit]((),Input.EOF)

				// Send an error and close the socket
				val enumerator =  Enumerator[String](error)
				.andThen(Enumerator.enumInput(Input.EOF))
				
				(iteratee,enumerator)
    	}
		
	}
}

class ChatHandler extends Actor {
	
	var members = Set.empty[String]
	
	// Creates an Enumerator and a Broadcast channel to push data to all 
	// incoming channels
	val (enumerator, channel) = Concurrent.broadcast[String]
	
	def receive = {
    
    	// Keeps track of the new incoming connection
		case Join(username) => {
			if(members.contains(username)) {
		 	 	sender ! CannotConnect("This username is already used")
		  	} else {
				members = members + username
				sender ! Connected(enumerator)
				self ! NotifyJoin(username)
		  	}
		}

		case NotifyJoin(username) => {
			//notifyAll("join", username, "has entered the room")
		}
		
		case Talk(username, text) => {
		  	notifyAll("talk", username, text)
		}
		
		case Quit(username) => {
		  	members = members - username
		  	notifyAll("quit", username, "has left the room")
		}
    
  	}
  	
  	def notifyAll(kind: String, user: String, text: String) {
		channel.push(text)
  	}
}
