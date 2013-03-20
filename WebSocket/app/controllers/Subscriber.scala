package controllers

import akka.actor._

import com.inspirel.yami._

case class Init

class Subscriber(ID : Int) extends Actor with IncomingMessageCallback{
	
	val subscriberAgent = new Agent


	def receive = {
		case Init => {
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
		    
		    println("Subscriber " + ID + " Received event : " + event)
		}catch{
			case e => e.printStackTrace 
		}
	}
	
}
