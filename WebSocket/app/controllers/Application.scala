package controllers

import play.api._
import play.api.mvc._

import models._

import play.api.libs.iteratee._

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._

case class User(chatID:String)

object Application extends Controller {
	
	var ids = 0;
	
	def index = Action {
		implicit request => {
			ids+=1
			Ok(views.html.wb("WebSocket",ids+""))	
		}
	}

	
	def chat(chatID:String) = WebSocket.async[String] { request  =>
		ChatHandler.join(chatID)
	}
  
}
