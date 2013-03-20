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

	def index = Action {
		
		val form = Form(
			mapping(
				"chatID" -> text
			)(User.apply)(User.unapply)
		)
		
		Ok(views.html.index("Your new application is ready.",form))
	}
	
	def chatPage(chatID : Option[String]) = Action {
		implicit request =>
			chatID.filterNot(_.isEmpty).map {
				chatID => Ok(views.html.wb("WebSocket",request.id+""))
			}.getOrElse {
		  		Redirect(routes.Application.index)
		  	}
	}
	
	def chat(chatID:String) = WebSocket.async[String] { request  =>
		ChatHandler.join(chatID)
	}
  
}
