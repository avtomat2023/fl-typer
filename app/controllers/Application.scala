package controllers

import play.api._
import play.api.mvc._

import play.api.data.{Form, Forms}

import parsing.FLParser

class Application extends Controller {
  val form = Form("expression" -> Forms.text)

  def index = Action { implicit request =>
    Ok(views.html.main(request))
  }

  def typing = Action { implicit req =>
    val expr = form.bindFromRequest.get
    val result = FLParser.parse(expr)
    if (result.successful)
      Ok(result.get.toJsObject.toString)
    else
      Ok(expr + "\nparse failed:\n" + result)
  }
}
