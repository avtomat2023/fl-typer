package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import play.api.data.{Form, Forms}

import parsing.FLParser

class Application extends Controller {
  val form = Form("expression" -> Forms.text)

  def index = Action {
    Ok(views.html.main())
  }

  def typing = Action { implicit request =>
    FLParser.parse(form.bindFromRequest.get) match {
      case FLParser.Success(result, _) => Ok(JsObject(Seq(
        "parsed" -> JsBoolean(true),
        "ast" -> result.jsDrawable
      )))
      case FLParser.Failure(msg, _) => Ok(JsObject(Seq(
        "parsed" -> JsBoolean(false),
        "error" -> JsString(msg)
      )))
      case FLParser.Error(msg, _) => Ok(JsObject(Seq(
        "parsed" -> JsBoolean(false),
        "error" -> JsString(msg)
      )))
    }
  }
}
