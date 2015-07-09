package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import parsing.FLParser

class Application extends Controller {
  def index = Action {
    Ok(views.html.main())
  }

  def typing(expr: String) = Action { implicit request =>
    def noSuccess(msg: String, next: FLParser.Input) =
      Ok(JsObject(Seq(
        "parsed" -> JsBoolean(false),
        "error" -> JsString("character " + next.pos.column + ": " + msg + "\n" +
                            next.pos.longString)
      )))

    FLParser.parse(expr) match {
      case FLParser.Success(ast, _) => Ok(JsObject(Seq(
        "parsed" -> JsBoolean(true),
        "ast" -> ast.toJsTree
      )))
      case FLParser.Failure(msg, next) => noSuccess(msg, next)
      case FLParser.Error(msg, next) => noSuccess(msg, next)
    }
  }
}
