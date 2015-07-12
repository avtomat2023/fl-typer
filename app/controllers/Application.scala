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
    FLParser.parse(expr) match {
      case FLParser.Success(ast, _) => Ok(JsObject(Seq(
        "parsed" -> JsBoolean(true),
        "ast" -> ast.toVisualAst.toJsObject,
        "expr" -> ast.toVisualExpr.toJsObject
      )))
      case FLParser.NoSuccess(msg, next) => Ok(JsObject(Seq(
        "parsed" -> JsBoolean(false),
        "error" -> JsString("character " + next.pos.column + ": " + msg + "\n" +
                            next.pos.longString)
      )))
    }
  }
}
