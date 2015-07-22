package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

import parsing.FLParser
import proofdiagram.ProofDiagram

/** Controller class for Play Framework. */
class Application extends Controller {
  /** Returns an HTML responce of index HTML. */
  def index = Action {
    Ok(views.html.main())
  }

  /** Respond to typing requests.
    *
    * Clients request typing by giving expressions as Strings.
    *
    * The responce is a JSON which contains the following field:
    *   - "parsed": Boolean<br>
    *     represents if parsing succeeded.
    *
    * it also contains the following fields when the value of "parsed" is true:
    *   - "expr": Object (see [[visual.Richtext]])<br>
    *     represents the requested expression.
    *   - "ast": Object (see [[visual.Ast]])<br>
    *     represents the abstract syntax tree of the requested expression.
    *
    * Otherwise, it also contains the following field:
    *   - "error": String<br>
    *     represents an error message generated by the parser.
    */
  def typing(expr: String) = Action { implicit request =>
    FLParser.parse(expr) match {
      case FLParser.Success(ast, _) => {
        val diag = ProofDiagram.make(ast)
        val unification = diag.unification
        val unificationVisual = unification.toVisual
        Ok(JsObject(Seq(
          "parsed" -> JsBoolean(true),
          "expr" -> ast.toVisualExpr.toJSON,
          "type" -> unification.solve().toVisual.toJSON,
          "ast" -> ast.toVisualAst.toJSON,
          "proof" -> diag.toVisual.toJSON,
          "unification" -> unificationVisual.toJSON
        )))
      }
      case FLParser.NoSuccess(msg, next) => Ok(JsObject(Seq(
        "parsed" -> JsBoolean(false),
        "error" -> JsString("character " + next.pos.column + ": " + msg + "\n" +
                            next.pos.longString)
      )))
    }
  }
}
