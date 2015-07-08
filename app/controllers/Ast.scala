package ast

import play.api.libs.json._

sealed abstract class Ast {
  def jsDrawable: JsObject
  protected def jsDrawableText(text: String, italic: Boolean = false): JsObject =
    JsObject(Seq(
      "kind" -> JsString("text"),
      "text" -> JsString(text)
    ))
  protected def jsDrawableTree(node: String, children: Ast*): JsObject =
    jsDrawableTreeFromJs(node, children.map(_.jsDrawable): _*)
  protected def jsDrawableTreeFromJs(node: String,
                                     children: JsObject*): JsObject =
    JsObject(Seq(
      "kind" -> JsString("tree"),
      "node" -> jsDrawableText(node),
      "children" -> JsArray(children)
    ))
}

case class Const(value: String) extends Ast {
  def jsDrawable = jsDrawableText(value)
}
case class Var(name: String) extends Ast {
  def jsDrawable = jsDrawableText(name)
}
case object Nil extends Ast {
  def jsDrawable = jsDrawableText("nil")
}
case class Cons(car: Ast, cdr: Ast) extends Ast {
  def jsDrawable = jsDrawableTree("::", car, cdr)
}
case class Abs(variable: Var, body: Ast) extends Ast {
  def jsDrawable = jsDrawableTree("λ", variable, body)
}
case class App(func: Ast, arg: Ast) extends Ast {
  def jsDrawable = jsDrawableTree("app", func, arg)
}
case class If(cond: Ast, thenExpr: Ast, elseExpr: Ast) extends Ast {
  def jsDrawable = jsDrawableTree("if", cond, thenExpr, elseExpr)
}
case class Let(variable: Var, binding: Ast, body: Ast) extends Ast {
  def jsDrawable = jsDrawableTree("let", variable, binding, body)
}
case class Case(caseExpr: Ast, nilExpr: Ast,
                carPat: Var, cdrPat: Var, consExpr: Ast) extends Ast {
  def jsDrawable = {
    val nilAlt = jsDrawableTree("nil", nilExpr)
    val condAlt = jsDrawableTree(carPat.name + "::" + cdrPat.name, consExpr)
    jsDrawableTreeFromJs("case", caseExpr.jsDrawable, nilAlt, condAlt)
  }
}

object Const {
  val add = Const("+")
  val sub = Const("-")
  val mul = Const("*")
  val div = Const("/")
  val uminus = Const("-")
  val uplus = Const("+")
  val and = Const("and")
  val or = Const("or")
  val not = Const("not")
  val lt = Const("<")
  val le = Const("<=")
  val intEq = Const("=")
  val ge = Const(">=")
  val gt = Const(">")
}

object Implicits {
  import scala.language.implicitConversions
  import scala.collection.immutable

  implicit def intToConst(x: Int): Const = Const(x.toString)
  implicit def boolToConst(x: Boolean): Const = Const(x.toString)
  implicit def strToVar(x: String): Var = Var(x)
  implicit def listToAst(x: List[Ast]): Ast = x match {
    case immutable.Nil => Nil
    case hd::tl => Cons(hd, listToAst(tl))
  }
  implicit def consToCons(x: immutable.::[Ast]): Cons =
    Cons(x.head, listToAst(x.tail))
  implicit def nilToNil(x: immutable.Nil.type): Nil.type = Nil
}
