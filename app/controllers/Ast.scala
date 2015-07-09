package ast

import play.api.libs.json.JsObject
import visual._

sealed abstract class Ast {
  def toJsTree: JsObject
}

object Ast {
  // utility function
  def jsTree(node: String, children: Ast*) =
    Visual.jsTree(node, children.map(_.toJsTree): _*)
}

case class Const(value: String) extends Ast {
  def toJsTree = Visual.jsText(value)
}
case class Var(name: String) extends Ast {
  def toJsTree = Visual.jsText(name, Italic())
}
case object Nil extends Ast {
  def toJsTree = Visual.jsText("nil")
}
case class Cons(car: Ast, cdr: Ast) extends Ast {
  def toJsTree = Ast.jsTree("::", car, cdr)
}
case class Abs(variable: Var, body: Ast) extends Ast {
  def toJsTree = Ast.jsTree("λ", variable, body)
}
case class App(func: Ast, arg: Ast) extends Ast {
  def toJsTree = Ast.jsTree("app", func, arg)
}
case class If(cond: Ast, thenExpr: Ast, elseExpr: Ast) extends Ast {
  def toJsTree = Ast.jsTree("if", cond, thenExpr, elseExpr)
}
case class Let(variable: Var, binding: Ast, body: Ast) extends Ast {
  def toJsTree = Ast.jsTree("let", variable, binding, body)
}
case class Case(caseExpr: Ast, nilExpr: Ast,
                carPat: Var, cdrPat: Var, consExpr: Ast) extends Ast {
  def toJsTree = {
    val nilAlt = Ast.jsTree("nil", nilExpr)
    val condAlt = Ast.jsTree(carPat.name + "::" + cdrPat.name, consExpr)
    Visual.jsTree("case", caseExpr.toJsTree, nilAlt, condAlt)
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
