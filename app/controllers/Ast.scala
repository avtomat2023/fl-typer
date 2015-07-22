package ast

import play.api.libs.json.JsObject

import fltype._

sealed trait Ast {
  def toVisualAst: visual.Ast
  def toVisualExpr: visual.Richtext
  def parenedVisualExpr =
    visual.Richtext("(") + toVisualExpr + visual.Richtext(")")
  // 式を具象構文で書いたとき、右端がAbsかIfかLetかCaseの終わりならtrueを返す
  def isRightOpen: Boolean
}

object Ast {
  // utility function
  def visualAst(node: String, children: Ast*) =
    visual.Ast(node, children.map(_.toVisualAst): _*)
}

case class Const(value: String, fltype: FLType) extends Ast {
  def toVisualAst = visual.Ast(value)
  def toVisualExpr = visual.Richtext(value)
  def isRightOpen = false
}

case class Var(name: String) extends Ast {
  def toVisualAst = visual.Ast(visual.Richtext(name, visual.Italic))
  def toVisualExpr = visual.Richtext(name, visual.Italic)
  def isRightOpen = false
}

case object Nil extends Ast {
  def toVisualAst = visual.Ast("nil")
  def toVisualExpr = visual.Richtext("nil")
  def isRightOpen = false
}

case class Cons(car: Ast, cdr: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("::", car, cdr)
  def toVisualExpr = {
    val carExpr =
      if (car.isRightOpen) car.parenedVisualExpr
      else car match {
        case Cons(_,_) => car.parenedVisualExpr
        case _ => car.toVisualExpr
      }
    carExpr + visual.Richtext(" :: ") + cdr.toVisualExpr
  }
  def isRightOpen = cdr.isRightOpen
}

case class Abs(variable: Var, body: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("λ", variable, body)
  def toVisualExpr =
    visual.Richtext("λ") + variable.toVisualExpr +
    visual.Richtext(". ") + body.toVisualExpr
  def isRightOpen = true
}

case class App(func: Ast, arg: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("app", func, arg)
  def toVisualExpr = {
    val funcExpr =
      if (func.isRightOpen) func.parenedVisualExpr
      else func match {
        case Cons(_,_) => func.parenedVisualExpr
        case _ => func.toVisualExpr
      }
    val argExpr = arg match {
      case Cons(_,_) => arg.parenedVisualExpr
      case App(_,_) => arg.parenedVisualExpr
      case _ => arg.toVisualExpr
    }
    funcExpr + visual.Richtext(" ") + argExpr
  }
  def isRightOpen = arg.isRightOpen
}

case class If(cond: Ast, thenExpr: Ast, elseExpr: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("if", cond, thenExpr, elseExpr)
  def toVisualExpr =
    visual.Richtext("if ") + cond.toVisualExpr +
      visual.Richtext(" then ") + thenExpr.toVisualExpr +
      visual.Richtext(" else ") + elseExpr.toVisualExpr
  def isRightOpen = true
}

case class Let(variable: Var, binding: Ast, body: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("let", variable, binding, body)
  def toVisualExpr =
    visual.Richtext("let ") + variable.toVisualExpr +
      visual.Richtext(" = ") + binding.toVisualExpr +
      visual.Richtext(" in ") + body.toVisualExpr
  def isRightOpen = true
}

case class Case(selector: Ast, nilExpr: Ast,
                carPat: Var, cdrPat: Var, consExpr: Ast) extends Ast {
  def toVisualAst = {
    val nilAlt = Ast.visualAst("nil", nilExpr)
    val consPat = carPat.toVisualExpr + visual.Richtext("::") + cdrPat.toVisualExpr
    val consAlt = visual.Ast(consPat, consExpr.toVisualAst)
    visual.Ast("case", selector.toVisualAst, nilAlt, consAlt)
  }
  def toVisualExpr =
    visual.Richtext("case ") + selector.toVisualExpr +
      visual.Richtext(" of nil → ") + nilExpr.toVisualExpr +
      visual.Richtext(" | ") +
      carPat.toVisualExpr + visual.Richtext("::") + cdrPat.toVisualExpr +
      visual.Richtext(" → ") + consExpr.toVisualExpr
  def isRightOpen = true
}

object Const {
  def apply(x: Int): Const = Const(x.toString, FLInt)
  def apply(x: Boolean): Const = Const(x.toString, FLBool)

  val add = Const("(+)", FLInt ->: FLInt ->: FLInt)
  val sub = Const("(-)", FLInt ->: FLInt ->: FLInt)
  val mul = Const("(×)", FLInt ->: FLInt ->: FLInt)
  val div = Const("(÷)", FLInt ->: FLInt ->: FLInt)
  val uminus = Const("(minus)", FLInt ->: FLInt)
  val uplus = Const("(plus)", FLInt ->: FLInt)
  val and = Const("(and)", FLBool ->: FLBool ->: FLBool)
  val or = Const("(or)", FLBool ->: FLBool ->: FLBool)
  val not = Const("(not)", FLBool ->: FLBool)
  val lt = Const("(<)", FLInt ->: FLInt ->: FLBool)
  val le = Const("(≤)", FLInt ->: FLInt ->: FLBool)
  val intEq = Const("(=)", FLInt ->: FLInt ->: FLBool)
  val ge = Const("(≥)", FLInt ->: FLInt ->: FLBool)
  val gt = Const("(>)", FLInt ->: FLInt ->: FLBool)
}

object Implicits {
  import scala.language.implicitConversions
  import scala.collection.immutable

  implicit def intToConst(x: Int): Const = Const(x.toString, FLInt)
  implicit def boolToConst(x: Boolean): Const = Const(x.toString, FLBool)
  implicit def strToVar(x: String): Var = Var(x)
  implicit def listToAst(x: List[Ast]): Ast = x match {
    case immutable.Nil => Nil
    case hd::tl => Cons(hd, listToAst(tl))
  }
  implicit def consToCons(x: immutable.::[Ast]): Cons =
    Cons(x.head, listToAst(x.tail))
  implicit def nilToNil(x: immutable.Nil.type): Nil.type = Nil
}
