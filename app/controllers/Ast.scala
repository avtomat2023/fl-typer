package ast

import play.api.libs.json.JsObject

import visual._

sealed trait Ast {
  def toVisualAst: Element
  def toVisualExpr: Richtext
  def parenedVisualExpr = Richtext("(") + toVisualExpr + Richtext(")")
  // 式を具象構文で書いたとき、右端がAbsかIfかLetかCaseの終わりならtrueを返す
  def isRightOpen: Boolean
}

object Ast {
  // utility function
  def visualAst(node: String, children: Ast*) =
    Tree(node, children.map(_.toVisualAst): _*)
}

case class Const(value: String) extends Ast {
  def toVisualAst = Richtext(value)
  def toVisualExpr = Richtext(value)
  def isRightOpen = false
}

case class Var(name: String) extends Ast {
  def toVisualAst = Richtext(name, Italic())
  def toVisualExpr = Richtext(name, Italic())
  def isRightOpen = false
}

case object Nil extends Ast {
  def toVisualAst = Richtext("nil")
  def toVisualExpr = Richtext("nil")
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
    carExpr + Richtext(" :: ") + cdr.toVisualExpr
  }
  def isRightOpen = cdr.isRightOpen
}

case class Abs(variable: Var, body: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("λ", variable, body)
  def toVisualExpr =
    Richtext("λ") + variable.toVisualExpr + Richtext(". ") + body.toVisualExpr
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
    funcExpr + Richtext(" ") + argExpr
  }
  def isRightOpen = arg.isRightOpen
}

case class If(cond: Ast, thenExpr: Ast, elseExpr: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("if", cond, thenExpr, elseExpr)
  def toVisualExpr =
    Richtext("if ") + cond.toVisualExpr +
      Richtext(" then ") + thenExpr.toVisualExpr +
      Richtext(" else ") + elseExpr.toVisualExpr
  def isRightOpen = true
}
case class Let(variable: Var, binding: Ast, body: Ast) extends Ast {
  def toVisualAst = Ast.visualAst("let", variable, binding, body)
  def toVisualExpr =
    Richtext("let ") + variable.toVisualExpr +
      Richtext(" = ") + binding.toVisualExpr +
      Richtext(" in ") + body.toVisualExpr
  def isRightOpen = true
}
case class Case(caseExpr: Ast, nilExpr: Ast,
                carPat: Var, cdrPat: Var, consExpr: Ast) extends Ast {
  def toVisualAst = {
    val nilAlt = Ast.visualAst("nil", nilExpr)
    val consPat = carPat.toVisualExpr + Richtext("::") + cdrPat.toVisualExpr
    val consAlt = Tree(consPat, consExpr.toVisualAst)
    Tree("case", caseExpr.toVisualAst, nilAlt, consAlt)
  }
  def toVisualExpr =
    Richtext("case ") + caseExpr.toVisualExpr +
      Richtext(" of nil → ") + nilExpr.toVisualExpr + Richtext(" | ") +
      carPat.toVisualExpr + Richtext("::") + cdrPat.toVisualExpr +
      Richtext(" → ") + consExpr.toVisualExpr
  def isRightOpen = true
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
