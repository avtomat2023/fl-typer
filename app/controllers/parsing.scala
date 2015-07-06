package parsing

import scala.util.parsing.combinator.RegexParsers
import play.api.libs.json._

sealed abstract class Ast {
  def toJsValue: JsValue
  def toJsObject: JsObject = JsObject(Seq("ast" -> toJsValue))
}

case class Const(value: String) extends Ast {
  def toJsValue = JsString(value)
}
case class Var(name: String) extends Ast {
  def toJsValue = JsString(name)
}
case object Nil extends Ast {
  def toJsValue = JsString("nil")
}
case class Cons(car: Ast, cdr: Ast) extends Ast {
  def toJsValue =
    JsArray(Seq(JsString("::"), car.toJsValue, cdr.toJsValue))
}
case class Abs(variable: Var, body: Ast) extends Ast {
  def toJsValue =
    JsArray(Seq(JsString("λ"), variable.toJsValue, body.toJsValue))
}
case class App(func: Ast, arg: Ast) extends Ast {
  def toJsValue =
    JsArray(Seq(JsString("app"), func.toJsValue, arg.toJsValue))
}
case class If(cond: Ast, thenExpr: Ast, elseExpr: Ast) extends Ast {
  def toJsValue =
    JsArray(Seq(JsString("if"), cond.toJsValue,
                thenExpr.toJsValue, elseExpr.toJsValue))
}
case class Let(variable: Var, binding: Ast, body: Ast) extends Ast {
  def toJsValue =
    JsArray(Seq(JsString("let"), variable.toJsValue, binding.toJsValue,
                body.toJsValue))
}
case class Case(caseExpr: Ast, nilExpr: Ast,
                carPat: Var, cdrPat: Var, consExpr: Ast) extends Ast {
  def toJsValue = {
    val cons = carPat.name + "::" + cdrPat.name
    JsArray(Seq(JsString("case"), caseExpr.toJsValue,
                JsArray(Seq(JsString("nil"), nilExpr.toJsValue)),
                JsArray(Seq(JsString(cons), consExpr.toJsValue))))
  }
}

object Implicits {
  import scala.language.implicitConversions
  import scala.collection.immutable

  implicit def intToConst(x: Int): Const = Const(x.toString)
  implicit def boolToConst(x: Boolean): Const = Const(x.toString)
  implicit def strToVar(x: String): Var = Var(x)
  implicit def listToAst(x: List[Ast]): Ast = x match {
    case immutable.Nil => Nil
    case hd::tl => Cons(hd, tl)
  }
  implicit def consToCons(x: immutable.::[Ast]): Cons = Cons(x.head, x.tail)
  implicit def nilToNil(x: immutable.Nil.type): Nil.type = Nil
}

object FLParser extends RegexParsers {
  def const: Parser[Ast] = """[0-9]+|true|false""".r ^^ { Const(_) }
  val reserved = Set("true", "false", "nil", "if", "then", "else",
                     "let", "in", "case", "of")
  def variable: Parser[Var] = new Parser[Var] {
    def apply(in: Input) = {
      regex("""[a-zA-Z][a-zA-Z0-9_]*""".r)(in) match {
        case Success(varName, next) =>
          if (reserved(varName))
            new Failure("variable expected but reserved word `" +
                        varName + "' found", in)
          else
            Success(Var(varName), next)
        case NoSuccess(msg, next) => Failure(msg, next)
      }
    }
  }
  def nil: Parser[Nil.type] = "nil" ^^ { _ => Nil }
  def abs: Parser[Abs] = "\\"~>variable~"."~expr ^^ {
    case variable~sep~body => Abs(variable, body)
  }
  def ifExpr: Parser[If] = "if"~>expr~"then"~expr~"else"~expr ^^ {
    case cond~sep1~thenExpr~sep2~elseExpr => If(cond, thenExpr, elseExpr)
      }
  def let: Parser[Let] = "let"~>variable~"="~expr~"in"~expr ^^ {
    case variable~sep1~binding~sep2~body => Let(variable, binding, body)
  }
  def caseExpr: Parser[Case] =
    "case"~>expr~"of"~
      opt("|")~"nil"~"->"~expr~
      "|"~variable~"::"~variable~"->"~expr ^^ {
        case e~_~_~_~_~nilE~_~v1~_~v2~_~consE =>
        Case(e, nilE, v1, v2, consE)
      }

  def nonrecExpr: Parser[Ast] =
    const | variable | nil | abs | ifExpr | let | caseExpr

  def expr: Parser[Ast] = maybeCons
  def maybeCons: Parser[Ast] = rep1sep(maybeApp, "::") ^^ { results =>
    results.reduceRight(Cons(_,_))
  }
  def maybeApp: Parser[Ast] = rep1(maybeParen) ^^ { results =>
    results.reduceLeft(App(_,_))
  }
  def maybeParen: Parser[Ast] = nonrecExpr | "("~>expr<~")"

  def parse(input: String) = parseAll(expr, input)
}
