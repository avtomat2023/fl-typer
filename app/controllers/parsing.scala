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
    case hd::tl => Cons(hd, tl)
  }
  implicit def consToCons(x: immutable.::[Ast]): Cons = Cons(x.head, x.tail)
  implicit def nilToNil(x: immutable.Nil.type): Nil.type = Nil
}

object FLParser extends RegexParsers {
  // 入力文字列全体を一つの式だと思ってパース
  def parse(input: String) = parseAll(expr, input)
  def expr: Parser[Ast] = consExpr

  def consExpr: Parser[Ast] = rep1sep(orExpr, "::") ^^ { results =>
    results.reduceRight(Cons(_,_))
  }

  // 左結合の二項演算子を使った式をパース
  def leftBinOpExpr(operator: Parser[String],
                    nextLevel: Parser[Ast]): Parser[Ast] =
    nextLevel~rep(operator~nextLevel) ^^ { case first~rest =>
      rest.foldLeft(first){ (tree, node) =>
        node match {
          case op~expr => App(App(Const(op), tree), expr)
        }
      }
    }

  // 識別子または予約語
  def word: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  // 述語predを満たすword
  // wordが見つかったがpredを満たさない場合、err(word)がエラーメッセージとなる
  def wordWith(pred: String => Boolean, err: String => String): Parser[String] =
    new Parser[String] {
      def apply(in: Input) = {
        word(in) match {
          case Success(w, next) =>
            if (pred(w))
              Success(w, next)
            else
              Failure(err(w), in)
          case NoSuccess(msg, next) => Failure(msg, next)
        }
      }
    }
  // errを省略したwordWith
  def wordWith(pred: String => Boolean): Parser[String] =
    wordWith(pred, "`" + _ + "' does not satisfy the required condition")
  // 与えられた文字列群のどれか一つに一致するword
  def word(expecteds: String*): Parser[String] = wordWith(expecteds.contains(_))

  def orExpr: Parser[Ast] = leftBinOpExpr(word("or"), andExpr)
  def andExpr: Parser[Ast] = leftBinOpExpr(word("and"), comparisonExpr)
  def comparisonExpr: Parser[Ast] = leftBinOpExpr("<=|<|=|>=|>".r, addsubExpr)
  def addsubExpr: Parser[Ast] = leftBinOpExpr("""\+|-""".r, muldivExpr)
  def muldivExpr: Parser[Ast] = leftBinOpExpr("""\*|/""".r, prefixExpr)

  def prefixExpr: Parser[Ast] = {
    val op = "+" | "-" | word("not")
    opt(op)~appExpr ^^ {
      case prefix~expr => prefix match {
        case Some(op) => App(Const(op), expr)
        case None => expr
      }
    }
  }

  def appExpr: Parser[Ast] = rep1(parenExpr) ^^ { results =>
    results.reduceLeft(App(_,_))
  }
  def parenExpr: Parser[Ast] = nonLeftRecursiveExpr | "("~>expr<~")"

  def nonLeftRecursiveExpr: Parser[Ast] =
    const | variable | nil | abs | ifExpr | letExpr | caseExpr

  def const: Parser[Ast] =
    ("""[0-9]+""".r | word("true", "false")) ^^ { Const(_) }

  val reservedWords = Set("true", "false", "and", "or", "not", "nil",
                          "if", "then", "else", "let", "in", "case", "of")
  def variable: Parser[Var] = wordWith(!reservedWords(_)) ^^ { Var(_) }

  def nil: Parser[Nil.type] = "nil" ^^ { _ => Nil }
  def abs: Parser[Abs] = """\\|λ""".r~>rep1(variable<~".")~expr ^^ {
    case vs~body => vs.init.foldRight(Abs(vs.last, body)){ Abs(_,_) }
  }
  def ifExpr: Parser[If] = "if"~>expr~"then"~expr~"else"~expr ^^ {
    case cond~_~thenExpr~_~elseExpr => If(cond, thenExpr, elseExpr)
  }
  def letExpr: Parser[Let] = "let"~>variable~"="~expr~"in"~expr ^^ {
    case variable~_~binding~_~body => Let(variable, binding, body)
  }
  def caseExpr: Parser[Case] =
    "case"~>expr~"of" ~ opt("|")~"nil"~"->"~expr ~
    "|"~variable~"::"~variable~"->"~expr ^^ {
      case e~_~_~_~_~nilE~_~v1~_~v2~_~consE =>
        Case(e, nilE, v1, v2, consE)
    }
}
