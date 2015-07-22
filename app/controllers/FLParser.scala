package parsing

import scala.util.parsing.combinator.RegexParsers

import ast._
import fltype._

object FLParser extends RegexParsers {
  // 入力文字列全体を一つの式だと思ってパース
  def parse(input: String) = parseAll(expr, input)
  def expr: Parser[Ast] = consExpr

  def consExpr: Parser[Ast] = rep1sep(orExpr, "::") ^^ {
    _.reduceRight(Cons(_,_))
  }

  // 左結合の二項演算子を使った式をパース
  def leftBinOpExpr(operator: Parser[Const],
                    nextLevel: Parser[Ast]): Parser[Ast] =
    nextLevel~rep(operator~nextLevel) ^^ { case first~rest =>
      rest.foldLeft(first){ (tree, node) =>
        node match {
          case op~expr => App(App(op, tree), expr)
        }
      }
    }

  // 識別子または予約語
  def word: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  // 述語predを満たすword
  // wordが見つかったがpredを満たさない場合、err(word)がエラーメッセージとなる
  def wordWith(pred: String => Boolean, err: String => String): Parser[String] =
    new Parser[String] {
      def apply(in: Input) =
        word(in) match {
          case Success(w, next) =>
            if (pred(w))
              Success(w, next)
            else
              Failure(err(w), in)
          case NoSuccess(msg, next) => Failure(msg, next)
        }
    }
  // errを省略したwordWith
  def wordWith(pred: String => Boolean): Parser[String] =
    wordWith(pred, "`" + _ + "' does not satisfy the required condition")
  // 与えられた文字列群のどれか一つに一致するword
  def word(expectedFirst: String, expectedRest: String*): Parser[String] = {
    val expecteds = expectedFirst :: expectedRest.toList
    wordWith(expecteds.contains(_),
             expecteds.reduceLeft(_+" or "+_) + " expected but `" +
             _ + "' found")
  }

  // parserを用いて演算子をパースし、opを返す
  def operator(parser: Parser[String], op: Const): Parser[Const] =
    parser ^^ { _ => op }

  def orExpr: Parser[Ast] =
    leftBinOpExpr(operator(word("or"), Const.or), andExpr)
  def andExpr: Parser[Ast] =
    leftBinOpExpr(operator(word("and"), Const.and), comparisonExpr)
  def comparisonExpr: Parser[Ast] = {
    val op = operator("<=", Const.le) | operator("<", Const.lt) |
             operator("=", Const.intEq) |
             operator(">=", Const.ge) | operator(">", Const.gt)
    leftBinOpExpr(op, addsubExpr)
  }
  def addsubExpr: Parser[Ast] = {
    val op = operator("+", Const.add) | operator("-", Const.sub)
    leftBinOpExpr(op, muldivExpr)
  }
  def muldivExpr: Parser[Ast] = {
    val op = operator("*", Const.mul) | operator("/", Const.div)
    leftBinOpExpr(op, prefixExpr)
  }

  def prefixExpr: Parser[Ast] = {
    val op = operator("+", Const.uplus) | operator("-", Const.uminus) |
             operator(word("not"), Const.not)
    opt(op) ~ appExpr ^^ {
      case Some(op)~expr => App(op, expr)
      case None~expr => expr
    }
  }

  def appExpr: Parser[Ast] = rep1(parenExpr) ^^ { _.reduceLeft(App(_,_)) }
  def parenExpr: Parser[Ast] = nonLeftRecursiveExpr | "("~>expr<~")"

  def nonLeftRecursiveExpr: Parser[Ast] =
    intConst | boolConst | variable | nil | abs | ifExpr | letExpr | caseExpr

  def intConst: Parser[Const] = """[0-9]+""".r ^^ { Const(_, FLInt) }
  def boolConst: Parser[Const] = word("true", "false") ^^ { Const(_, FLBool) }

  val reservedWords = Set("true", "false", "and", "or", "not", "nil",
                          "if", "then", "else", "let", "in", "case", "of")
  def variable: Parser[Var] = {
    def err(found: String) =
      "`" + found + "' is a reserved word, not available for variable name"
    wordWith(!reservedWords(_), err) ^^ { Var(_) }
  }

  def nil: Parser[Nil.type] = word("nil") ^^ { _ => Nil }
  def abs: Parser[Abs] = """\\|¥|λ""".r~>rep1(variable<~".")~expr ^^ {
    case vs~body => vs.init.foldRight(Abs(vs.last, body)){ Abs(_,_) }
  }
  def ifExpr: Parser[If] =
    word("if")~>expr~word("then")~expr~word("else")~expr ^^ {
      case cond~_~thenExpr~_~elseExpr => If(cond, thenExpr, elseExpr)
    }
  def letExpr: Parser[Let] =
    word("let")~>variable~"="~expr~word("in")~expr ^^ {
      case variable~_~binding~_~body => Let(variable, binding, body)
    }
  def caseExpr: Parser[Case] =
    word("case")~>expr~word("of") ~ opt("|")~word("nil")~"->"~expr ~
    "|"~variable~"::"~variable~"->"~expr ^^ {
      case sel~_~_~_~_~nilExpr~_~v1~_~v2~_~consExpr =>
        Case(sel, nilExpr, v1, v2, consExpr)
    }
}
