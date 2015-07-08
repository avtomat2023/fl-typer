package parsing

import ast._
import scala.util.parsing.combinator.RegexParsers

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
  def word(expecteds: String*): Parser[String] = wordWith(expecteds.contains(_))

  def orExpr: Parser[Ast] = leftBinOpExpr(word("or"), andExpr)
  def andExpr: Parser[Ast] = leftBinOpExpr(word("and"), comparisonExpr)
  def comparisonExpr: Parser[Ast] = leftBinOpExpr("<=|<|=|>=|>".r, addsubExpr)
  def addsubExpr: Parser[Ast] = leftBinOpExpr("""\+|-""".r, muldivExpr)
  def muldivExpr: Parser[Ast] = leftBinOpExpr("""\*|/""".r, prefixExpr)

  def prefixExpr: Parser[Ast] = opt("+" | "-" | word("not")) ~ appExpr ^^ {
      case prefix~expr => prefix match {
        case Some(op) => App(Const(op), expr)
        case None => expr
      }
    }

  def appExpr: Parser[Ast] = rep1(parenExpr) ^^ { _.reduceLeft(App(_,_)) }
  def parenExpr: Parser[Ast] = nonLeftRecursiveExpr | "("~>expr<~")"

  def nonLeftRecursiveExpr: Parser[Ast] =
    const | variable | nil | abs | ifExpr | letExpr | caseExpr

  def const: Parser[Ast] =
    ("""[0-9]+""".r | word("true", "false")) ^^ { Const(_) }

  val reservedWords = Set("true", "false", "and", "or", "not", "nil",
                          "if", "then", "else", "let", "in", "case", "of")
  def variable: Parser[Var] = wordWith(!reservedWords(_)) ^^ { Var(_) }

  def nil: Parser[Nil.type] = "nil" ^^ { _ => Nil }
  def abs: Parser[Abs] = """\\|¥|λ""".r~>rep1(variable<~".")~expr ^^ {
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
      case e~_~_~_~_~nilExpr~_~v1~_~v2~_~consExpr =>
        Case(e, nilExpr, v1, v2, consExpr)
    }
}
