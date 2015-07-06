import parsing._

import org.scalatest.FunSuite

class ParsingTest extends FunSuite {
  import parsing.Implicits._

  test("'true'") {
    assertResult(Const("true")) { FLParser.parse("true").get }
  }

  test("lambda abstraction") {
    assertResult(Abs("x", Abs("y", App("x", "y")))) {
      FLParser.parse("\\x.λy.x y").get
    }
  }

  test("case expression") {
    assertResult(Case(Cons(1,Nil), 0, "hd", "tl", "hd")) {
      FLParser.parse("case 1::nil of nil -> 0 | hd::tl -> hd").get
    }
  }

  test("simple arithmetic expression") {
    assertResult(App(App(Const("-"), App(App(Const("+"), 1), 2)), 3)) {
      FLParser.parse("1+2-3").get
    }
  }

  test("operator '<='") {
    assertResult(App(App(Const("<="), 1), 2)) {
      FLParser.parse("1<=2").get
    }
  }

  test("method 'word'") {
    assertResult("and") {
      FLParser.parseAll(FLParser.word("and"), "and").get
    }
  }

  test("operator 'and'") {
    assertResult(App(App(Const("and"), true), false)) {
      FLParser.parse("true and false").get
    }
  }

  test("operator 'not'") {
    assertResult(App(Const("not"), true)) {
      FLParser.parse("not true").get
    }
  }

  test("logical expression") {
    assertResult(App(App(Const("or"), App(Const("not"), true)),
                     App(App(Const("and"), true), false))) {
      FLParser.parse("not true or true and false").get
    }
  }

  test("complex arighmetic and logical expression 1") {
    assertResult(App(App(Const("and"),
                         App(App(Const("<="), 1), 2)),
                     App(App(Const("<"), 3), 4))) {
      FLParser.parse("1<=2 and 3<4").get
    }
  }

  test("complex arithmetic and logical expression 2") {
    assertResult(App(App(Const("and"),
                         App(App(Const("<="),
                                 App(App(Const("+"), 1), 2)),
                             3)),
                     App(App(Const("<"),
                             App(Const("-"), 4)),
                         5))) {
      FLParser.parse("1+2<=3and-4<5").get
    }
  }

  test("character '=' as operator and let binder") {
    assertResult(Let("x", App(App(Const("="), 1), 2), App(Const("not"), "x"))) {
      FLParser.parse("let x=1=2 in not x").get
    }
  }
}
