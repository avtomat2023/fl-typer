import parsing._

import org.scalatest.FunSuite

class ParsingTest extends FunSuite {
  import parsing.Implicits._

  test("case expression") {
    assertResult(Case(Cons(1,Nil), 0, "hd", "tl", "hd")) {
      FLParser.parse("case 1::nil of nil -> 0 | hd::tl -> hd").get
    }
  }
}
