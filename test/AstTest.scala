import ast._
import ast.Implicits._
import ast.Const._
import fltype._

import org.scalatest.FunSuite

class AstVisualExprText extends FunSuite {
  test("operator: 1 + 2 ... (+) 1 2") {
    assertResult("(+) 1 2"){ App(App(add, 1), 2).toVisualExpr.mkString }
  }

  test("precedence: 1+2*3 ... (+) 1 ((×) 2 3)") {
    assertResult("(+) 1 ((×) 2 3)") {
      App(App(add, 1), App(App(mul, 2), 3)).toVisualExpr.mkString
    }
  }

  test("parened let: (let id = λx. x in id) 1") {
    assertResult("(let id = λx. x in id) 1") {
      val (id, x) = (Var("id"), Var("x"))
      App(Let(id, Abs(x, x), id), 1).toVisualExpr.mkString
    }
  }
}
