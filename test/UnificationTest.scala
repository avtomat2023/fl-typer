import fltype._
import unification._
import SharpTypeVarGenerator._
import scala.collection.mutable.ListBuffer

import org.scalatest.FunSuite

object Common {
  val V = TypeVar
  val Eq = TypeEquation
  def t(n: Int) = V("τ", n)
  val t = V("τ")
}
import Common._

class SharpenTest extends FunSuite {
  test("τ123 TO α") {
    assertResult(V("α")){ sharpen(t(123)) }
  }

  test("τ3 -> τ1 -> τ2 TO α -> β -> γ") {
    assertResult(V("α") ->: V("β") ->: V("γ")) {
      sharpen(t(3) ->: t(1) ->: t(2))
    }
  }

  test("list(τ9->τ3) -> int -> list(τ3->τ7) TO " +
       "list(α->β) -> int -> list(β->γ)") {
    assertResult(FLList(V("α") ->: V("β")) ->: FLInt ->:
                 FLList(V("β") ->: V("γ"))) {
      sharpen(FLList(t(9)->:t(3)) ->: FLInt ->: FLList(t(3)->:t(7)))
    }
  }
}

class TypeEquationTest extends FunSuite {
  test("vars: τ2->τ3 = τ3->τ4->τ5") {
    assertResult(Set(t(2), t(3), t(4), t(5))) {
      Eq(t(2) ->: t(3), t(3) ->: t(4) ->: t(5)).vars
    }
  }

  test("diffrent application: int = bool") {
    assert(Eq(FLInt, FLBool).isDifferentApplication)
  }

  test("recursive: τ = τ->int") { assert(Eq(t, t ->: FLInt).isRecursive) }

  test("trivial: τ = τ") { assert(Eq(t, t).isTrivial) }
  test("not trivial: τ = τ1") { assert(!Eq(t, t(1)).isTrivial) }
  test("not trivial: int = int") { assert(!Eq(FLInt, FLInt).isTrivial) }

  test("decomposed: τ1->int = int->τ2") {
    assertResult(Some(Seq(Eq(t(1), FLInt), Eq(FLInt, t(2))))) {
      Eq(t(1) ->: FLInt, FLInt ->: t(2)).decomposeOption
    }
  }
  test("decomposed: int = int") {
    assertResult(Some(Seq())){ Eq(FLInt, FLInt).decomposeOption }
  }

  test("flippable: int = τ") {
    assert(Eq(FLInt, t).isFlippable)
  }

  test("subst: (τ1->τ2 = τ1->τ3->τ4)[τ1 <-| τ2]") {
    assertResult(Eq(t(2)->:t(2), t(2)->:t(3)->:t(4))) {
      Eq(t(1)->:t(2), t(1)->:t(3)->:t(4)).subst(t(1), t(2))
    }
  }
}

class UnificationTest extends FunSuite {
  test("findTrivial: found") {
    val e0 = Eq(t(1), t(2))
    val e1 = Eq(FLInt, FLInt)
    val e2 = Eq(t(3), t(3))
    val e3 = Eq(t(1), t(3) ->: t(4))
    val u = Unification(ListBuffer(e0, e1, e2, e3))
    assert(u.removeTrivial())
    assert(u == Unification(ListBuffer(e0, e1, e3)))
  }

  test("substitute") {
    val e0 = Eq(t, t(1))
    val e1 = Eq(t(2), t(3))
    val e2 = Eq(t(3), t(4))
    val e3 = Eq(t(3), t(5))
    val e4 = Eq(t(3)->:t(4), FLInt->:FLInt)
    val u = Unification(ListBuffer(e0, e1, e2, e3, e4))

    val e1sub = Eq(t(2), t(4))
    val e3sub = Eq(t(4), t(5))
    val e4sub = Eq(t(4)->:t(4), FLInt->:FLInt)

    assert(u.substitute())
    assert(u == Unification(ListBuffer(e0, e1sub, e2, e3sub, e4sub)))
  }
}
