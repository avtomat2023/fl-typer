import proofdiagram._
import unification._
import fltype._
import visual._
import ast._

import org.scalatest.FunSuite

class ProofDiagramTest extends FunSuite {
  import ast.Implicits._

  val tau = TypeVar("τ")
  def tau(n: Int) = TypeVar("τ", n)
  val Eq = TypeEquation

  test("free variable: x") {
    val expr = Var("x")
    val diagram = ProofDiagram.make(expr)
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.isEmpty)
    assert(diagram.unificator == None)
    assert(diagram.isFreeVariableError)

    assert(diagram.allUnificators.isEmpty)
    assert(diagram.nextVarGenerator.varCount == 0)
  }

  test("const: 1") {
    val expr = Const(1)
    val diagram = ProofDiagram.make(expr)
    val u = Eq(tau, FLInt)
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.isEmpty)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    assert(diagram.allUnificators == Seq(Eq(tau, FLInt)))
    assert(diagram.nextVarGenerator.varCount == 0)
  }

  test("nil") {
    val diagram = ProofDiagram.make(Nil)
    val u = Eq(tau, FLList(tau(1)))
    assert(diagram.expr == Nil)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.isEmpty)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    assert(diagram.allUnificators == Seq(u))
    assert(diagram.nextVarGenerator.varCount == 1)
  }

  test("cons: 1::nil") {
    val car = Const(1)
    val cdr = Nil
    val expr = Cons(car, cdr)
    val diagram = ProofDiagram.make(expr)
    val u = Eq(tau, FLList(tau(1)))
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.length == 2)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    val par0 = diagram.parents(0)
    val u0 = Eq(tau(1), FLInt)
    assert(par0.expr == car)
    assert(par0.flType == tau(1))
    assert(par0.context == Map())
    assert(par0.parents.isEmpty)
    assert(par0.unificator == Some(u0))
    assert(!par0.isFreeVariableError)

    val par1 = diagram.parents(1)
    val u1 = Eq(FLList(tau(1)), FLList(tau(2)))
    assert(par1.expr == cdr)
    assert(par1.flType == FLList(tau(1)))
    assert(par1.context == Map())
    assert(par1.parents.isEmpty)
    assert(par1.unificator == Some(u1))
    assert(!par1.isFreeVariableError)

    assert(diagram.allUnificators == Seq(u, u0, u1))
    assert(diagram.nextVarGenerator.varCount == 2)
  }

  test("abs: λx.x") {
    val x = Var("x")
    val expr = Abs(x, x)
    val diagram = ProofDiagram.make(expr)
    val u = Eq(tau, tau(1)->:tau(2))
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.length == 1)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    val par = diagram.parents(0)
    val u1 = Eq(tau(2), tau(1))
    assert(par.expr == x)
    assert(par.flType == tau(2))
    assert(par.context == Map(x -> tau(1)))
    assert(par.parents.isEmpty)
    assert(par.unificator == Some(u1))
    assert(!diagram.isFreeVariableError)

    assert(diagram.allUnificators == Seq(u, u1))
    assert(diagram.nextVarGenerator.varCount == 2)
  }

  test("app: 1+2") {
    val expr = App(App(Const.add, 1), 2)
    val diagram = ProofDiagram.make(expr)
    val u = Eq(tau, tau(2))
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.length == 2)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    val par0 = diagram.parents(0)
    val u1 = Eq(tau(1) ->: tau(2), tau(4))
    assert(par0.expr == App(Const.add, 1))
    assert(par0.flType == (tau(1) ->: tau(2)))
    assert(par0.context == Map())
    assert(par0.parents.length == 2)
    assert(par0.unificator == Some(u1))
    assert(!par0.isFreeVariableError)

    val par00 = par0.parents(0)
    val u11 = Eq(tau(3) ->: tau(4), FLInt ->: FLInt ->: FLInt)
    assert(par00.expr == Const.add)
    assert(par00.flType == (tau(3) ->: tau(4)))
    assert(par00.context == Map())
    assert(par00.parents.isEmpty)
    assert(par00.unificator == Some(u11))
    assert(!par00.isFreeVariableError)

    val par01 = par0.parents(1)
    val u12 = Eq(tau(3), FLInt)
    assert(par01.expr == Const(1))
    assert(par01.flType == tau(3))
    assert(par01.context == Map())
    assert(par01.parents.isEmpty)
    assert(par01.unificator == Some(u12))
    assert(!par01.isFreeVariableError)

    val par1 = diagram.parents(1)
    val u2 = Eq(tau(1), FLInt)
    assert(par1.expr == Const(2))
    assert(par1.flType == tau(1))
    assert(par1.context == Map())
    assert(par1.parents.isEmpty)
    assert(par1.unificator == Some(u2))
    assert(!par1.isFreeVariableError)

    assert(diagram.allUnificators == Seq(u, u1, u11, u12, u2))
    assert(diagram.nextVarGenerator.varCount == 4)
  }

  test("if-expression: if true then 1 else 2") {
    val expr = If(true, 1, 2)
    val diagram = ProofDiagram.make(expr)
    val u = Eq(tau, tau(1))
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.length == 3)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    val par0 = diagram.parents(0)
    val u0 = Eq(FLBool, FLBool)
    assert(par0.expr == Const(true))
    assert(par0.flType == FLBool)
    assert(par0.context == Map())
    assert(par0.parents.isEmpty)
    assert(par0.unificator == Some(u0))
    assert(!par0.isFreeVariableError)

    val par1 = diagram.parents(1)
    val u1 = Eq(tau(1), FLInt)
    assert(par1.expr == Const(1))
    assert(par1.flType == tau(1))
    assert(par1.context == Map())
    assert(par1.parents.isEmpty)
    assert(par1.unificator == Some(u1))
    assert(!par1.isFreeVariableError)

    val par2 = diagram.parents(2)
    val u2 = Eq(tau(1), FLInt)
    assert(par2.expr == Const(2))
    assert(par2.flType == tau(1))
    assert(par2.context == Map())
    assert(par2.parents.isEmpty)
    assert(par2.unificator == Some(u2))
    assert(!par2.isFreeVariableError)

    assert(diagram.allUnificators == Seq(u, u0, u1, u2))
    assert(diagram.nextVarGenerator.varCount == 1)
  }

  test("let-expression: let x = 1 in x") {
    val x = Var("x")
    val expr = Let(x, 1, x)
    val diagram = ProofDiagram.make(expr)
    val u = Eq(tau, tau(2))
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.length == 2)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    val par0 = diagram.parents(0)
    val u0 = Eq(tau(1), FLInt)
    assert(par0.expr == Const(1))
    assert(par0.flType == tau(1))
    assert(par0.context == Map())
    assert(par0.parents.isEmpty)
    assert(par0.unificator == Some(u0))
    assert(!par0.isFreeVariableError)

    val par1 = diagram.parents(1)
    val u1 = Eq(tau(2), tau(1))
    assert(par1.expr == x)
    assert(par1.flType == tau(2))
    assert(par1.context == Map(x -> tau(1)))
    assert(par1.parents.isEmpty)
    assert(par1.unificator == Some(u1))
    assert(!par1.isFreeVariableError)

    assert(diagram.allUnificators == Seq(u, u0, u1))
    assert(diagram.nextVarGenerator.varCount == 2)
  }

  test("case-expression: case nil of nil -> 0 | hd::tl -> hd") {
    val (hd, tl) = (Var("hd"), Var("tl"))
    val expr = Case(Nil, 0, hd, tl, hd)
    val diagram = ProofDiagram.make(expr)
    val u = Eq(tau, tau(2))
    assert(diagram.expr == expr)
    assert(diagram.flType == tau)
    assert(diagram.context == Map())
    assert(diagram.parents.length == 3)
    assert(diagram.unificator == Some(u))
    assert(!diagram.isFreeVariableError)

    val par0 = diagram.parents(0)
    val u0 = Eq(FLList(tau(1)), FLList(tau(3)))
    assert(par0.expr == Nil)
    assert(par0.flType == FLList(tau(1)))
    assert(par0.context == Map())
    assert(par0.parents.isEmpty)
    assert(par0.unificator == Some(u0))
    assert(!par0.isFreeVariableError)

    val par1 = diagram.parents(1)
    val u1 = Eq(tau(2), FLInt)
    assert(par1.expr == Const(0))
    assert(par1.flType == tau(2))
    assert(par1.context == Map())
    assert(par1.parents.isEmpty)
    assert(par1.unificator == Some(u1))
    assert(!par1.isFreeVariableError)

    val par2 = diagram.parents(2)
    val u2 = Eq(tau(2), tau(1))
    assert(par2.expr == hd)
    assert(par2.flType == tau(2))
    assert(par2.context == Map(hd -> tau(1), tl -> FLList(tau(1))))
    assert(par2.parents.isEmpty)
    assert(par2.unificator == Some(u2))
    assert(!par2.isFreeVariableError)

    assert(diagram.allUnificators == Seq(u, u0, u1, u2))
    assert(diagram.nextVarGenerator.varCount == 3)
  }
}
