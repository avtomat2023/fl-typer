package proofdiagram

import scala.collection.immutable.ListMap
import scala.collection.mutable
import fltype._
import unification._

case class TypeVarGenerator(varCount: Int) {
  def next: (TypeVar, TypeVarGenerator) =
    (TypeVar("τ", varCount+1), TypeVarGenerator(varCount+1))
}

case class ProofDiagram(expr: ast.Ast, flType: FLType,
                        context: ListMap[ast.Var, FLType],
                        varGenerator: TypeVarGenerator) {
  import visual.Richtext
  private val Eq = TypeEquation
  val (parents: Seq[ProofDiagram],
       nextVarGenerator: TypeVarGenerator,
       unificator: Option[TypeEquation]) = expr match {
    case ast.Const(_, t) => (Seq(), varGenerator, Some(Eq(flType, t)))
    case ast.Var(name) => (Seq(), varGenerator,
                           context.get(ast.Var(name)).map(Eq(flType, _)))
    case ast.Nil => {
      val (t, generator) = varGenerator.next
      (Seq(), generator, Some(Eq(flType, FLList(t))))
    }
    case ast.Cons(car, cdr) => {
      val (carType, generator) = varGenerator.next
      val carDiagram = ProofDiagram(car, carType, context, generator)
      val cdrDiagram = ProofDiagram(cdr, FLList(carType), context,
                                    carDiagram.nextVarGenerator)
      (Seq(carDiagram, cdrDiagram), cdrDiagram.nextVarGenerator,
       Some(Eq(flType, FLList(carType))))
    }
    case ast.Abs(variable, body) => {
      val (argType, generator1) = varGenerator.next
      val (retType, generator2) = generator1.next
      val newContext = context + (variable->argType)
      val parent = ProofDiagram(body, retType, newContext, generator2)
      (Seq(parent), parent.nextVarGenerator,
       Some(Eq(flType, argType->:retType)))
    }
    case ast.App(func, arg) => {
      val (argType, generator1) = varGenerator.next
      val (retType, generator2) = generator1.next
      val fType = argType ->: retType
      val funcDiagram = ProofDiagram(func, fType, context, generator2)
      val argDiagram = ProofDiagram(arg, argType, context,
                                    funcDiagram.nextVarGenerator)
      (Seq(funcDiagram, argDiagram), argDiagram.nextVarGenerator,
       Some(Eq(flType, retType)))
    }
    case ast.Let(variable, binding, body) => {
      val (bindingType, generator1) = varGenerator.next
      val (bodyType, generator2) = generator1.next
      val bindingDiagram = ProofDiagram(binding, bindingType, context,
                                        generator2)
      val newContext = context + (variable->bindingType)
      val bodyDiagram = ProofDiagram(body, bodyType, newContext,
                                     bindingDiagram.nextVarGenerator)
      (Seq(bindingDiagram, bodyDiagram), bodyDiagram.nextVarGenerator,
       Some(Eq(flType, bodyType)))
    }
    case ast.If(cond, thenExpr, elseExpr) => {
      val (exprType, generator) = varGenerator.next
      val condDiagram = ProofDiagram(cond, FLBool, context, generator)
      val thenDiagram = ProofDiagram(thenExpr, exprType, context,
                                     condDiagram.nextVarGenerator)
      val elseDiagram = ProofDiagram(elseExpr, exprType, context,
                                     thenDiagram.nextVarGenerator)
      (Seq(condDiagram, thenDiagram, elseDiagram),
       elseDiagram.nextVarGenerator,
       Some(Eq(flType, exprType)))
    }
    case ast.Case(selector, nilExpr, carPat, cdrPat, consExpr) => {
      val (listType, generator1) = varGenerator.next
      val (resultType, generator2) = generator1.next
      val selDiagram = ProofDiagram(selector, FLList(listType), context,
                                    generator2)
      val nilDiagram = ProofDiagram(nilExpr, resultType, context,
                                        selDiagram.nextVarGenerator)
      val consContext = context +
        (carPat -> listType) + (cdrPat -> FLList(listType))
      val consDiagram = ProofDiagram(consExpr, resultType, consContext,
                                     nilDiagram.nextVarGenerator)
      (Seq(selDiagram, nilDiagram, consDiagram),
       consDiagram.nextVarGenerator,
       Some(Eq(flType, resultType)))
    }
  }
  def isFreeVariableError: Boolean = unificator.isEmpty
  def allUnificators: Seq[TypeEquation] =
    unificator.toSeq ++ parents.flatMap(_.allUnificators)
  def unification: Unification =
    new Unification(mutable.ListBuffer(allUnificators: _*))

  def typingRuleName: String = expr match {
    case ast.Const(_,_) => "con"
    case ast.Var(_) => "var"
    case ast.Cons(_,_) => "list"
    case ast.Nil => "nil"
    case ast.If(_,_,_) => "if"
    case ast.Abs(_,_) => "abs"
    case ast.App(_,_) => "app"
    case ast.Let(_,_,_) => "let"
    case ast.Case(_,_,_,_,_) => "case"
  }

  def toVisual: visual.ProofDiagram = {
    val visualContext =
      if (context.isEmpty)
        Richtext("")
      else
        context.map{ case (v, t) =>
          v.toVisualExpr + Richtext(" ↦ ") + t.toVisual
        }.reduceLeft{
          _ + Richtext(", ") + _
        }
    val visualExpr = visualContext + Richtext(" ⊢ ") +
      expr.toVisualExpr + Richtext(" : ") +
      flType.toVisual
    val visualRule = Richtext("(" + typingRuleName + ")", visual.Middle)
    visual.ProofDiagram(visualExpr, visualRule, parents.map(_.toVisual))
  }
}

object ProofDiagram {
  def make(expr: ast.Ast) =
    ProofDiagram(expr, TypeVar("τ"), ListMap(), TypeVarGenerator(0))
}
