package unification

import fltype._
import visual._

case class TypeEquation(lhs: FLType, rhs: FLType) {
  def visual: Richtext = lhs.visual + Richtext(" = ") + rhs.visual
}

sealed trait Result
case class Success(result: FLType) extends Result
case class Error(msg: String) extends Result

abstract case class Unification(eqs: Seq[TypeEquation]) {
  def next(solveFor: TypeVar = TypeVar("τ")): Either[Result, Unification]
  def solve(solveFor: TypeVar = TypeVar("τ")): Result
}
