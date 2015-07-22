package fltype

import visual._

sealed trait FLType {
  def ->:(other: FLType) = new ->:(this, other)
  def visual: Richtext
}

case class TypeVar(greek: String, index: Option[Int]) extends FLType {
  def visual: Richtext = index.foldLeft(Richtext(greek)){ (g,i) =>
    g + Richtext(i.toString, Subscript)
  }
}
object TypeVar {
  def apply(greek: String): TypeVar = TypeVar(greek, None)
  def apply(greek: String, index: Int): TypeVar = TypeVar(greek, Some(index))
}
case object FLInt extends FLType {
  def visual = Richtext("int")
}
case object FLBool extends FLType {
  def visual = Richtext("bool")
}
case class FLList(paramType: FLType) extends FLType {
  def visual = Richtext("list(") + paramType.visual + Richtext(")")
}
case class ->:(argType: FLType, retType: FLType) extends FLType {
  def visual = argType.visual + Richtext(" → ") + retType.visual
}

case class TypeVarGenerator(varCount: Int) {
  def next: (TypeVar, TypeVarGenerator) =
    (TypeVar("τ", varCount+1), TypeVarGenerator(varCount+1))
}
