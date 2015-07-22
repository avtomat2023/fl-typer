package fltype

sealed trait FLType {
  def ->:(other: FLType) = new ->:(this, other)
  def toVisual: visual.Richtext
}

case class TypeVar(greek: String, index: Option[Int]) extends FLType {
  def toVisual: visual.Richtext =
    index.foldLeft(visual.Richtext(greek)){ (g,i) =>
      g + visual.Richtext(i.toString, visual.Subscript)
    }
}
object TypeVar {
  def apply(greek: String): TypeVar = TypeVar(greek, None)
  def apply(greek: String, index: Int): TypeVar = TypeVar(greek, Some(index))
}
case object FLInt extends FLType {
  def toVisual = visual.Richtext("int")
}
case object FLBool extends FLType {
  def toVisual = visual.Richtext("bool")
}
case class FLList(paramType: FLType) extends FLType {
  def toVisual =
    visual.Richtext("list(") + paramType.toVisual + visual.Richtext(")")
}
case class ->:(argType: FLType, retType: FLType) extends FLType {
  def toVisual = argType.toVisual + visual.Richtext(" → ") + retType.toVisual
}

case class TypeVarGenerator(varCount: Int) {
  def next: (TypeVar, TypeVarGenerator) =
    (TypeVar("τ", varCount+1), TypeVarGenerator(varCount+1))
}
