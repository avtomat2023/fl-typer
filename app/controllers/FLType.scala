package fltype

sealed trait FLType {
  def -->(other: FLType) = new -->(this, other)
}

case class TypeVar(greek: String, index: Option[Int]) extends FLType
object TypeVar {
  def apply(greek: String): TypeVar = TypeVar(greek, None)
  def apply(greek: String, index: Int): TypeVar = TypeVar(greek, Some(index))
}
case object FLInt extends FLType
case object FLBool extends FLType
case class FLList(paramType: FLType) extends FLType
case class -->(argType: FLType, retType: FLType) extends FLType
