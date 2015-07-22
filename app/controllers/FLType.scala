package fltype

sealed trait FLType {
  def ->:(other: FLType) = new ->:(other, this)
  def kind: Symbol
  def vars: Set[TypeVar]
  def subst(variable: TypeVar, flType: FLType): FLType
  def toVisual: visual.Richtext
}

case class TypeVar(greek: String, index: Option[Int]) extends FLType {
  def kind = 'var
  def vars = Set(this)
  def subst(variable: TypeVar, flType: FLType) =
    if (variable == this) flType else this
  def toVisual: visual.Richtext =
    index.foldLeft(visual.Richtext(greek)){ (g,i) =>
      g + visual.Richtext(i.toString, visual.Subscript)
    }
}

object TypeVar {
  def apply(greek: String): TypeVar = TypeVar(greek, None)
  def apply(greek: String, index: Int): TypeVar = TypeVar(greek, Some(index))
}

sealed abstract class Application(flTypes: Seq[FLType]) extends FLType {
  def vars = flTypes.flatMap(_.vars).toSet
}
object Application {
  def unapply(x: FLType) = x match {
    case FLInt               => Some(Seq())
    case FLBool              => Some(Seq())
    case FLList(paramType)   => Some(Seq(paramType))
    case argType ->: retType => Some(Seq(argType, retType))
    case _                   => None
  }
}

case object FLInt extends Application(Seq()) {
  def kind = 'int
  def subst(variable: TypeVar, flType: FLType) = FLInt
  def toVisual = visual.Richtext("int")
}
case object FLBool extends Application(Seq()) {
  def kind = 'bool
  def subst(variable: TypeVar, flType: FLType) = FLBool
  def toVisual = visual.Richtext("bool")
}
case class FLList(paramType: FLType) extends Application(Seq(paramType)) {
  def kind = 'list
  def subst(variable: TypeVar, flType: FLType) =
    FLList(paramType.subst(variable, flType))
  def toVisual =
    visual.Richtext("list(") + paramType.toVisual + visual.Richtext(")")
}
case class ->:(argType: FLType, retType: FLType)
    extends Application(Seq(argType, retType)) {
  def kind = 'func
  def subst(variable: TypeVar, flType: FLType) =
    argType.subst(variable, flType) ->: retType.subst(variable, flType)
  def toVisual = argType.toVisual + visual.Richtext(" → ") + retType.toVisual
}
