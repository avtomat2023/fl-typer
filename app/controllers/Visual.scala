package visual

import play.api.libs.json._

sealed trait TextStyle
case object Upright extends TextStyle
case object Italic extends TextStyle
case object Subscript extends TextStyle

case class RichtextFragment(text: String, style: TextStyle)

sealed trait Element {
  def toJsObject: JsObject
}

case class Richtext(fragments: Vector[RichtextFragment]) extends Element {
  def toJsObject = JsObject(Seq(
    "kind" -> JsString("richtext"),
    "contents" -> JsArray(fragments.map{ case RichtextFragment(text, style) =>
      JsObject(Seq(
        "text" -> JsString(text),
        "italic" -> JsBoolean(style == Italic),
        "subscript" -> JsBoolean(style == Subscript)
      ))
    })
  ))

  def +(other: Richtext): Richtext = Richtext(this.fragments ++ other.fragments)
}

object Richtext {
  def apply(text: String, style: TextStyle = Upright): Richtext =
    Richtext(Vector(RichtextFragment(text, style)))
}

case class Tree(node: Richtext, children: Element*) extends Element {
  def toJsObject = JsObject(Seq(
    "kind" -> JsString("tree"),
    "node" -> node.toJsObject,
    "children" -> JsArray(children.map(_.toJsObject))
  ))
}

object Tree {
  def apply(node: String, children: Element*): Tree =
    Tree(Richtext(node), children: _*)
}

case class Diagram(node: Richtext, parents: Seq[Diagram],
                   unificators: Seq[Richtext]) extends Element {
  def toJsObject = JsObject(Seq(
    "kind" -> JsString("diagram"),
    "node" -> node.toJsObject,
    "parents" -> JsArray(parents.map(_.toJsObject)),
    "unificators" -> JsArray(unificators.map(_.toJsObject))
  ))
}
