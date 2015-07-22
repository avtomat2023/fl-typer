package visual

import play.api.libs.json._

sealed trait TextStyle
case object Upright extends TextStyle
case object Italic extends TextStyle
case object Subscript extends TextStyle

case class RichtextFragment(text: String, style: TextStyle)

case class Richtext(fragments: Vector[RichtextFragment]) {
  def toJsObject: JsObject = JsObject(Seq(
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

case class Ast(node: Richtext, children: Ast*) {
  def toJsObject: JsObject =
    if (children.isEmpty)
      node.toJsObject
    else
      JsObject(Seq(
        "kind" -> JsString("tree"),
        "node" -> node.toJsObject,
        "children" -> JsArray(children.map(_.toJsObject))
      ))
}

object Ast {
  def apply(node: String, children: Ast*): Ast =
    Ast(Richtext(node), children: _*)
}

case class ProofDiagram(node: Richtext, parents: Seq[ProofDiagram],
                        unificators: Seq[Richtext]) {
  def toJsObject: JsObject = JsObject(Seq(
    "kind" -> JsString("diagram"),
    "node" -> node.toJsObject,
    "parents" -> JsArray(parents.map(_.toJsObject)),
    "unificators" -> JsArray(unificators.map(_.toJsObject))
  ))
}
