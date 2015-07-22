package visual

import play.api.libs.json._

sealed trait TextStyle {
  def toJsString: JsString
}
case object Upright extends TextStyle {
  def toJsString = JsString("upright")
}
case object Italic extends TextStyle {
  def toJsString = JsString("italic")
}
case object Middle extends TextStyle {
  def toJsString = JsString("middle")
}
case object Subscript extends TextStyle {
  def toJsString = JsString("subscript")
}

case class RichtextFragment(text: String, style: TextStyle)

case class Richtext(fragments: Vector[RichtextFragment]) {
  def toJSON: JsArray =
    JsArray(fragments.map{ case RichtextFragment(text, style) =>
      JsObject(Seq(
        "text" -> JsString(text),
        "style" -> style.toJsString
      ))
    })
  def +(other: Richtext): Richtext = Richtext(this.fragments ++ other.fragments)
  def mkString: String = fragments.map(_.text).mkString
}

object Richtext {
  def apply(text: String, style: TextStyle = Upright): Richtext =
    Richtext(Vector(RichtextFragment(text, style)))
}

case class Ast(node: Richtext, children: Ast*) {
  def toJSON: JsObject = JsObject(Seq(
    "node" -> node.toJSON,
    "children" -> JsArray(children.map(_.toJSON))
  ))
}

object Ast {
  def apply(node: String, children: Ast*): Ast =
    Ast(Richtext(node), children: _*)
}

case class ProofDiagram(typedExpr: Richtext, rule: Richtext,
                        parents: Seq[ProofDiagram]) {
  def toJSON: JsObject = JsObject(Seq(
    "typedExpr" -> typedExpr.toJSON,
    "rule" -> rule.toJSON,
    "parents" -> JsArray(parents.map(_.toJSON))
  ))
}

case class Equation(lhs: Richtext, rhs: Richtext) {
  def toJSON: JsObject = JsObject(Seq(
    "lhs" -> lhs.toJSON,
    "eq" -> Richtext("=").toJSON,
    "rhs" -> rhs.toJSON
  ))
}

case class Equations(eqs: Seq[Equation]) {
  def toJSON: JsArray = JsArray(eqs.map(_.toJSON))
}
