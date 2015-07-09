package visual

import play.api.libs.json._

sealed trait TextStyle;
case class Upright() extends TextStyle;
case class Italic() extends TextStyle;
case class Subscript() extends TextStyle;

object Visual {
  def jsRichtext(fragments: (String, TextStyle)*): JsObject =
    JsObject(Seq(
      "kind" -> JsString("richtext"),
      "contents" -> JsArray(fragments.map{ case (text, style) =>
        JsObject(Seq(
          "text" -> JsString(text),
          "italic" -> JsBoolean(style == Italic()),
          "subscript" -> JsBoolean(style == Subscript())
        ))
      })
    ))

  def jsText(text: String, style: TextStyle = Upright()): JsObject =
    jsRichtext((text, style))

  def jsTree(node: String, children: JsObject*): JsObject =
    JsObject(Seq(
      "kind" -> JsString("tree"),
      "node" -> jsText(node),
      "children" -> JsArray(children)
    ))
}
