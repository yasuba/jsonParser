import Transformations._
import Validations._

object JaycenParser {

  def parse[T](rawJson: String): Jaycen[T] = isValidJson(rawJson) match {
    case InvalidJaycen(_, error) => throw new Exception(error)
    case ValidJaycen(json) =>
      val keyValuePairs: List[String] = curlyBraceRemover(json).split(",").toList

      val rawJayObjects: List[(String, String)] = keyValuePairs.map(p => p.splitAt(p.indexOf(":")))

//      List(List("name", "Maya"))
//      List(("name","Maya"))
//      List(List("person", "name", "Maya"))
//      List(("person","name":"Maya"))

      rawJayObjects.map(pair => toJayObject(pair._1, toJayValue(pair._2)))
//
//      val loop: List[String] => JayValue = {
//        case h :: Nil => toJayValue(h)
//        case h :: t :: Nil => toJayObject(h -> toJayValue(t))
//        case k :: v :: t => toJayObject(k -> toJayObject(v -> loop(t)))
//        case _ => throw new Exception("There's nothing to parse")
//      }
//
//      val x: List[JayValue] = rawJayObjects.map(loop)

    Jaycen(List(JayObject(JayField("k"), JayInt(1))))
  }

  private def toJayObject[T](pairs: (String,JayValue)): JayObject =
    JayObject(JayField(quoteMarkRemover(pairs._1)), pairs._2)

  private def toJayValue(raw: String): JayValue = {
    if (raw.replaceFirst(":", "").exists(_ != ':')) {
      if (isArray(raw)) JayArray(squareBracketRemover(raw).split(",").toList)
      else {
        try {
          JayInt(raw.toInt)
        } catch {
          case _: NumberFormatException => try {
            JayBoolean(raw.toBoolean)
          }
          catch {
            case _: IllegalArgumentException => JayString(raw)
          }
        }
      }
    } else {
      val s = raw.splitAt(raw.indexOf(":"))
      toJayObject(s._1, toJayValue(s._2))
    }
  }
}
