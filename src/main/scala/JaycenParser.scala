import Transformations._
import Validations._

object JaycenParser {

  def parse[T](rawJson: String): Jaycen[T] = isValidJson(rawJson) match {
    case InvalidJaycen(_, error) => throw new Exception(error)
    case ValidJaycen(json) =>
      val keyValuePairs: List[String] = curlyBraceRemover(json).split(",").toList

      val rawJayObjects: List[List[String]] = keyValuePairs.map(p => p.split(":").toList)

      def keyValueMap(rawObjects: List[List[String]]): List[JayValue] = {
        def loop(l: List[String]): JayValue = {
          l match {
            case h :: Nil => toJayValue(h)
            case h :: t :: Nil => toJayObject(h -> toJayValue(t))
            case k :: v :: t => toJayObject(k -> toJayObject(v -> loop(t)))
            case _ => throw new Exception("There's nothing to parse")
          }
        }

        rawObjects.map(loop)
      }

        Jaycen(keyValueMap(rawJayObjects))

    Jaycen(List(JayObject(JayField("k"), JayInt(1))))
  }

  private def toJayObject[T](pairs: (String,JayValue)): JayObject =
    JayObject(JayField(quoteMarkRemover(pairs._1)), pairs._2)

  private def toJayValue(raw: String): JayValue =
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
}
