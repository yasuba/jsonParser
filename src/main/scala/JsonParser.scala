import Transformations._
import Validations._

object JsonParser {

  def parse[T](rawJson: String): Jaycen[T] =
    isValidJson(rawJson) match {
      case InvalidJaycen(_, error) => throw new Exception(error)
      case ValidJaycen(json) =>
        val pairs: List[String] = sanitizedString(json).split(",").toList
        Jaycen(toJaycenFields(pairs))
    }

  private def toJaycenFields[T](pairs: List[String]): List[JayObject] =
    pairs.flatMap{ pair =>
      pair.split(":").toList match {
        case h :: m :: t => JayObject(JayField(h), toJayValue(m)) :: toJaycenFields(t)
        case _ => throw new Exception("invalid jayson")
      }
    }

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
