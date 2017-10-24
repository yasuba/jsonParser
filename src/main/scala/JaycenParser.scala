import Transformations._
import Validations._

object JaycenParser {

  def parse[T](rawJson: String): Jaycen[T] =
    isValidJson(rawJson) match {
      case InvalidJaycen(_, error) => throw new Exception(error)
      case ValidJaycen(json) =>
        val keyValuePairs: List[String] = sanitizedString(json).split(",").toList

        println(keyValuePairs.map(p => p.split(":").toList))

        Jaycen(toJaycenFields(keyValuePairs))
    }

  private def toJaycenFields[T](pairs: List[String]): List[JayObject] =
    pairs.flatMap{ pair =>
      pair.split(":").toList match {
        case h :: m :: t => JayObject(JayField(quoteMarkRemover(h)), toJayValue(quoteMarkRemover(m))) :: toJaycenFields(t)
        case _ => throw new Exception("invalid jaycen")
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
