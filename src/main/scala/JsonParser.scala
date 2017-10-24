import Transformations._
import Validations._

sealed trait JaysonValidateResult

case class JayObject(field: JayField, value: JayValue)

case class Jayson[T](fields: List[JayObject]) {
  def get(key: String) = {
    val matchedField = fields.find(_.field.key == key)
    matchedField match {
      case Some(JayObject(_, str@JayString(_))) => str
      case Some(JayObject(_, int@JayInt(_))) => int
      case Some(JayObject(_, bool@JayBoolean(_))) => bool
      case Some(JayObject(_, array@JayArray(_))) => array
      case None => throw new Exception("invalid jayson")
    }
  }
}

case class JayField(key: String)

sealed trait JayValue

object JayValue {
  def toJayValue(raw: String): JayValue =
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

case class JayString(value: String) extends JayValue
case class JayInt(value: Int) extends JayValue
case class JayBoolean(value: Boolean) extends JayValue
case class JayArray[T](value: List[T]) extends JayValue

case class ValidJayson(rawJson: String) extends JaysonValidateResult
case class InvalidJayson(rawJson: String, error: String) extends JaysonValidateResult



object JsonParser {

  def parse[T](rawJson: String): Jayson[T] =
    isValidJson(rawJson) match {
      case InvalidJayson(_, error) => throw new Exception(error)
      case ValidJayson(json) =>
        val pairs: List[String] = sanitizedString(json).split(",").toList
        Jayson(toJaysonFields(pairs))
    }

}
