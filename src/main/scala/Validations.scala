import Transformations._

sealed trait JaycenValidateResult
case class ValidJaycen(rawJson: String) extends JaycenValidateResult
case class InvalidJaycen(rawJson: String, error: String) extends JaycenValidateResult

object Validations {
  val curlyBraceChecker: String => JaycenValidateResult = {
    case rawJson if rawJson.head != '{' => InvalidJaycen(rawJson, "Invalid jaycen")
    case rawJson if rawJson.reverse.head != '}' => InvalidJaycen(rawJson, "Invalid jaycen")
    case rawJson => ValidJaycen(rawJson)
  }

  val quoteMarkChecker: JaycenValidateResult => JaycenValidateResult = {
    case ValidJaycen(rawJson) =>
      val pairs = outerCurlyBraceRemover(rawJson).split(":").toList
      pairs match {
        case h :: _ if h.startsWith("\"") && h.reverse.startsWith("\"") => ValidJaycen(rawJson)
        case _ => InvalidJaycen(rawJson, "Key must be in quotes")
      }
    case parseFail => parseFail
  }

  val isMultiField: String => Boolean = string => string.contains(",\"")

  def isArray(rawJson: String): Boolean =
    rawJson.head == '[' && rawJson.reverse.head == ']'

  val isValidJson: String => JaycenValidateResult = {
    curlyBraceChecker andThen quoteMarkChecker
  }
}