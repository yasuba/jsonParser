import Transformations._

object Validations {
  val curlyBraceChecker: String => JaysonValidateResult = {
    case rawJson if rawJson.head != '{' => InvalidJayson(rawJson, "Invalid jayson")
    case rawJson if rawJson.reverse.head != '}' => InvalidJayson(rawJson, "Invalid jayson")
    case rawJson => ValidJayson(rawJson)
  }

  val quoteMarkChecker: JaysonValidateResult => JaysonValidateResult = {
    case ValidJayson(rawJson) =>
      val pairs = curlyBraceRemover(rawJson).split(":").toList
      pairs match {
        case h :: _ if h.startsWith("\"") && h.reverse.startsWith("\"") => ValidJayson(rawJson)
        case _ => InvalidJayson(rawJson, "Key must be in quotes")
      }
    case parseFail => parseFail
  }

  def isArray(rawJson: String): Boolean =
    rawJson.head == '[' && rawJson.reverse.head == ']'

//  val keyValueChecker: JaysonValidateResult => JaysonValidateResult = {
//    case ValidJayson(rawJson) => {
//      val keysValues = rawJson.split(":").toList
//
//      keysValues.
//
//      keysValues.foreach(println(_))
//      ValidJayson(keysValues.toString)
//    }
//    case InvalidJayson(j,e) => InvalidJayson(j,e)
////    case ValidJayson(rawJson) => InvalidJayson(rawJson, "Jayson must consist of one key and one value")
//  }

  val isValidJson: String => JaysonValidateResult = {
    curlyBraceChecker andThen quoteMarkChecker
  }
}