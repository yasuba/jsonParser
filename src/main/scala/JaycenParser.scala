import Transformations._
import Validations._


object JaycenParser {

  def parse(rawJson: String): Jaycen = isValidJson(rawJson) match {
    case InvalidJaycen(_, error) => throw new Exception(error)
    case ValidJaycen(json) => {
      val keyValueMap: Map[String, String] = toKeyValueMap(rawJayObjects(keyValuePairs(json)))
      val nestedJayObjects: List[JayObject] = nestedObjectsKeyValueMap(keyValueMap).map {
        case x => toNestedJayObject(x)
      }.toList
      val simple = simpleJayObjects(simpleKeyValueObjectsMap(keyValueMap))
      Jaycen(simple ++ nestedJayObjects)
    }
  }

  private def nestedObjectsKeyValueMap(kVMap: Map[String, String]): Map[String, Map[String, String]] =
    kVMap.collect{
      case s if s._2.contains('{') => {
        (s._1, toKeyValueMap(rawJayObjects(List(outerCurlyBraceRemover(s._2)))))
      }
    }

  private val keyValuePairs: String => List[String] = json => outerCurlyBraceRemover(json).split(",").toList

  private val rawJayObjects: List[String] => List[(String, String)] = kVPairs =>
    kVPairs.map(p => splitAtFirstColon(p))

  private def toKeyValueMap(rawJayObjects: List[(String, String)]): Map[String, String] =
    rawJayObjects.map { case (k,v) => k -> colonRemover(v)}.toMap

  private val simpleJayObjects: Map[String, String] => List[JayObject] = simpleMap =>
    simpleMap
      .map(keyValueObject => toSimpleJayObject(quoteMarkRemover(keyValueObject._1), curlyBracketRemover(quoteMarkRemover(keyValueObject._2))))
      .toList

  private def simpleKeyValueObjectsMap(keyValueMap: Map[String, String]): Map[String, String] =
    keyValueMap.filterNot(s => s._2.contains(':'))


  private def toSimpleJayObject(pairs: (String, String)): JayObject =
    SimpleObject(JayField(quoteMarkRemover(pairs._1)), toJayValue(colonRemover(pairs._2)))

  private def toNestedJayObject(raw: (String, Map[String, String])): JayObject =
    NestedObject(JayField(quoteMarkRemover(raw._1)), simpleJayObjects(simpleKeyValueObjectsMap(raw._2)))


  private def toJayValue(raw: String): JayValue = {
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
}
