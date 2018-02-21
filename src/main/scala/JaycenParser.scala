import JaycenModel._
import Transformations._
import Validations._


object JaycenParser {

  def parse(rawJson: String): Jaycen = isValidJson(rawJson) match {
    case InvalidJaycen(_, error) => throw new Exception(error)
    case ValidJaycen(json) => {
      val keyValueMap: Map[String, String] = toKeyValueMap(rawJayObjects(keyValuePairs(json)))
      //Map("name" -> "Bob")
      val nestedJayObjects: List[JayObject] = nestedObjectsKeyValueMap(keyValueMap).map {
        case x => toNestedJayObject(x)
      }.toList
      //List()
      val simple = simpleJayObjects(simpleKeyValueObjectsMap(keyValueMap))
      Jaycen(simple ++ nestedJayObjects)
    }
  }

  def nestedObjectsKeyValueMap(kVMap: Map[String, String]): Map[String, Map[String, String]] =
    kVMap.collect{
      case s if s._2.contains('{') => {
        (s._1, toKeyValueMap(rawJayObjects(List(outerCurlyBraceRemover(s._2)))))
      }
    }

  val keyValuePairs: String => List[String] = json => {
    val stripCurlies = outerCurlyBraceRemover(json)

    def loop(raw: String, acc: List[String]): List[String] = {
      if (isMultiField(raw)) {
        val firstPair = raw.substring(raw.indexOf('"'), raw.indexOf(","))
        commaRemove(raw.diff(firstPair)) match {
          case sub if sub.contains('[') => {
            loop(sub, firstPair :: acc)
          }
          case sub => List(firstPair, commaRemove(sub))
        }
      } else raw :: acc
    }
    loop(stripCurlies, List())
  }

  val rawJayObjects: List[String] => List[(String, String)] = kVPairs =>
    kVPairs.map(p => splitAtFirstColon(p))

  def toKeyValueMap(rawJayObjects: List[(String, String)]): Map[String, String] =
    rawJayObjects.map { case (k,v) => k -> colonRemover(v)}.toMap

  val simpleJayObjects: Map[String, String] => List[JayObject] = simpleMap =>
    simpleMap
      .map(keyValueObject => {
        toSimpleJayObject(quoteMarkRemover(keyValueObject._1), curlyBracketRemover(quoteMarkRemover(keyValueObject._2)))
      })
      .toList

  def simpleKeyValueObjectsMap(keyValueMap: Map[String, String]): Map[String, String] =
    keyValueMap.filterNot(s => s._2.contains(':'))


  def toSimpleJayObject(pairs: (String, String)): JayObject =
    SimpleObject(JayField(quoteMarkRemover(pairs._1)), toJayValue(colonRemover(pairs._2)))

  def toNestedJayObject(raw: (String, Map[String, String])): JayObject =
    NestedObject(JayField(quoteMarkRemover(raw._1)), simpleJayObjects(simpleKeyValueObjectsMap(raw._2)))

}
