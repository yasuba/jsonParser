import Transformations._
import Validations._

object JaycenParser {

  def parse[T](rawJson: String) =
    isValidJson(rawJson) match {
      case InvalidJaycen(_, error) => throw new Exception(error)
      case ValidJaycen(json) =>
        val keyValuePairs: List[String] = curlyBraceRemover(json).split(",").toList

        val rawJayObjects: List[List[String]] = keyValuePairs.map(p => p.split(":").toList)

        def keyValueMap(rawObjects: List[List[String]]): List[List[JayObject]] = rawObjects match {
          case k :: v :: Nil => {
            println(toJaycenFields(Map(k -> v)))
            toJaycenFields(Map(k -> v))
          }
//          case k :: t => k -> keyValueMap(List(t))
        }


        Jaycen(List(JayObject(JayField("k"), JayInt(1))))
//        Jaycen(keyValueMap(rawJayObjects))
    }

  private def toJaycenFields[T](pairs: Map[String,String]): List[JayObject] = {
    pairs.map { pair =>
      JayObject(JayField(quoteMarkRemover(pair._1)), toJayValue(quoteMarkRemover(pair._2)))
    }.toList
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
