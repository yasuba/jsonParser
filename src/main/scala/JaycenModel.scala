import Transformations._
import Validations.isArray
import scala.reflect.runtime.universe._

object JaycenModel {

  case class Jaycen(fields: List[JayObject]) {
    def get[T](key: String)(implicit as: TypeTag[T]): Any = {
      def loop(jObjs: List[JayObject]): Any = {
        matchedField(key)(jObjs) match {
          case Some(SimpleObject(_, value)) => as.tpe match {
            case s if s =:= typeOf[String] => value match {
              case JayString(x) => x
              case _ => throw new Exception(s"Could not extract value as ${s.toString}")
            }
            case i if i =:= typeOf[Int] => {
              value match {
                case JayInt(x) => x
                case _ => throw new Exception(s"Could not extract value as ${i.toString}")
              }
            }
            case b if b =:= typeOf[Boolean] => value match {
              case JayBoolean(x) => x
              case _ => throw new Exception(s"Could not extract value as ${b.toString}")
            }
            case a if a =:= typeOf[List[Int]] => value match {
              case JayArray(x) => x
              case s => throw new Exception(s"Could not extract value as ${a.toString}")
            }
            case a if a =:= typeOf[List[String]] => value match {
              case JayArray(x) => x
              case s => throw new Exception(s"Could not extract value as ${a.toString}")
            }
            case a if a =:= typeOf[List[Boolean]] => value match {
              case JayArray(x) => x
              case s => throw new Exception(s"Could not extract value as ${a.toString}")
            }
            case x => throw new Exception(s"Cannot extract value as ${x.toString}")
          }
          case Some(NestedObject(_, objects)) => loop(objects)
          case None => throw new Exception("not a match")
        }
      }
      loop(fields)
    }

//    def get(key: String): Any = {
//      def loop(jObjs: List[JayObject]): Any = {
//        matchedField(key)(jObjs) match {
//          case Some(SimpleObject(_, value)) => value match {
//            case JayString(str) => str
//            case JayInt(int) => int
//            case JayBoolean(bool) => bool
//            case JayArray(array) => array
//          }
//          case Some(NestedObject(_, objects)) => loop(objects)
//          case None => throw new Exception("invalid jaycen")
//        }
//      }
//      loop(fields)
//    }
  }

  val matchedField: String => List[JayObject] => Option[JayObject] = key => fields =>
    fields.find {
      case NestedObject(_, List(SimpleObject(field, _))) => field.key == key
      case SimpleObject(field, _) => field.key == key
//      case NestedObject(field, _) => field.key == key
      case x => throw new Exception(x.toString)
    }

  sealed trait JayValue
  case class JayString(value: String) extends JayValue
  case class JayInt(value: Int) extends JayValue
  case class JayBoolean(value: Boolean) extends JayValue
  case class JayArray[T](value: List[T]) extends JayValue

  case class JayField(key: String)

  sealed trait JayObject
  case class SimpleObject(field: JayField, value: JayValue) extends JayObject
  case class NestedObject(field: JayField, value: List[JayObject]) extends JayObject

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
