case class Jaycen(fields: List[JayObject]) {
  def get(key: String): Any = {
    def loop(jObjs: List[JayObject]): Any = {
      matchedField(key)(jObjs) match {
        case Some (SimpleObject (_, JayString (str) ) ) => str
        case Some (SimpleObject (_, JayInt (int) ) ) => int
        case Some (SimpleObject (_, JayBoolean (bool) ) ) => bool
        case Some (SimpleObject (_, JayArray (array) ) ) => array
        case Some (NestedObject (_, objects) ) => loop(objects)
        case None => throw new Exception ("invalid jaycen")
      }
    }
    loop(fields)
  }

  val matchedField: String => List[JayObject] => Option[JayObject] = key => fields =>
    fields.find{
      case NestedObject(_, List(SimpleObject(field, _))) => field.key == key
      case SimpleObject(field, _) => field.key == key
      case NestedObject(field, _) => field.key == key
      case _ => throw new Exception("Cannot get from empty Jaycen")
    }
}

sealed trait JayValue
case class JayString(value: String) extends JayValue
case class JayInt(value: Int) extends JayValue
case class JayBoolean(value: Boolean) extends JayValue
case class JayArray[T](value: List[T]) extends JayValue
case class JayField(key: String)

sealed trait JayObject
case class SimpleObject(field: JayField, value: JayValue) extends  JayObject
case class NestedObject(field: JayField, value: List[JayObject]) extends JayObject
