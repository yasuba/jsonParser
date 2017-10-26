case class Jaycen[T](fields: List[JayObject]) {
  def get(key: String): Any = {
    val matchedField = fields.find(_.field.key == key)
    matchedField match {
      case Some(JayObject(_, JayString(str))) => str
      case Some(JayObject(_, JayInt(int))) => int
      case Some(JayObject(_, JayBoolean(bool))) => bool
      case Some(JayObject(_, JayArray(array))) => array
      case None => throw new Exception("invalid jaycen")
    }
  }
}

sealed trait JayValue
case class JayString(value: String) extends JayValue
case class JayInt(value: Int) extends JayValue
case class JayBoolean(value: Boolean) extends JayValue
case class JayArray[T](value: List[T]) extends JayValue
case class JayField(key: String)
case class JayObject(field: JayField, value: JayObject)
