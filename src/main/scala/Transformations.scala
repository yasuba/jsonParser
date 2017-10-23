import JayValue._

object Transformations {

  val curlyBraceRemover: String => String = rawJson => {
    rawJson.filterNot(c => c == '{' || c == '}')
  }

  val quoteMarkRemover: String => String = rawJson => {
    rawJson.filterNot(_ == '\"')
  }

  val sanitizedString: String => String = curlyBraceRemover andThen quoteMarkRemover

  val squareBracketRemover: String => String = rawJson => {
    rawJson.replace("[", "").replace("]","")
  }

  def toJaysonFields[T](pairs: List[String]): List[JayObject[T]] =
    pairs.flatMap{ pair =>
      pair.split(":").toList match {
        case h :: m :: t => JayObject(h, m) :: toJaysonFields(t)
        case _ => throw new Exception("invalid jayson")
      }
    }
}
