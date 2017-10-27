object Transformations {

  val outerCurlyBraceRemover: String => String = rawJson => {
    val removeFirst = rawJson.replaceFirst("\\{","")
    removeFirst.patch(removeFirst.lastIndexOf("}"),"",1)
  }

  val curlyBracketRemover : String => String = rawJson => {
    rawJson.filterNot(c => c == '{' || c == '}')
  }

  val quoteMarkRemover: String => String = rawJson => {
    rawJson.filterNot(_ == '\"')
  }

  val colonRemover: String => String = rawJson => {
    rawJson.replaceFirst(":","")
  }

  val splitAtFirstColon: String => (String, String) = rawJson => {
    rawJson.splitAt(rawJson.indexOf(":"))
  }

  val commaRemove: String => String = rawJson => {
    rawJson.replaceFirst(",","")
  }

  val sanitizedString: String => String = outerCurlyBraceRemover

  val squareBracketRemover: String => String = rawJson => {
    rawJson.replace("[", "").replace("]","")
  }
}
