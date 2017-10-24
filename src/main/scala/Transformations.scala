object Transformations {

  val curlyBraceRemover: String => String = rawJson => {
    rawJson.filterNot(c => c == '{' || c == '}')
  }

  val quoteMarkRemover: String => String = rawJson => {
    rawJson.filterNot(_ == '\"')
  }

  val sanitizedString: String => String = curlyBraceRemover

  val squareBracketRemover: String => String = rawJson => {
    rawJson.replace("[", "").replace("]","")
  }
}
