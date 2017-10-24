import org.scalatest.{FlatSpec, Matchers}
import JaycenParser._

class JaycenParserSpec extends FlatSpec with Matchers {

  val jsonString = """{"name":"Maya"}"""

  val noQuotesJson = """{name":Maya}"""

  val numberJson = """{"age":35}"""

  val booleanJson = """{"isBoolean":true}"""

  val multipleField = """{"name":"Maya","age":35}"""

  val arrayValue = """{"pets":["dog"]}"""

  val nestedObject = """{"person":{"name":"Maya"}}"""

  behavior of "Jaycen parser"

  it should "parse simple raw json"  in {
    parse(jsonString).get("name") shouldEqual "Maya"
  }

  it should "fail to parse json with no quote marks around key and value strings"  in {
    intercept[Exception] {
      parse(noQuotesJson).get("name")
    }
  }

  it should "parse ints as well as strings" in {
    parse(numberJson).get("age") shouldEqual 35
  }

  it should "parse booleans" in {
    parse(booleanJson).get("isBoolean") shouldEqual true
  }

  it should "allow for more than one field" in {
    parse(multipleField).get("age") shouldEqual 35
  }

  it should "parse values which are arrays" in {
    parse(arrayValue).get("pets") match {
      case List(v) => v shouldEqual "dog"
      case _ => fail
    }
  }

  it should "parse nested objects" in {
    parse(nestedObject).get("name") shouldEqual "Maya"
  }

}
