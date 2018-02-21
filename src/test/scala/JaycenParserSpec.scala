import org.scalatest.{FlatSpec, Matchers}
import JaycenParser._

class JaycenParserSpec extends FlatSpec with Matchers {

  val jsonString = """{"name":"Maya"}"""

  val noQuotesJson = """{name":Maya}"""

  val numberJson = """{"age":35}"""

  val booleanJson = """{"isBoolean":true}"""

  val multipleField = """{"name":"Maya","age":35}"""

  val arrayValue = """{"pets":["dog"]}"""

  val intArrayValue = """{"hi":"there","faveNumbers":[3,5]}"""

  val nestedObject = """{"person":{"name":"Maya"}}"""

  val arrayMix = """{"pets":["dog"], "food":"steak"}"""

  val bigJaycen = """{"person":{"name":"Maya","age":35,"pets":["dog","cat"],"job":{"company":"ITV","title":"Scala Developer"}}}"""

  behavior of "Jaycen deserializer"

  it should "parse simple raw json"  in {
    parse(jsonString).get[String]("name") shouldEqual "Maya"
  }
//
//  it should "fail to parse json with no quote marks around key and value strings"  in {
//    intercept[Exception] {
//      parse(noQuotesJson).get[String]("name")
//    }
//  }
//
//  it should "parse ints as well as strings" in {
//    parse(numberJson).get[Int]("age") shouldEqual 35
//  }
//
//  it should "parse booleans" in {
//    parse(booleanJson).get[Boolean]("isBoolean") shouldEqual true
//  }
//
//  it should "allow for more than one field" in {
//    parse(multipleField).get[Int]("age") shouldEqual 35
//  }
//
//  it should "parse values which are arrays" in {
//    parse(arrayValue).get[List[String]]("pets") match {
//      case List(v) => v shouldEqual "dog"
//      case _ => fail
//    }
//  }
//
//  it should "parser array values that contain Ints" in {
//    parse(intArrayValue).get[List[Boolean]]("faveNumbers")  shouldEqual List("3", "5")
//  }
//
//  it should "parse nested objects" in {
//    parse(nestedObject).get[String]("name") shouldEqual "Maya"
//  }
//
//  it should "parse a big json!" in {
//    parse(bigJaycen).get[String]("title") shouldEqual "Scala Developer"
//  }

//  it should "parse arrays when other objects follow" in {
//    parse(arrayMix).get[String]("food") shouldEqual "steak"
//  }

}
