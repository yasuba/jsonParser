import org.scalatest.{FlatSpec, Matchers}
import JaycenSerializer._

class JaysenSerializerSpec extends FlatSpec with Matchers {

  behavior of "Jaycen serializer"

  it should "parse a simple case class in to Jaycen" in {
    case class PersonAge(age: Int)
    toJaycen(PersonAge(5)) shouldEqual """{"age":5}"""
    case class PersonName(name: String)
    toJaycen(PersonName("Bob")) shouldEqual """{"name":"Bob"}"""
    case class PersonBool(happy: Boolean)
    toJaycen(PersonBool(true)) shouldEqual """{"happy":true}"""
  }

  it should "parse a harder case class" in {
    case class HarderClass(name: String, age: Int)
    toJaycen(HarderClass("Bob", 5)) shouldEqual """{"name":"Bob","age":5}"""
  }

  it should "parse a class with a list in its parameters" in {
    case class Person(pets: List[String])
    toJaycen(Person(List("dog", "cat"))) shouldEqual """{"pets":["dog","cat"]}"""
  }

}
