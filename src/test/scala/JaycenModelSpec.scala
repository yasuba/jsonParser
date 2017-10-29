import JaycenModel._
import org.scalatest.{FlatSpec, Matchers}

class JaycenModelSpec extends FlatSpec with Matchers {

  it should "get int" in {
    val json = Jaycen(List(SimpleObject(JayField("int"), JayInt(1))))
    json.get[Int]("int") shouldEqual 1
  }

  it should "get booleans" in {
    val json = Jaycen(List(SimpleObject(JayField("bool"), JayBoolean(true))))
    json.get[Boolean]("bool") shouldEqual true
  }

  it should "get arrays" in {
    val json = Jaycen(List(SimpleObject(JayField("list"), JayArray(List(true)))))
    json.get[List[Boolean]]("list") shouldEqual List(true)
  }

}
