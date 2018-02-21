import java.lang.reflect.Type

import JaycenModel.Jaycen
object JaycenSerializer {

  def toJaycen(o: Object): Jaycen = {
    val objectClass = o.getClass
    val keyNames = objectClass.getDeclaredFields.map(_.getName).toList

    println(s"keynames are ${keyNames}")

    val genericParameterTypes: List[Type] = objectClass.getConstructors.toList.flatMap(c => c.getGenericParameterTypes.toList)

    val valueTypes = genericParameterTypes.filter(t => t.toString.contains("String") || !t.toString.startsWith("class"))

    val values: List[String] = o.toString
      .dropWhile(_ != '(')
      .replaceAllLiterally("(","")
      .replaceAllLiterally(")","")
      .split(",")
      .toList


    val values1: String = o.toString.dropWhile(_ != '(').replaceAllLiterally("(","")
      .replaceAllLiterally(")","")

    if (values1.contains("List")) values1.split("List").toList.foreach(println) else  println(values1)


    val typedValues = valueTypes.zip(values)

    val allTheThings = keyNames.zip(typedValues)

    val properTypes = allTheThings.map{x =>
      val values = x._2._1.toString match {
        case "int" => x._2._2.toInt
        case "boolean" => x._2._2.toBoolean
        case _ => s""""${x._2._2}""""
      }

      s""""${x._1}":$values"""
    }

    val keyValuePairs: List[(String, String)] = keyNames.zip(values)

    val pairsToStrings = keyValuePairs.map{pair =>
      s""""${pair._1}":"${pair._2}""""
    }

    s"{${properTypes.mkString(",")}}"

    Jaycen(List())
  }
}
