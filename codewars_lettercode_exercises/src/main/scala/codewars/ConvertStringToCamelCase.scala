package codewars

object ConvertStringToCamelCase {
  def toCamelCase(str: String): String = str match {
    case _ if str.contains('_') =>
      str.split("_").head + str.split("_").tail.map(x => x.head.toUpper + x.tail).mkString("")
    case _ if str.contains('-') =>
      str.split("-").head + str.split("-").tail.map(x => x.head.toUpper + x.tail).mkString("")
    case s => s
  }
}
