package codewars.lvl6

object YourOrderPlease {
  def construct(str: String): String = {
    import scala.collection.mutable

    val buff = mutable.Map[Int, String]()
    str.split(" ").map { word =>
      word.map { char =>
        if (char.isDigit) buff.addOne(char.asDigit, word)
      }
    }

    buff.values.mkString(" ")
  }

  def order(str: String): String = str match {
    case "" => ""
    case _ => construct(str)
  }
}
