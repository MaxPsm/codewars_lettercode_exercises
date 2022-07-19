package codewars.lvl6

object ReverseOrRotate {
  def rotateLeft(str: String): String = {
    val size = str.length
    str.drop(1) ++ str.take(1)
  }

  def sum(x: String, acc: Int): Int =
    x.map(_.asDigit).sum

  def goRevRot(str: String, acc: String, sz: Int): String = str match {
    case s if str.length >= sz =>
      val sub = str.take(sz)
      if (sum(sub, 0) % 2 == 0) goRevRot(str.drop(sz), acc + sub.reverse, sz)
      else goRevRot(str.drop(sz), acc + rotateLeft(sub), sz)
    case _ => acc
  }

  def revRot(strng: String, sz: Int): String = strng match {
    case str if sz > 0 && str.length >= sz && strng.nonEmpty =>
      goRevRot(strng, "", sz)
    case _ => ""
  }
}
