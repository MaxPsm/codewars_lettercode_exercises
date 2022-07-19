package codewars

object RectangleRotation {
  def rectangleRotation(a: Int, b: Int): Int = {
    val w = (a / Math.sqrt(2)).toInt
    val h = (b / Math.sqrt(2)).toInt

    if ((w + h) % 2 == 1)
      (w + 1) * h + w * (h + 1)
    else w * h + (w + 1) * (h + 1)
  }
}
