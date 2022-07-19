package codewars.lvl5

object RgbToHexConversion {
  def rgb(r: Int, g: Int, b: Int): String = {
    Seq(r, g, b).map{ x =>
      if (x < 0) 0
      else if (x > 255) 255
      else x
    }.map { y =>
      if (y < 16) s"0${y.toHexString.toUpperCase}"
      else y.toHexString.toUpperCase
    }.mkString("")
  }
}
