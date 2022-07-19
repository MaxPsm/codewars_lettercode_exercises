package codewars.lvl4

object RangeExtraction {
  def findSubstring(xs: List[Int], startValue: Int, endValue: Int, acc: List[String]): String = xs match {
    case h :: t if h - endValue == 1 => findSubstring(t, startValue, endValue + 1, acc)
    case h :: t if h - endValue != 1 =>
      if (endValue - startValue >= 2) findSubstring(t, h, h, s"$startValue-$endValue" :: acc)
      else if (startValue != endValue) findSubstring(t, h, h, s"$endValue" :: s"$startValue" :: acc)
      else findSubstring(t, h, h, s"$startValue" :: acc)
    case Nil =>
      if (endValue - startValue >= 2) (s"$startValue-$endValue" :: acc).reverse.mkString(",")
      else if (startValue != endValue) (s"$endValue" :: s"$startValue" :: acc).reverse.mkString(",")
      else (s"$startValue" :: acc).reverse.mkString(",")
  }

  def solution(xs: List[Int]): String = xs match {
    case Nil => ""
    case h :: t if t.isEmpty => s"$h"
    case h :: t => findSubstring(t, h, h, List.empty)
  }
}
