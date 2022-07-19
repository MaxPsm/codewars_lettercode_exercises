package codewars.lvl5

object MeanSquareError {
  def solution(a: Array[Int], b: Array[Int]): Double = {
    a.foldLeft((0, Array[Int]())){
      case ((ind, res), el) =>
        (ind + 1, res :+ math.pow(el - b(ind), 2).toInt)
    }._2.foldLeft(0.0)(_ + _) / a.length
  }

  def solution2(a: List[Int], b: List[Int]): Double =
    a.lazyZip(b).map{ case (x, y) => math.pow(x - y, 2) }.sum / a.length

  def solution3(a: Array[Int], b: Array[Int]): Double =
    a.zip(b).map { case (x, y) => math.pow(x-y, 2) }.sum / a.length
}
