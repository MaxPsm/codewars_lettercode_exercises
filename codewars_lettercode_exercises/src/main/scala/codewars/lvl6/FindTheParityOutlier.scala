package codewars.lvl6

object FindTheParityOutlier {
  import scala.math.abs

  def findOutlier(integers: List[Int]): Int = {
    if (integers.count(abs(_) % 2 == 0) == 1)
      integers.filter(abs(_) % 2 == 0).head
    else
      integers.filter(abs(_) % 2 == 1).head
  }
}
