package lettercode.Easy

object MinCostClimbingStairs {
  def minCostClimbingStairs(cost: Array[Int]): Int = {
    var one = 0
    var two = 0

    for (i <- cost.indices) {
      val temp = cost(i) + Math.min(one, two)
      two = one
      one = temp
    }

    Math.min(one, two)
  }
}
