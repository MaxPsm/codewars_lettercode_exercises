package lettercode.Easy

object ClimbingStairs {
  def climbStairs(n: Int): Int = {
    var one = 1
    var two = 1
    for (i <- 1 until n) {
      val temp = one
      one = one + two
      two = temp
    }
    one
  }
}
