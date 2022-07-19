package codewars.lvl6

object BouncingBalls {
  def goBall(h: Double, bounce: Double, window: Double, count: Int): Int = {
    (h, bounce, window) match {
      case (ht, b, w) if w < b * ht => goBall(ht * b, b, w, count + 2)
      case _ => count
    }
  }

  def bouncingBall(h: Double, bounce: Double, window: Double): Int = {
    if (h > 0 && bounce > 0 && bounce < 1 && window < h)
      goBall(h, bounce, window, 1)
    else -1
  }
}
