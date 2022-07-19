package codewars.lvl5

object NumberOfTrailingZeros {
  def del5(n: Int, count: Int): Int = n match {
    case a if a > 0 && a % 5 == 0 => del5(n / 5, count + 1)
    case _ => count
  }

  def go(n: Int, fives: Int): Int = (n, fives) match{
    case (a, five) if a > 0 && a % 5 == 0 =>
      val fivesInA = del5(a, 0)
      go(n - 1, five + fivesInA)

    case (a, f) if a > 0 => go(n - n % 5, f)

    case _ => fives
  }

  def zeros(n: Int): Int = {
    go(n, 0)
  }
}
