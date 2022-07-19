package codewars.lvl6

object SumOfDigits {
  def go(n: Int, sum: Int): Int = n match {
    case num if num > 0 => go(n/10, sum + num % 10)
    case _ => sum
  }

  def digitalRoot(n: Int): Int = n match {
    case num if num >= 10 => digitalRoot(go(num, 0))
    case m => m
  }
}
