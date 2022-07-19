package codewars

object PrimesInNumbers {
  def goFactors(base: Int, n: Int, currStr: String, currValue: Int, currCount: Int, isPrime: Boolean): String = n match {
    case _ if currValue == Math.sqrt(base).toInt && n > 1 =>
      println(n)
      if (isPrime) s"($base)"
      else if (currCount == 0) currStr + s"($n)"
      else if (currCount == 1) currStr + s"($currValue)($n)"
      else currStr + s"($currValue**$currCount)($n)"
    case _ if currValue == Math.sqrt(base).toInt || n == 1 =>
      if (isPrime) s"($base)"
      else if (currCount == 0) currStr
      else if (currCount == 1) currStr + s"($currValue)"
      else currStr + s"($currValue**$currCount)"
    case _ if n % currValue == 0 => goFactors(base, n / currValue, currStr, currValue, currCount + 1, isPrime = false)
    case _ =>
      if (currCount >= 2) goFactors(base, n, currStr + s"($currValue**$currCount)", currValue + 1, 0, isPrime = false)
      else if (currCount == 1) goFactors(base, n, currStr + s"($currValue)", currValue + 1, 0, isPrime = false)
      else goFactors(base, n, currStr, currValue + 1, 0, isPrime)
  }

  def factors(m: Int): String = m match{
    case _ if Range(0, 3).contains(m) => s"($m)"
    case _ => goFactors(m, m, "", 2, 0, true)
  }
}
