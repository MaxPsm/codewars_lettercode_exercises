package lettercode.Hard

object ConsecutiveNumbersSum {
  def consecutiveNumbersSum(n: Int): Int = {
    var res = 0
    var num = 1
    while(num * (num + 1) / 2 <= n) {
      if((n - num * (num + 1) / 2) % num == 0)
        res = res + 1
      num = num + 1
    }
    res
  }
}

object Main extends App {
  println(ConsecutiveNumbersSum.consecutiveNumbersSum(79932))
}
