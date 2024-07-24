package lettercode.Medium

object NumberOfSubstringsWithOnly1s {

  object Solution {
    val MOD = 1_000_000_007
    def numSub(s: String): Int = {
      def calculateArithmeticProgression(n: Int): Long = ((n + 1L) * n / 2L) % MOD

      val r = s.foldLeft(0L, 0){
        (res, c) =>
          if (c == '0')
            (res._1 + calculateArithmeticProgression(res._2), 0)
          else (res._1, res._2 + 1)
      }
      (r._1 % MOD + calculateArithmeticProgression(r._2)).toInt
    }
  }

}
