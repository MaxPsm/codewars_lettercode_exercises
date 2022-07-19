package codewars.lvl3

object FabergeEasterEggs {
  import java.math.BigInteger
  import java.math.BigInteger.valueOf
  import scala.math.BigInt.javaBigInteger2bigInt

  object Faberge {
    def height(n: BigInteger, m: BigInteger): BigInteger = {
      var mCoef: BigInteger = m
      var h: BigInteger = BigInteger.ZERO
      var k: BigInteger = BigInteger.ONE
      for (i <- 1 to n.toInt) {
        k = k.multiply(mCoef).divide(valueOf(i))
        h = h.add(k)
        mCoef = mCoef.subtract(valueOf(1))
      }
      h
    }
  }
}
