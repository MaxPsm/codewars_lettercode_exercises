package codewars.lvl3

import java.util.Random

object Psychic {
  def guess(): Double = random.nextDouble

  private val seed: Long = System.currentTimeMillis
  private val random = new Random(seed)

  private def overrideJavaRandomClass: Unit = {
    val randomGeneratorClass = Class.forName("java.lang.Math$RandomNumberGeneratorHolder")
    val randomNumberGeneratorMethod = randomGeneratorClass.getDeclaredField("randomNumberGenerator")
    randomNumberGeneratorMethod.setAccessible(true)

    val fakeRandomNumberGeneratorMethod = randomNumberGeneratorMethod.get(null).asInstanceOf[Random]
    fakeRandomNumberGeneratorMethod.setSeed(seed)
  }

  overrideJavaRandomClass
}