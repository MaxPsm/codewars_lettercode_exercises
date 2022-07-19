package codewars.lvl6

object StopSpinningMyWords {
  def spinWords(sentence: String): String = {
    sentence.split(" ").map(w =>
      if(w.length() >= 5) w.reverse
      else w
    ).mkString(" ")
  }
}
