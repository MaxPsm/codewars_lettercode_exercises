package codewars.lvl6

object DuplicateEncoder {
  def duplicateEncode(word: String): String = word match {
      case "" => ""
      case w =>
        val charCounter = word.groupBy(c => c.toLower).view.mapValues(str => str.length)
        w.foldLeft("")((acc, c) => if(charCounter(c.toLower) > 1) acc + ")" else acc + "(")
  }

  def solution2(word: String): String = {
    val wordLower = word.toLowerCase
    val charCounts = wordLower.groupBy(identity).view.mapValues(_.length)
    wordLower.map(c => if (charCounts(c) > 1) ')' else '(')
  }
}
