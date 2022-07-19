package codewars.lvl6

object DecodeTheMorseCode {
  def decode(msg: String, morseDictionary: Map[String, String]): String =
    msg.trim().split(" {3}").map(s =>
      s.split(" ")).map( ss =>
      ss.map(str => morseDictionary(str)).mkString("")
    ).mkString(" ")
}
