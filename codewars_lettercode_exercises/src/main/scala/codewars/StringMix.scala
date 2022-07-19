package codewars

object StringMix {

  def makeSortedSeq(s: String, num: Int): Seq[(Char, (Int, Int))] = {
    s.trim.split(" ").mkString.filter(_.isLower)
      .toSeq.groupBy(identity).view.mapValues(_.unwrap)
      .filter(el => el._2.length > 1).view.mapValues(s => (s.length, num)).toMap
      .toSeq.sortWith(_._2._1 > _._2._1)
  }

  def makeGeneralSeq(m1: Seq[(Char, (Int, Int))], m2: Seq[(Char, (Int, Int))]) = {
    (m1 ++ m2)
      .groupMap(_._1)(_._2)
      .view
      .mapValues( l =>
        if (l.length > 1 && l.head._1 == l(1)._1) (l(1)._1, 3)
        else l.max
      )
      .toSeq
  }

  def mix(s1: String, s2: String): String = {
    makeGeneralSeq(makeSortedSeq(s1, 1), makeSortedSeq(s2, 2))
      .sortBy { case (a, (c, d)) =>
        (-c, d, a)
      }
      .map(el =>
        if (el._2._2 == 3)
          s"=:${Array.fill(el._2._1)(el._1).mkString("")}"
        else
          s"${el._2._2}:${Array.fill(el._2._1)(el._1).mkString("")}")
      .mkString("/")
  }
}
