package codewars.lvl6

object TakeATenMinutesWalk {
  def isValidWalk(walk: Seq[Char]): Boolean = {
    def goWalk(walk: String, time: Int, rightDir: Int, upDir: Int): Boolean = walk match {
      case "" if time != 0 => false
      case "" if time == 0 => rightDir == 0 && upDir == 0
      case _ if walk.head == 'n' => goWalk(walk.drop(1), time - 1, rightDir, upDir + 1)
      case _ if walk.head == 's' => goWalk(walk.drop(1), time - 1, rightDir, upDir - 1)
      case _ if walk.head == 'w' => goWalk(walk.drop(1), time - 1, rightDir - 1, upDir)
      case _ if walk.head == 'e' => goWalk(walk.drop(1), time - 1, rightDir + 1, upDir)
    }

    goWalk(walk.mkString(""), 10, 0, 0)
  }
}
