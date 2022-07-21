package codewars.lvl3

object ScreenLockingPattern {
  val nearestNeighbours: Map[Char, Seq[Char]] = Map(
    ('A', Seq('B', 'D', 'E', 'F', 'H')),
    ('B', Seq('A', 'C', 'E', 'D', 'F', 'G', 'I')),
    ('C', Seq('B', 'F', 'E', 'D', 'H')),
    ('D', Seq('A', 'G', 'E', 'B', 'H', 'C', 'I')),
    ('E', Seq('A', 'B', 'C', 'D', 'F', 'G', 'H', 'I')),
    ('F', Seq('C', 'I', 'E', 'B', 'H', 'G', 'A')),
    ('G', Seq('D', 'H', 'E', 'F', 'B')),
    ('H', Seq('G', 'I', 'E', 'D', 'F', 'C', 'A')),
    ('I', Seq('F', 'H', 'E', 'D', 'B'))
  )

  val neighboursWithObstacle: Map[Char, Seq[(Char, Char)]] = Map(
    ('A', Seq(('C','B'), ('I', 'E'), ('G', 'D'))),
    ('B', Seq(('H', 'E'))),
    ('C', Seq(('A','B'), ('G', 'E'), ('I', 'F'))),
    ('D', Seq(('F', 'E'))),
    ('E', Seq()),
    ('F', Seq(('D', 'E'))),
    ('G', Seq(('A','D'), ('C', 'E'), ('I', 'H'))),
    ('H', Seq(('B', 'E'))),
    ('I', Seq(('C','F'), ('A', 'E'), ('G', 'H')))
  )

  def charToIndex(char: Char): Int = char - 'A'

  def go(start: Char, len: Int, visited: List[Boolean]): Int = len match {
    case 0 => 0
    case 1 => 1
    case _ => {
      val newVisited = visited.updated(charToIndex(start), true)
      val neighbours = nearestNeighbours(start)
      var res = 0
      for (
        i <- neighbours.indices
      ) {
        if (!newVisited(charToIndex(neighbours(i))))
          res += go(neighbours(i), len - 1, newVisited)
      }

      val notNearNeighbours = neighboursWithObstacle(start)
      if(notNearNeighbours.nonEmpty) {
        for (i <- notNearNeighbours.indices) {
          val end = notNearNeighbours(i)._1
          val obs = notNearNeighbours(i)._2
          if(newVisited(charToIndex(obs)) && !newVisited(charToIndex(end)))
            res += go(end, len - 1, newVisited)
        }
      } else ()

      res
    }
  }

  def countPatternsFrom(f: Char, l: Int): Int =
    if (l <= 0 || l > 9) 0
    else {
      go(f, l, List.fill(9)(false))
    }
}

object Main extends App {
  ScreenLockingPattern.countPatternsFrom('A', 5)
}
