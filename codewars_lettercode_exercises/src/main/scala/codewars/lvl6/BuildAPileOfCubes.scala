package codewars.lvl6

object BuildAPileOfCubes {
  def currentCube(n: Int): Long = Math.pow(n, 3).toLong

  def go(m: Long, n: Int): Int = m match{
    case m if m > 0 => go(m - currentCube(n), n + 1)
    case 0 => n - 1
    case _ => -1
  }

  def findNb(m: Long): Int = {
    go(m, 1)
  }
}
