package codewars.lvl4

object Snail {

  def makeRightLists(xs: List[List[Int]]): List[List[Int]] = {
    xs.map(x => x.dropRight(1))
  }

  def makeLeftLists(xs: List[List[Int]]): List[List[Int]] = {
    xs.map(x => x.tail)
  }

  def makeLeftSLice(xs: List[List[Int]]): List[Int] = {
    xs.map(x => x.head).reverse
  }

  def makeRightSlice(xs: List[List[Int]]): List[Int] = {
    xs.map(x => x.last)
  }

  def snail(xs: List[List[Int]]): List[Int] = {
    def go(xs: List[List[Int]], res: List[Int], n: Int, dir: Char): List[Int] = dir match {
      case 'u' if n > 0 => go(xs.drop(1), res ::: xs.head.take(n), n - 1, 'r')
      case 'r' if n > 0 => go(makeRightLists(xs), res ::: makeRightSlice(xs), n, 'd')
      case 'd' if n > 0 => go(xs.dropRight(1), res ::: xs.last.reverse, n - 1, 'l')
      case 'l' if n > 0 => go(makeLeftLists(xs), res ::: makeLeftSLice(xs), n, 'u')
      case _ if xs.isEmpty => res
      case _ if n == 0 => res :+ xs.head.head
    }

    if (xs.nonEmpty) go(xs, List.empty[Int], xs.length, 'u')
    else List.empty[Int]
  }
}
