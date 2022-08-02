package codewars.lvl6

object UniqueInOrder {
  def uniqueInOrder[T](xs: Iterable[T]): Seq[T] = {
    def go(xs: List[T], res: Seq[T]): Seq[T] = xs match {
      case Nil => res
      case it if res.isEmpty => go(xs.tail, res :+ it.head)
      case it if it.head == res.last => go(xs.tail, res)
      case it => go(xs.tail, res :+ it.head)
    }

    go(xs.toList, Seq.empty)
  }
}

object Main extends App {
  println(UniqueInOrder.uniqueInOrder(List(1, 2, 2, 3, 3)))
}
