package codewars.lvl5

object JosephusSurvivor {
  def josephusSurvivor(n: Int, k: Int): Int = {
    if (n <= 1) 1
    else {
      (josephusSurvivor(n - 1, k) + k - 1) % n + 1
    }
  }

  def josephusSurvivor2(n: Int, k: Int): Int = eliminate(1 to n, k)

  def eliminate(people: Seq[Int], k: Int): Int = {
    people.splitAt(k % people.size) match {
      case (Nil, Seq(x)) => x
      case (front, back) => eliminate((back :++ front).init, k)
    }
  }
}

object Main extends App {
  println(JosephusSurvivor.josephusSurvivor2(7, 3))
}
