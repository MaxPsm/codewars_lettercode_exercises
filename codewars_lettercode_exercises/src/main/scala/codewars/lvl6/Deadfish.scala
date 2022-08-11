package codewars.lvl6

object Deadfish {
  def parse(data: String): List[Int] = {
    data.foldLeft((List.empty[Int], 0)){
      (acc, ch) => ch match {
        case 'i' => (acc._1, acc._2 + 1)
        case 'd' => (acc._1, acc._2 - 1)
        case 's' => (acc._1, math.pow(acc._2, 2).toInt)
        case 'o' => (acc._1 :+ acc._2, acc._2)
      }
    }._1
  }
}
