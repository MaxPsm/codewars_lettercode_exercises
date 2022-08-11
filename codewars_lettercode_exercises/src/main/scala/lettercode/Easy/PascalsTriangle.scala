package lettercode.Easy

import scala.collection.mutable.ArrayBuffer

object PascalsTriangle {
  def generate(numRows: Int): List[List[Int]] = {
    if (numRows == 1) List(List(1))
    else if (numRows == 2) List(List(1), List(1, 1))
    else {
      var res = ArrayBuffer[Array[Int]](Array(1), Array(1, 1))
      var temp = ArrayBuffer(1, 1)
      var p1 = 0
      var p2 = 1

      for(i <- 2 until numRows) {
        val itArr = ArrayBuffer.fill(i + 1)(1)
        while (p2 < i) {
          itArr(p2) = temp(p1) + temp(p2)
          p1 = p2
          p2 += 1
        }
        p1 = 0
        p2 = 1

        res = res :+ itArr.toArray
        temp = itArr
      }

      res.map(a => a.toList).toList
    }
  }

  def generate2(numRows: Int): List[List[Int]] = {
    def iter(index: Int, acc: List[List[Int]]): List[List[Int]] = index match {
      case `numRows` => acc
      case 0 => iter(1, List(List(1)))
      case 1 => iter(2, List(1, 1) +: acc)
      case _ =>
        val row = 1 +: acc.head.sliding(2 , 1).map(_.sum).toList :+ 1
        iter(index + 1, row +: acc)
    }

    iter(0, Nil).reverse
  }
}

object Main extends App {
  println(PascalsTriangle.generate(5))
}
