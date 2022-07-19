package codewars

object MatrixDeterminant {
  type Matrix = List[List[Int]]

  def getMinor(m: Matrix)(i: Int, j: Int) : Matrix = {
    def without[A](xs: List[A], n: Int) : List[A] =
      xs.zipWithIndex.filterNot { case (_, i) => i + 1 == n }.map(_._1)

    without(m, i).map(r => without(r, j))
  }

  def getFirstRowPairs(m: Matrix) : List[(Int, Int)] =
    (1 to (m.length)).toList.map(y => (1, y))

  def getCell(m: Matrix)(i: Int, j: Int) : Int =
    m(i - 1)(j - 1)

  def evensNegative(xs: List[Int]) : List[Int] =
    xs.zipWithIndex
      .map { case (x, n) => (x, n % 2 != 0) }
      .map { case (x, isEven) => if (isEven) -x else x }

  def getDeterminant(m: Matrix) : Int =
    if (m.length == 1) getCell(m)(1, 1)
    else {
      val list = getFirstRowPairs(m).map(p =>
        (getCell(m) _).tupled(p) * getDeterminant((getMinor(m) _).tupled(p)))
      evensNegative(list).sum
    }

  def determinant(matrix: Array[Array[Int]]): Int =
    getDeterminant(matrix.map(_.toList).toList)

}
