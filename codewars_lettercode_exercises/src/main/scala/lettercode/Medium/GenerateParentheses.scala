package lettercode.Medium

object GenerateParentheses {
  def generateParenthesis(n: Int): List[String] = {
    var ans: List[String] = List.empty
    def go(str: String, n: Int, left: Int, right: Int): Unit = {
      if (str.length == 2 * n) ans = ans :+ str

      if (left < n) {
        go(str + "(", n, left + 1, right)
      }

      if (right < left) {
        go(str + ")", n, left, right + 1)
      }
    }

    go("", n, 0, 0)

    ans
  }
}

object App extends App {
  println(GenerateParentheses.generateParenthesis(3))
}
