package codewars.lvl4

object TwiceLinear {
  def dblLinear(n: Int): Int = {
    def go(u: Vector[Int], pointer2: Int, pointer3: Int, n: Int, counter: Int): Int = {
      val min = math.min(2 * u(pointer2) + 1, 3 * u(pointer3) + 1)
      println(s"min: ${math.min(2 * u(pointer2) + 1, 3 * u(pointer3) + 1)}", s"p2: $pointer2", s"p3: $pointer3")
      (u :+ min, counter) match {
        case (seq, c) if c == n => seq(c)
        case (seq, _) if 3 * seq(pointer3) + 1 == 2 * seq(pointer2) + 1 => go(seq, pointer2 + 1, pointer3 + 1, n, counter)
        case (seq, _) if 2 * seq(pointer2) + 1 == min => go(seq, pointer2 + 1, pointer3, n, counter + 1)
        case (seq, _) if 3 * seq(pointer3) + 1 == min => go(seq, pointer2, pointer3 + 1, n, counter + 1)
      }
    }

    go(Vector(1), 0, 0, n, 1)
  }

  def dblLinear2(n: Int): Int = {
    var u = Vector(1)
    var i = 1
    var (p2, p3) = (0, 0)

    while(i <= n) {
      u = u :+ math.min(2 * u(p2) + 1, 3 * u(p3) + 1)
      println(s"min: ${math.min(2 * u(p2) + 1, 3 * u(p3) + 1)}", s"p2: $p2", s"p3: $p3")
      if(u(i) == 2 * u(p2) + 1) p2 += 1
      if(u(i) == 3 * u(p3) + 1) p3 += 1
      i += 1
    }
    u(n)
  }
}

object Main extends App {
  TwiceLinear.dblLinear(20)
  println(":::::::::::::::::::::::")
  TwiceLinear.dblLinear2(20)
}