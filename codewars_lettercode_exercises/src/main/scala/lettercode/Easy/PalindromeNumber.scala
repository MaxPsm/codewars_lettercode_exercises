package lettercode.Easy

object PalindromeNumber {

  def isPalindrome(x: Int): Boolean = {
    x.toString.equals(x.toString.reverse)
  }

  def isPalindrome2(x: Int): Boolean = {
    def go(x: Int, res: Int): Int = {
      x match {
        case 0 => res
        case _ => go(x/10, res * 10 + x % 10)
      }
    }

    Option.when(x >= 0)(go(x, 0)).fold(false)(v => v == x)
  }
}
