package lettercode.Medium

object addTwoNumbers {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val res = new ListNode(0)
    var currList = res
    var carry = 0
    var ll1 = l1
    var ll2 = l2

    while (ll1 != null || ll2 != null) {
      var x = 0
      var y = 0
      if (ll1 != null) x = ll1.x
      if (ll2 != null) y = ll2.x
      val currDigit = (carry + x + y) % 10
      carry = (carry + x + y) / 10

      currList.next = new ListNode(currDigit, null)
      currList = currList.next

      if (ll1 != null) ll1 = ll1.next
      if (ll2 != null) ll2 = ll2.next
      if (ll1 == null && ll2 == null && carry > 0) {
        currList.next = new ListNode(carry, null)
        currList = currList.next
      }
    }
    res.next
  }
}
