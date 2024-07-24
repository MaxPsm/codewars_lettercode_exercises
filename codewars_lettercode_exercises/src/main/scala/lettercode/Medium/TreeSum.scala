package lettercode.Medium

object TreeSum {
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    if (nums.isEmpty) return List.empty[List[Int]]
    val numsS = nums.sorted
    val ans = numsS.indices.foldLeft(List.empty[List[Int]]) {
      case (acc, i) if numsS(i) <= 0 && (i == 0 || (i > 0 && numsS(i) != numsS(i - 1))) =>
        twoSum(numsS, i, acc)
      case (acc, _) => acc
    }

    ans
  }

  private def twoSum(nums: Array[Int], from: Int, ans: List[List[Int]]): List[List[Int]] = {

    @scala.annotation.tailrec
    def loop(x: Int, y: Int, result: List[List[Int]]): List[List[Int]] = {
      if (x < y) {
        val sum = nums(from) + nums(x) + nums(y)
        if (sum > 0 || (y < nums.length - 1 && nums(y) == nums(y + 1))) loop(x, y - 1, result)
        else if (sum < 0 || (x > from + 1 && nums(x) == nums(x - 1))) loop(x + 1, y, result)
        else {
          loop(x + 1, y - 1, List(nums(from), nums(x), nums(y)) +: result)
        }
      } else result
    }

    loop(from + 1, nums.length - 1, ans)
  }
}
