package lettercode.Easy

object TwoSum {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val indexedNums = nums.zipWithIndex
    val dict = indexedNums.toMap

    indexedNums.collectFirst {
      case (value, index) if dict.get(target - value).exists(_ != index) =>
        Array(index, dict(target - value))
    }.getOrElse(Array())
  }
}
