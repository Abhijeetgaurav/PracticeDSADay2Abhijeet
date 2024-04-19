class CombinationSum {

  def findCombinations(nums: Array[Int], target: Int): List[List[Int]] = {

    def solveBacktrack(start: Int, target: Int, path: List[Int], result: List[List[Int]]): List[List[Int]] = {
      if (target == 0) {
        path :: result
      } else if (target < 0 || start >= nums.length) {
        result
      } else {
        val includeCurrent = solveBacktrack(start + 1, target - nums(start), nums(start) :: path, result)

        val excludeCurrent = solveBacktrack(start + 1, target, path, result)

        includeCurrent ::: excludeCurrent
      }
    }

    solveBacktrack(0, target, List(), List()).distinct
  }
}


