/*
 * 1. Brief description of the algorithm
 * Binary search is an efficient algorithm for finding an item from a **sorted**
 * list of items. It works by repeatedly dividing the search interval in half.
 *
 * 2. Time complexity: O(log n)
 * With each comparison, the search space is halved.
 *
 * 3. Space complexity:
 * - Iterative: O(1) - Constant space is used.
 * - Recursive: O(log n) - Due to the call stack depth for recursion.
 */
object BinarySearch {

  // 4. Implementation
  
  /** Iterative Binary Search
   */
  def binarySearchIterative(arr: Array[Int], target: Int): Option[Int] = {
    var low = 0
    var high = arr.length - 1

    while (low <= high) {
        val mid = low + (high - low) / 2
        if (arr(mid) == target) return Some(mid)
        else if (arr(mid) < target) low = mid + 1
        else high = mid - 1
    }
    None
  }
  
  /** Recursive Binary Search
   */
  def binarySearchRecursive(arr: Array[Int], target: Int, low: Int, high: Int): Option[Int] = {
    if (low > high) return None

    val mid = low + (high - low) / 2
    if (arr(mid) == target) {
      Some(mid)
    } else if (arr(mid) > target) {
      binarySearchRecursive(arr, target, low, mid - 1)
    } else {
      binarySearchRecursive(arr, target, mid + 1, high)
    }
  }

  // 5. Example usage/test cases
  def main(args: Array[String]): Unit = {
    val sortedArray = Array(2, 5, 8, 12, 16, 23, 38, 56, 72, 91)
    val target1 = 23
    val target2 = 99

    println(s"Array: ${sortedArray.mkString("[", ", ", "]")}")
    
    println("\n--- Iterative ---")
    binarySearchIterative(sortedArray, target1) match {
      case Some(index) => println(s"Element $target1 found at index $index.")
      case None        => println(s"Element $target1 not found in the array.")
    }
     binarySearchIterative(sortedArray, target2) match {
      case Some(index) => println(s"Element $target2 found at index $index.")
      case None        => println(s"Element $target2 not found in the array.")
    }

    println("\n--- Recursive ---")
    binarySearchRecursive(sortedArray, target1, 0, sortedArray.length - 1) match {
      case Some(index) => println(s"Element $target1 found at index $index.")
      case None        => println(s"Element $target1 not found in the array.")
    }
    binarySearchRecursive(sortedArray, target2, 0, sortedArray.length - 1) match {
      case Some(index) => println(s"Element $target2 found at index $index.")
      case None        => println(s"Element $target2 not found in the array.")
    }
  }
}

