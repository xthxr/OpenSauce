/*
 * ## Problem: Binary Search ##
 *
 * Binary search is an efficient algorithm for finding an item from a **sorted** list of items.
 * It works by repeatedly dividing the search interval in half.
 *
 * How it works:
 * 1.  Compare the target value to the middle element of the array.
 * 2.  If they are not equal, the half in which the target cannot lie is eliminated,
 * and the search continues on the remaining half.
 * 3.  This process is repeated until the target value is found or the remaining half is empty.
 *
 * Prerequisite: The input array must be sorted.
 */
object BinarySearch {

  // A recursive implementation of Binary Search.
  def binarySearchRecursive(arr: Array[Int], target: Int, low: Int, high: Int): Option[Int] = {
    // Base case: If the search range is invalid, the element is not present.
    if (low > high) {
      return None
    }

    // Find the middle index to split the search space.
    // Using (low + (high - low) / 2) avoids potential overflow if low and high are large.
    val mid = low + (high - low) / 2

    // Check if the middle element is the target.
    if (arr(mid) == target) {
      Some(mid) // Target found, return its index.
    } else if (arr(mid) > target) {
      // If the middle element is greater than the target,
      // the target must be in the left half.
      binarySearchRecursive(arr, target, low, mid - 1)
    } else {
      // If the middle element is less than the target,
      // the target must be in the right half.
      binarySearchRecursive(arr, target, mid + 1, high)
    }
  }

  // An iterative implementation of Binary Search.
  def binarySearchIterative(arr: Array[Int], target: Int): Option[Int] = {
    var low = 0
    var high = arr.length - 1

    while (low <= high) {
        val mid = low + (high - low) / 2

        if (arr(mid) == target) {
            return Some(mid) // Target found
        } else if (arr(mid) < target) {
            low = mid + 1 // Search in the right half
        } else {
            high = mid - 1 // Search in the left half
        }
    }
    None // Target not found
  }

  // Main method to test the binary search functions.
  def main(args: Array[String]): Unit = {
    val sortedArray = Array(2, 5, 8, 12, 16, 23, 38, 56, 72, 91)
    val target1 = 23
    val target2 = 99

    println(s"Array: ${sortedArray.mkString("[", ", ", "]")}")
    
    println("\n--- Recursive ---")
    binarySearchRecursive(sortedArray, target1, 0, sortedArray.length - 1) match {
      case Some(index) => println(s"Element $target1 found at index $index.")
      case None        => println(s"Element $target1 not found in the array.")
    }
    binarySearchRecursive(sortedArray, target2, 0, sortedArray.length - 1) match {
      case Some(index) => println(s"Element $target2 found at index $index.")
      case None        => println(s"Element $target2 not found in the array.")
    }

    println("\n--- Iterative ---")
    binarySearchIterative(sortedArray, target1) match {
      case Some(index) => println(s"Element $target1 found at index $index.")
      case None        => println(s"Element $target1 not found in the array.")
    }
     binarySearchIterative(sortedArray, target2) match {
      case Some(index) => println(s"Element $target2 found at index $index.")
      case None        => println(s"Element $target2 not found in the array.")
    }
  }
}
