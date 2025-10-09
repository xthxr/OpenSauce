/*
 * 1. Brief description of the algorithm
 * Merge Sort is a highly efficient, stable, comparison-based sorting algorithm.
 * It's a classic example of the "divide and conquer" paradigm that works by
 * recursively splitting the list into halves, sorting them, and then merging them back.
 *
 * 2. Time complexity: O(n log n)
 * This is consistent for the best, average, and worst cases because the list is
 * always divided in half.
 *
 * 3. Space complexity: O(n)
 * This is because it requires additional memory to store the merged sublists.
 */
object MergeSort {

  // 4. Implementation
  def mergeSort(list: List[Int]): List[Int] {
    // Base case: A list with zero or one element is already sorted.
    if (list.length <= 1) {
      list
    } else {
      // Divide the list into two halves.
      val middle = list.length / 2
      val (left, right) = list.splitAt(middle)

      // Recursively sort both halves.
      val sortedLeft = mergeSort(left)
      val sortedRight = mergeSort(right)

      // Conquer by merging the two sorted halves.
      merge(sortedLeft, sortedRight)
    }
  }

  def merge(left: List[Int], right: List[Int]): List[Int] = {
    (left, right) match {
      case (l, Nil) => l
      case (Nil, r) => r
      case (lHead :: lTail, rHead :: rTail) =>
        if (lHead < rHead) {
          lHead :: merge(lTail, right)
        } else {
          rHead :: merge(left, rTail)
        }
    }
  }

  // 5. Example usage/test cases
  def main(args: Array[String]): Unit = {
    val unsortedList = List(38, 27, 43, 3, 9, 82, 10)
    println(s"Unsorted List: ${unsortedList.mkString(", ")}")

    val sortedList = mergeSort(unsortedList)
    println(s"Sorted List  : ${sortedList.mkString(", ")}") // Expected: 3, 9, 10, 27, 38, 43, 82
    
    val anotherList = List(5, 1, 4, 8, 2, 9, 3, 7, 6)
    println(s"\nUnsorted List: ${anotherList.mkString(", ")}")
    val sortedAnotherList = mergeSort(anotherList)
    println(s"Sorted List  : ${sortedAnotherList.mkString(", ")}") // Expected: 1, 2, 3, 4, 5, 6, 7, 8, 9
  }
}

