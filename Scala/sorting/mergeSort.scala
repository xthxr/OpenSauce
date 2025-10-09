/*
 * ## Problem: Merge Sort ##
 *
 * Merge Sort is a highly efficient, stable, comparison-based sorting algorithm.
 * It's a classic example of the "divide and conquer" paradigm.
 *
 * How it works:
 * 1.  **Divide**: The unsorted list is divided into n sublists, each containing one element.
 * A list of one element is considered sorted.
 * 2.  **Conquer**: Repeatedly merge sublists to produce new sorted sublists until there is only
 * one sublist remaining. This will be the sorted list.
 *
 * The core of the algorithm is the `merge` function, which merges two sorted sublists
 * into a single sorted list.
 */
object MergeSort {

  def mergeSort(list: List[Int]): List[Int] = {
    // A list with zero or one element is already sorted. This is our base case.
    if (list.length <= 1) {
      list
    } else {
      // Find the middle of the list to split it into two halves.
      val middle = list.length / 2
      val (left, right) = list.splitAt(middle)

      // Recursively sort both halves.
      val sortedLeft = mergeSort(left)
      val sortedRight = mergeSort(right)

      // Merge the two sorted halves back together.
      merge(sortedLeft, sortedRight)
    }
  }

  def merge(left: List[Int], right: List[Int]): List[Int] = {
    // This is a helper function to merge two sorted lists.
    // We use pattern matching to handle the different cases.
    (left, right) match {
      // If the right list is empty, the result is the left list.
      case (l, Nil) => l
      // If the left list is empty, the result is the right list.
      case (Nil, r) => r
      // If both lists have elements, compare their heads.
      case (lHead :: lTail, rHead :: rTail) =>
        if (lHead < rHead) {
          // If the head of the left list is smaller, it comes first.
          // Then, we recursively merge the rest of the left list with the right list.
          lHead :: merge(lTail, right)
        } else {
          // Otherwise, the head of the right list comes first.
          // Then, we recursively merge the left list with the rest of the right list.
          rHead :: merge(left, rTail)
        }
    }
  }

  // Main method to test the mergeSort function.
  def main(args: Array[String]): Unit = {
    val unsortedList = List(38, 27, 43, 3, 9, 82, 10)
    println(s"Unsorted List: ${unsortedList.mkString(", ")}")

    val sortedList = mergeSort(unsortedList)
    println(s"Sorted List  : ${sortedList.mkString(", ")}")
    
    val anotherList = List(5, 1, 4, 8, 2, 9, 3, 7, 6)
    println(s"\nUnsorted List: ${anotherList.mkString(", ")}")
    val sortedAnotherList = mergeSort(anotherList)
    println(s"Sorted List  : ${sortedAnotherList.mkString(", ")}")
  }
}
