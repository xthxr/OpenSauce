    /*
     * 1. Brief description of the algorithm
     * Linear search sequentially checks each element of the list until a match
     * is found or the whole list has been searched.
     *
     * 2. Time complexity: O(n)
     * In the worst case, the algorithm needs to check every element.
     *
     * 3. Space complexity: O(1)
     * Constant space is used as it only requires a few variables.
     */
    object LinearSearch {
    
      // 4. Implementation
    
      /** Performs linear search on an array.
       *
       * @param arr    The array to search within.
       * @param target The element to search for.
       * @return       An Option containing the index of the target if found, None otherwise.
       */
      def linearSearch(arr: Array[Int], target: Int): Option[Int] = {
        for (i <- arr.indices) { // Use indices for index-based loop
          if (arr(i) == target) {
            return Some(i) // Return index if target is found
          }
        }
        None // Return None if target is not found after checking all elements
      }
    
      // 5. Example usage/test cases
      def main(args: Array[String]): Unit = {
        val array = Array(2, 5, 8, 12, 16, 23, 38, 56, 72, 91)
        val target1 = 23
        val target2 = 99
    
        println(s"Array: ${array.mkString("[", ", ", "]")}")
    
        println("\n--- Linear Search ---")
        linearSearch(array, target1) match {
          case Some(index) => println(s"Element $target1 found at index $index.")
          case None        => println(s"Element $target1 not found in the array.")
        }
    
        linearSearch(array, target2) match {
          case Some(index) => println(s"Element $target2 found at index $index.")
          case None        => println(s"Element $target2 not found in the array.")
        }
      }
    }
    
