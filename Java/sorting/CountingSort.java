/**
 * Counting Sort Algorithm
 * 
 * Time Complexity: O(n + k) where n is the number of elements and k is the
 * range of input
 * Space Complexity: O(k)
 * 
 * Counting sort is an integer sorting algorithm that operates by counting the
 * number of objects
 * that have distinct key values, and using arithmetic to determine the
 * positions of each key.
 * It is efficient when the range of input data (k) is not significantly greater
 * than the number
 * of elements to be sorted (n).
 * 
 * Note: This implementation works with non-negative integers. For negative
 * numbers,
 * an offset can be applied.
 */
public class CountingSort {

  /**
   * Sorts an array using counting sort algorithm (ascending order).
   * Works only with non-negative integers.
   * 
   * @param arr Array of non-negative integers to be sorted
   * @return Sorted array
   */
  public static int[] countingSort(int[] arr) {
    if (arr.length == 0) {
      return arr;
    }

    // Find the maximum element to determine range
    int max = findMax(arr);

    return countingSortByRange(arr, max);
  }

  /**
   * Sorts an array using counting sort with a specified range.
   * 
   * @param arr Array of non-negative integers to be sorted
   * @param max Maximum value in the array
   * @return Sorted array
   */
  private static int[] countingSortByRange(int[] arr, int max) {
    int n = arr.length;

    // Create counting array to store count of each element
    int[] count = new int[max + 1];

    // Store count of each element
    for (int i = 0; i < n; i++) {
      count[arr[i]]++;
    }

    // Modify count array to store actual positions
    for (int i = 1; i <= max; i++) {
      count[i] += count[i - 1];
    }

    // Build the output array
    int[] output = new int[n];

    // Place elements in sorted order (traverse from right to maintain stability)
    for (int i = n - 1; i >= 0; i--) {
      output[count[arr[i]] - 1] = arr[i];
      count[arr[i]]--;
    }

    // Copy output array to original array
    for (int i = 0; i < n; i++) {
      arr[i] = output[i];
    }

    return arr;
  }

  /**
   * Sorts an array in descending order using counting sort.
   * 
   * @param arr Array of non-negative integers to be sorted
   * @return Sorted array in descending order
   */
  public static int[] countingSortDescending(int[] arr) {
    if (arr.length == 0) {
      return arr;
    }

    int n = arr.length;
    int max = findMax(arr);

    // Create counting array
    int[] count = new int[max + 1];

    // Store count of each element
    for (int i = 0; i < n; i++) {
      count[arr[i]]++;
    }

    // Place elements in descending order
    int index = 0;
    for (int i = max; i >= 0; i--) {
      while (count[i] > 0) {
        arr[index++] = i;
        count[i]--;
      }
    }

    return arr;
  }

  /**
   * Sorts an array that may contain negative numbers using counting sort.
   * 
   * @param arr Array of integers (may contain negative numbers)
   * @return Sorted array
   */
  public static int[] countingSortWithNegatives(int[] arr) {
    if (arr.length == 0) {
      return arr;
    }

    int n = arr.length;

    // Find minimum and maximum elements
    int min = arr[0];
    int max = arr[0];
    for (int i = 1; i < n; i++) {
      if (arr[i] < min)
        min = arr[i];
      if (arr[i] > max)
        max = arr[i];
    }

    int range = max - min + 1;

    // Create counting array
    int[] count = new int[range];

    // Store count of each element (with offset for negative numbers)
    for (int i = 0; i < n; i++) {
      count[arr[i] - min]++;
    }

    // Modify count array to store actual positions
    for (int i = 1; i < range; i++) {
      count[i] += count[i - 1];
    }

    // Build the output array
    int[] output = new int[n];

    for (int i = n - 1; i >= 0; i--) {
      output[count[arr[i] - min] - 1] = arr[i];
      count[arr[i] - min]--;
    }

    // Copy output array to original array
    for (int i = 0; i < n; i++) {
      arr[i] = output[i];
    }

    return arr;
  }

  /**
   * Finds the maximum element in an array.
   * 
   * @param arr Array of integers
   * @return Maximum element
   */
  private static int findMax(int[] arr) {
    int max = arr[0];
    for (int i = 1; i < arr.length; i++) {
      if (arr[i] > max) {
        max = arr[i];
      }
    }
    return max;
  }

  /**
   * Prints statistics about the counting sort process.
   * 
   * @param arr Array to analyze
   */
  public static void printStatistics(int[] arr) {
    if (arr.length == 0) {
      System.out.println("Empty array");
      return;
    }

    int min = arr[0];
    int max = arr[0];
    for (int i = 1; i < arr.length; i++) {
      if (arr[i] < min)
        min = arr[i];
      if (arr[i] > max)
        max = arr[i];
    }

    int range = max - min + 1;

    System.out.println("Array size: " + arr.length);
    System.out.println("Min value: " + min);
    System.out.println("Max value: " + max);
    System.out.println("Range: " + range);
    System.out.println("Space complexity: O(" + range + ")");
  }

  /**
   * Utility method to print an array
   */
  public static void printArray(int[] arr) {
    for (int i = 0; i < arr.length; i++) {
      System.out.print(arr[i] + " ");
    }
    System.out.println();
  }

  /**
   * Main method with example usage
   */
  public static void main(String[] args) {
    System.out.println("Counting Sort Examples:");

    // Test ascending sort with non-negative integers
    int[] arr1 = { 4, 2, 2, 8, 3, 3, 1 };
    System.out.print("\nOriginal array: ");
    printArray(arr1);
    printStatistics(arr1);

    countingSort(arr1);
    System.out.print("Sorted array (ascending): ");
    printArray(arr1);

    // Test descending sort
    int[] arr2 = { 4, 2, 2, 8, 3, 3, 1 };
    countingSortDescending(arr2);
    System.out.print("\nSorted array (descending): ");
    printArray(arr2);

    // Test with negative numbers
    int[] arr3 = { -5, -10, 0, -3, 8, 5, -1, 10 };
    System.out.print("\nArray with negative numbers: ");
    printArray(arr3);

    countingSortWithNegatives(arr3);
    System.out.print("Sorted array (with negatives): ");
    printArray(arr3);

    // Test with already sorted array
    int[] arr4 = { 1, 2, 3, 4, 5 };
    System.out.print("\nAlready sorted array: ");
    printArray(arr4);
    countingSort(arr4);
    System.out.print("After counting sort: ");
    printArray(arr4);

    // Test with single element
    int[] arr5 = { 42 };
    System.out.print("\nSingle element array: ");
    printArray(arr5);
    countingSort(arr5);
    System.out.print("After counting sort: ");
    printArray(arr5);

    // Test with duplicates
    int[] arr6 = { 5, 5, 5, 3, 3, 1, 1, 1, 1 };
    System.out.print("\nArray with many duplicates: ");
    printArray(arr6);
    countingSort(arr6);
    System.out.print("After counting sort: ");
    printArray(arr6);

    // Test with large range (demonstrates when counting sort may not be ideal)
    int[] arr7 = { 1, 1000, 2, 999 };
    System.out.print("\nArray with large range: ");
    printArray(arr7);
    System.out.println("Note: Large range makes counting sort less efficient");
    printStatistics(arr7);
    countingSort(arr7);
    System.out.print("After counting sort: ");
    printArray(arr7);
  }
}
