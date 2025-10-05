/**
 * Insertion Sort Algorithm
 * 
 * Time Complexity: O(n^2) in worst and average case, O(n) in best case (when
 * array is already sorted)
 * Space Complexity: O(1)
 * 
 * Insertion sort is a simple sorting algorithm that builds the final sorted
 * array one item at a time.
 * It iterates through an input array and removes one element per iteration,
 * finds the place the element
 * belongs in the sorted list, and inserts it there. It repeats this process
 * until no elements remain in the unsorted list.
 */
public class InsertionSort {

  /**
   * Sorts an array using insertion sort algorithm (ascending order).
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array
   */
  public static int[] insertionSort(int[] arr) {
    int n = arr.length;

    // Traverse through all array elements starting from index 1
    for (int i = 1; i < n; i++) {
      int key = arr[i];
      int j = i - 1;

      // Move elements of arr[0..i-1] that are greater than key
      // to one position ahead of their current position
      while (j >= 0 && arr[j] > key) {
        arr[j + 1] = arr[j];
        j = j - 1;
      }

      // Insert the key at its correct position
      arr[j + 1] = key;
    }

    return arr;
  }

  /**
   * Sorts an array in descending order using insertion sort.
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array in descending order
   */
  public static int[] insertionSortDescending(int[] arr) {
    int n = arr.length;

    for (int i = 1; i < n; i++) {
      int key = arr[i];
      int j = i - 1;

      // Move elements of arr[0..i-1] that are smaller than key
      // to one position ahead of their current position
      while (j >= 0 && arr[j] < key) {
        arr[j + 1] = arr[j];
        j = j - 1;
      }

      arr[j + 1] = key;
    }

    return arr;
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
    System.out.println("Insertion Sort Examples:");

    // Test ascending sort
    int[] arr1 = { 64, 34, 25, 12, 22, 11, 90 };
    System.out.print("\nOriginal array: ");
    printArray(arr1);

    insertionSort(arr1);
    System.out.print("Sorted array (ascending): ");
    printArray(arr1);

    // Test descending sort
    int[] arr2 = { 64, 34, 25, 12, 22, 11, 90 };
    insertionSortDescending(arr2);
    System.out.print("Sorted array (descending): ");
    printArray(arr2);

    // Test with already sorted array
    int[] arr3 = { 1, 2, 3, 4, 5 };
    System.out.print("\nAlready sorted array: ");
    printArray(arr3);
    insertionSort(arr3);
    System.out.print("After insertion sort: ");
    printArray(arr3);

    // Test with single element
    int[] arr4 = { 42 };
    System.out.print("\nSingle element array: ");
    printArray(arr4);
    insertionSort(arr4);
    System.out.print("After insertion sort: ");
    printArray(arr4);
  }
}
