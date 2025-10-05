/**
 * Selection Sort Algorithm
 * 
 * Time Complexity: O(n^2) in all cases (worst, average, and best)
 * Space Complexity: O(1)
 * 
 * Selection sort is a simple sorting algorithm that divides the input list into
 * two parts:
 * a sorted portion at the left end and an unsorted portion at the right end.
 * Initially, the sorted portion is empty and the unsorted portion is the entire
 * list.
 * The algorithm proceeds by finding the smallest (or largest) element in the
 * unsorted portion,
 * swapping it with the leftmost unsorted element, and moving the sublist
 * boundaries one element to the right.
 */
public class SelectionSort {

  /**
   * Sorts an array using selection sort algorithm (ascending order).
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array
   */
  public static int[] selectionSort(int[] arr) {
    int n = arr.length;

    // Traverse through all array elements
    for (int i = 0; i < n - 1; i++) {
      // Find the minimum element in unsorted portion
      int minIndex = i;

      for (int j = i + 1; j < n; j++) {
        if (arr[j] < arr[minIndex]) {
          minIndex = j;
        }
      }

      // Swap the found minimum element with the first element
      if (minIndex != i) {
        int temp = arr[minIndex];
        arr[minIndex] = arr[i];
        arr[i] = temp;
      }
    }

    return arr;
  }

  /**
   * Sorts an array in descending order using selection sort.
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array in descending order
   */
  public static int[] selectionSortDescending(int[] arr) {
    int n = arr.length;

    for (int i = 0; i < n - 1; i++) {
      // Find the maximum element in unsorted portion
      int maxIndex = i;

      for (int j = i + 1; j < n; j++) {
        if (arr[j] > arr[maxIndex]) {
          maxIndex = j;
        }
      }

      // Swap the found maximum element with the first element
      if (maxIndex != i) {
        int temp = arr[maxIndex];
        arr[maxIndex] = arr[i];
        arr[i] = temp;
      }
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
    System.out.println("Selection Sort Examples:");

    // Test ascending sort
    int[] arr1 = { 64, 34, 25, 12, 22, 11, 90 };
    System.out.print("\nOriginal array: ");
    printArray(arr1);

    selectionSort(arr1);
    System.out.print("Sorted array (ascending): ");
    printArray(arr1);

    // Test descending sort
    int[] arr2 = { 64, 34, 25, 12, 22, 11, 90 };
    selectionSortDescending(arr2);
    System.out.print("Sorted array (descending): ");
    printArray(arr2);

    // Test with already sorted array
    int[] arr3 = { 1, 2, 3, 4, 5 };
    System.out.print("\nAlready sorted array: ");
    printArray(arr3);
    selectionSort(arr3);
    System.out.print("After selection sort: ");
    printArray(arr3);

    // Test with single element
    int[] arr4 = { 42 };
    System.out.print("\nSingle element array: ");
    printArray(arr4);
    selectionSort(arr4);
    System.out.print("After selection sort: ");
    printArray(arr4);
  }
}
