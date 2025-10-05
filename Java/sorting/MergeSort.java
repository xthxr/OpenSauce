/**
 * Merge Sort Algorithm
 * 
 * Time Complexity: O(n log n) in all cases (worst, average, and best)
 * Space Complexity: O(n)
 * 
 * Merge sort is a divide-and-conquer algorithm that divides the input array
 * into two halves,
 * recursively sorts them, and then merges the two sorted halves.
 * It is a stable sorting algorithm and guarantees O(n log n) time complexity.
 */
public class MergeSort {

  /**
   * Sorts an array using merge sort algorithm (ascending order).
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array
   */
  public static int[] mergeSort(int[] arr) {
    if (arr.length <= 1) {
      return arr;
    }

    mergeSortHelper(arr, 0, arr.length - 1);
    return arr;
  }

  /**
   * Helper method for merge sort that recursively divides the array.
   * 
   * @param arr   Array to be sorted
   * @param left  Starting index
   * @param right Ending index
   */
  private static void mergeSortHelper(int[] arr, int left, int right) {
    if (left < right) {
      // Find the middle point
      int mid = left + (right - left) / 2;

      // Sort first and second halves
      mergeSortHelper(arr, left, mid);
      mergeSortHelper(arr, mid + 1, right);

      // Merge the sorted halves
      merge(arr, left, mid, right);
    }
  }

  /**
   * Merges two sorted subarrays of arr[].
   * First subarray is arr[left..mid]
   * Second subarray is arr[mid+1..right]
   * 
   * @param arr   Array containing the subarrays
   * @param left  Starting index of first subarray
   * @param mid   Ending index of first subarray
   * @param right Ending index of second subarray
   */
  private static void merge(int[] arr, int left, int mid, int right) {
    // Find sizes of two subarrays to be merged
    int n1 = mid - left + 1;
    int n2 = right - mid;

    // Create temporary arrays
    int[] leftArray = new int[n1];
    int[] rightArray = new int[n2];

    // Copy data to temporary arrays
    for (int i = 0; i < n1; i++) {
      leftArray[i] = arr[left + i];
    }
    for (int j = 0; j < n2; j++) {
      rightArray[j] = arr[mid + 1 + j];
    }

    // Merge the temporary arrays back into arr[left..right]
    int i = 0, j = 0;
    int k = left;

    while (i < n1 && j < n2) {
      if (leftArray[i] <= rightArray[j]) {
        arr[k] = leftArray[i];
        i++;
      } else {
        arr[k] = rightArray[j];
        j++;
      }
      k++;
    }

    // Copy remaining elements of leftArray[] if any
    while (i < n1) {
      arr[k] = leftArray[i];
      i++;
      k++;
    }

    // Copy remaining elements of rightArray[] if any
    while (j < n2) {
      arr[k] = rightArray[j];
      j++;
      k++;
    }
  }

  /**
   * Sorts an array in descending order using merge sort.
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array in descending order
   */
  public static int[] mergeSortDescending(int[] arr) {
    if (arr.length <= 1) {
      return arr;
    }

    mergeSortDescendingHelper(arr, 0, arr.length - 1);
    return arr;
  }

  /**
   * Helper method for descending merge sort.
   */
  private static void mergeSortDescendingHelper(int[] arr, int left, int right) {
    if (left < right) {
      int mid = left + (right - left) / 2;

      mergeSortDescendingHelper(arr, left, mid);
      mergeSortDescendingHelper(arr, mid + 1, right);

      mergeDescending(arr, left, mid, right);
    }
  }

  /**
   * Merges two subarrays in descending order.
   */
  private static void mergeDescending(int[] arr, int left, int mid, int right) {
    int n1 = mid - left + 1;
    int n2 = right - mid;

    int[] leftArray = new int[n1];
    int[] rightArray = new int[n2];

    for (int i = 0; i < n1; i++) {
      leftArray[i] = arr[left + i];
    }
    for (int j = 0; j < n2; j++) {
      rightArray[j] = arr[mid + 1 + j];
    }

    int i = 0, j = 0;
    int k = left;

    while (i < n1 && j < n2) {
      if (leftArray[i] >= rightArray[j]) {
        arr[k] = leftArray[i];
        i++;
      } else {
        arr[k] = rightArray[j];
        j++;
      }
      k++;
    }

    while (i < n1) {
      arr[k] = leftArray[i];
      i++;
      k++;
    }

    while (j < n2) {
      arr[k] = rightArray[j];
      j++;
      k++;
    }
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
    System.out.println("Merge Sort Examples:");

    // Test ascending sort
    int[] arr1 = { 64, 34, 25, 12, 22, 11, 90 };
    System.out.print("\nOriginal array: ");
    printArray(arr1);

    mergeSort(arr1);
    System.out.print("Sorted array (ascending): ");
    printArray(arr1);

    // Test descending sort
    int[] arr2 = { 64, 34, 25, 12, 22, 11, 90 };
    mergeSortDescending(arr2);
    System.out.print("Sorted array (descending): ");
    printArray(arr2);

    // Test with already sorted array
    int[] arr3 = { 1, 2, 3, 4, 5 };
    System.out.print("\nAlready sorted array: ");
    printArray(arr3);
    mergeSort(arr3);
    System.out.print("After merge sort: ");
    printArray(arr3);

    // Test with single element
    int[] arr4 = { 42 };
    System.out.print("\nSingle element array: ");
    printArray(arr4);
    mergeSort(arr4);
    System.out.print("After merge sort: ");
    printArray(arr4);

    // Test with large array
    int[] arr5 = { 38, 27, 43, 3, 9, 82, 10 };
    System.out.print("\nLarge array: ");
    printArray(arr5);
    mergeSort(arr5);
    System.out.print("After merge sort: ");
    printArray(arr5);
  }
}
