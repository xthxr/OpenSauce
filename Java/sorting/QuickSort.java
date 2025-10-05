/**
 * Quick Sort Algorithm with Multiple Pivot Selection Strategies
 * 
 * Time Complexity: O(n log n) average case, O(n^2) worst case
 * Space Complexity: O(log n) due to recursion stack
 * 
 * Quick sort is a divide-and-conquer algorithm that picks an element as a pivot
 * and partitions the array around the pivot. The key process is the partition
 * operation.
 */
public class QuickSort {

  // Pivot selection strategies
  public enum PivotStrategy {
    LAST, // Last element as pivot
    FIRST, // First element as pivot
    MIDDLE, // Middle element as pivot
    RANDOM, // Random element as pivot
    MEDIAN_OF_THREE // Median of first, middle, and last elements
  }

  /**
   * Sorts an array using quick sort algorithm (ascending order).
   * Uses LAST element as pivot by default.
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array
   */
  public static int[] quickSort(int[] arr) {
    return quickSort(arr, PivotStrategy.LAST);
  }

  /**
   * Sorts an array using quick sort with specified pivot strategy.
   * 
   * @param arr      Array of integers to be sorted
   * @param strategy Pivot selection strategy
   * @return Sorted array
   */
  public static int[] quickSort(int[] arr, PivotStrategy strategy) {
    if (arr.length <= 1) {
      return arr;
    }

    quickSortHelper(arr, 0, arr.length - 1, strategy);
    return arr;
  }

  /**
   * Helper method for quick sort that recursively sorts the array.
   * 
   * @param arr      Array to be sorted
   * @param low      Starting index
   * @param high     Ending index
   * @param strategy Pivot selection strategy
   */
  private static void quickSortHelper(int[] arr, int low, int high, PivotStrategy strategy) {
    if (low < high) {
      // Partition the array and get the pivot index
      int pivotIndex = partition(arr, low, high, strategy);

      // Recursively sort elements before and after partition
      quickSortHelper(arr, low, pivotIndex - 1, strategy);
      quickSortHelper(arr, pivotIndex + 1, high, strategy);
    }
  }

  /**
   * Partitions the array around a pivot element.
   * 
   * @param arr      Array to be partitioned
   * @param low      Starting index
   * @param high     Ending index
   * @param strategy Pivot selection strategy
   * @return Final position of the pivot
   */
  private static int partition(int[] arr, int low, int high, PivotStrategy strategy) {
    // Select pivot based on strategy
    int pivotIndex = selectPivot(arr, low, high, strategy);

    // Move pivot to the end
    swap(arr, pivotIndex, high);
    int pivot = arr[high];

    // Index of smaller element
    int i = low - 1;

    for (int j = low; j < high; j++) {
      // If current element is smaller than or equal to pivot
      if (arr[j] <= pivot) {
        i++;
        swap(arr, i, j);
      }
    }

    // Place pivot in its correct position
    swap(arr, i + 1, high);
    return i + 1;
  }

  /**
   * Selects pivot index based on the chosen strategy.
   * 
   * @param arr      Array
   * @param low      Starting index
   * @param high     Ending index
   * @param strategy Pivot selection strategy
   * @return Index of selected pivot
   */
  private static int selectPivot(int[] arr, int low, int high, PivotStrategy strategy) {
    switch (strategy) {
      case FIRST:
        return low;

      case MIDDLE:
        return low + (high - low) / 2;

      case RANDOM:
        return low + (int) (Math.random() * (high - low + 1));

      case MEDIAN_OF_THREE:
        return medianOfThree(arr, low, high);

      case LAST:
      default:
        return high;
    }
  }

  /**
   * Finds the median of first, middle, and last elements.
   * 
   * @param arr  Array
   * @param low  Starting index
   * @param high Ending index
   * @return Index of median element
   */
  private static int medianOfThree(int[] arr, int low, int high) {
    int mid = low + (high - low) / 2;

    if (arr[low] > arr[mid]) {
      swap(arr, low, mid);
    }
    if (arr[low] > arr[high]) {
      swap(arr, low, high);
    }
    if (arr[mid] > arr[high]) {
      swap(arr, mid, high);
    }

    return mid;
  }

  /**
   * Sorts an array in descending order using quick sort.
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array in descending order
   */
  public static int[] quickSortDescending(int[] arr) {
    return quickSortDescending(arr, PivotStrategy.LAST);
  }

  /**
   * Sorts an array in descending order with specified pivot strategy.
   * 
   * @param arr      Array of integers to be sorted
   * @param strategy Pivot selection strategy
   * @return Sorted array in descending order
   */
  public static int[] quickSortDescending(int[] arr, PivotStrategy strategy) {
    if (arr.length <= 1) {
      return arr;
    }

    quickSortDescendingHelper(arr, 0, arr.length - 1, strategy);
    return arr;
  }

  /**
   * Helper method for descending quick sort.
   */
  private static void quickSortDescendingHelper(int[] arr, int low, int high, PivotStrategy strategy) {
    if (low < high) {
      int pivotIndex = partitionDescending(arr, low, high, strategy);
      quickSortDescendingHelper(arr, low, pivotIndex - 1, strategy);
      quickSortDescendingHelper(arr, pivotIndex + 1, high, strategy);
    }
  }

  /**
   * Partitions the array in descending order.
   */
  private static int partitionDescending(int[] arr, int low, int high, PivotStrategy strategy) {
    int pivotIndex = selectPivot(arr, low, high, strategy);
    swap(arr, pivotIndex, high);
    int pivot = arr[high];

    int i = low - 1;

    for (int j = low; j < high; j++) {
      if (arr[j] >= pivot) {
        i++;
        swap(arr, i, j);
      }
    }

    swap(arr, i + 1, high);
    return i + 1;
  }

  /**
   * Swaps two elements in an array.
   * 
   * @param arr Array
   * @param i   First index
   * @param j   Second index
   */
  private static void swap(int[] arr, int i, int j) {
    int temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
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
    System.out.println("Quick Sort Examples:");

    // Test ascending sort with default pivot (LAST)
    int[] arr1 = { 64, 34, 25, 12, 22, 11, 90 };
    System.out.print("\nOriginal array: ");
    printArray(arr1);

    quickSort(arr1);
    System.out.print("Sorted array (ascending, LAST pivot): ");
    printArray(arr1);

    // Test with MEDIAN_OF_THREE pivot strategy
    int[] arr2 = { 64, 34, 25, 12, 22, 11, 90 };
    quickSort(arr2, PivotStrategy.MEDIAN_OF_THREE);
    System.out.print("Sorted array (MEDIAN_OF_THREE pivot): ");
    printArray(arr2);

    // Test with RANDOM pivot strategy
    int[] arr3 = { 64, 34, 25, 12, 22, 11, 90 };
    quickSort(arr3, PivotStrategy.RANDOM);
    System.out.print("Sorted array (RANDOM pivot): ");
    printArray(arr3);

    // Test descending sort
    int[] arr4 = { 64, 34, 25, 12, 22, 11, 90 };
    quickSortDescending(arr4);
    System.out.print("Sorted array (descending): ");
    printArray(arr4);

    // Test with already sorted array
    int[] arr5 = { 1, 2, 3, 4, 5 };
    System.out.print("\nAlready sorted array: ");
    printArray(arr5);
    quickSort(arr5, PivotStrategy.MEDIAN_OF_THREE);
    System.out.print("After quick sort: ");
    printArray(arr5);

    // Test with single element
    int[] arr6 = { 42 };
    System.out.print("\nSingle element array: ");
    printArray(arr6);
    quickSort(arr6);
    System.out.print("After quick sort: ");
    printArray(arr6);
  }
}
