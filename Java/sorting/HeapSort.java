/**
 * Heap Sort Algorithm
 * 
 * Time Complexity: O(n log n) in all cases (worst, average, and best)
 * Space Complexity: O(1)
 * 
 * Heap sort is a comparison-based sorting algorithm that uses a binary heap
 * data structure.
 * It divides its input into a sorted and an unsorted region, and iteratively
 * shrinks the
 * unsorted region by extracting the largest element and moving that to the
 * sorted region.
 * The algorithm builds a max heap from the input data, then repeatedly extracts
 * the maximum
 * element from the heap and reconstructs the heap until all elements are
 * sorted.
 */
public class HeapSort {

  /**
   * Sorts an array using heap sort algorithm (ascending order).
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array
   */
  public static int[] heapSort(int[] arr) {
    int n = arr.length;

    // Build max heap (rearrange array)
    for (int i = n / 2 - 1; i >= 0; i--) {
      heapify(arr, n, i);
    }

    // Extract elements from heap one by one
    for (int i = n - 1; i > 0; i--) {
      // Move current root to end
      swap(arr, 0, i);

      // Call max heapify on the reduced heap
      heapify(arr, i, 0);
    }

    return arr;
  }

  /**
   * Heapifies a subtree rooted at node i (max heap).
   * 
   * @param arr Array representing the heap
   * @param n   Size of heap
   * @param i   Index of root node of subtree
   */
  private static void heapify(int[] arr, int n, int i) {
    int largest = i; // Initialize largest as root
    int left = 2 * i + 1; // Left child
    int right = 2 * i + 2; // Right child

    // If left child is larger than root
    if (left < n && arr[left] > arr[largest]) {
      largest = left;
    }

    // If right child is larger than largest so far
    if (right < n && arr[right] > arr[largest]) {
      largest = right;
    }

    // If largest is not root
    if (largest != i) {
      swap(arr, i, largest);

      // Recursively heapify the affected subtree
      heapify(arr, n, largest);
    }
  }

  /**
   * Sorts an array in descending order using heap sort.
   * 
   * @param arr Array of integers to be sorted
   * @return Sorted array in descending order
   */
  public static int[] heapSortDescending(int[] arr) {
    int n = arr.length;

    // Build min heap
    for (int i = n / 2 - 1; i >= 0; i--) {
      heapifyMin(arr, n, i);
    }

    // Extract elements from heap one by one
    for (int i = n - 1; i > 0; i--) {
      // Move current root to end
      swap(arr, 0, i);

      // Call min heapify on the reduced heap
      heapifyMin(arr, i, 0);
    }

    return arr;
  }

  /**
   * Heapifies a subtree rooted at node i (min heap).
   * 
   * @param arr Array representing the heap
   * @param n   Size of heap
   * @param i   Index of root node of subtree
   */
  private static void heapifyMin(int[] arr, int n, int i) {
    int smallest = i; // Initialize smallest as root
    int left = 2 * i + 1; // Left child
    int right = 2 * i + 2; // Right child

    // If left child is smaller than root
    if (left < n && arr[left] < arr[smallest]) {
      smallest = left;
    }

    // If right child is smaller than smallest so far
    if (right < n && arr[right] < arr[smallest]) {
      smallest = right;
    }

    // If smallest is not root
    if (smallest != i) {
      swap(arr, i, smallest);

      // Recursively heapify the affected subtree
      heapifyMin(arr, n, smallest);
    }
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
   * Prints the array in a tree-like structure to visualize the heap.
   * 
   * @param arr Array representing the heap
   */
  public static void printHeapStructure(int[] arr) {
    int n = arr.length;
    int height = (int) Math.ceil(Math.log(n + 1) / Math.log(2));

    System.out.println("Heap structure:");
    int index = 0;
    for (int level = 0; level < height && index < n; level++) {
      int elementsInLevel = (int) Math.pow(2, level);
      int spaces = (int) Math.pow(2, height - level) - 1;

      // Print leading spaces
      for (int s = 0; s < spaces; s++) {
        System.out.print("  ");
      }

      // Print elements in this level
      for (int i = 0; i < elementsInLevel && index < n; i++) {
        System.out.print(arr[index]);
        index++;

        // Print spaces between elements
        for (int s = 0; s < spaces * 2 + 1; s++) {
          System.out.print("  ");
        }
      }
      System.out.println();
    }
    System.out.println();
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
    System.out.println("Heap Sort Examples:");

    // Test ascending sort
    int[] arr1 = { 64, 34, 25, 12, 22, 11, 90 };
    System.out.print("\nOriginal array: ");
    printArray(arr1);

    heapSort(arr1);
    System.out.print("Sorted array (ascending): ");
    printArray(arr1);

    // Test descending sort
    int[] arr2 = { 64, 34, 25, 12, 22, 11, 90 };
    heapSortDescending(arr2);
    System.out.print("Sorted array (descending): ");
    printArray(arr2);

    // Test with already sorted array
    int[] arr3 = { 1, 2, 3, 4, 5 };
    System.out.print("\nAlready sorted array: ");
    printArray(arr3);
    heapSort(arr3);
    System.out.print("After heap sort: ");
    printArray(arr3);

    // Test with single element
    int[] arr4 = { 42 };
    System.out.print("\nSingle element array: ");
    printArray(arr4);
    heapSort(arr4);
    System.out.print("After heap sort: ");
    printArray(arr4);

    // Demonstrate heap structure visualization
    int[] arr5 = { 64, 34, 25, 12, 22, 11, 90 };
    System.out.println("\nVisualization of max heap before sorting:");

    // Build max heap first
    int n = arr5.length;
    for (int i = n / 2 - 1; i >= 0; i--) {
      heapify(arr5, n, i);
    }

    printHeapStructure(arr5);

    // Complete the sort
    for (int i = n - 1; i > 0; i--) {
      swap(arr5, 0, i);
      heapify(arr5, i, 0);
    }

    System.out.print("Final sorted array: ");
    printArray(arr5);
  }
}
