/**
 * Radix Sort Algorithm
 * 
 * Time Complexity: O(d * (n + k)) where d is the number of digits, n is the
 * number of elements,
 * and k is the range of digits (usually 10 for decimal)
 * Space Complexity: O(n + k)
 * 
 * Radix sort is a non-comparative sorting algorithm that sorts integers by
 * processing individual digits.
 * It processes digits from least significant digit (LSD) to most significant
 * digit (MSD).
 * It uses counting sort as a subroutine to sort the array based on each digit.
 * 
 * This implementation works with non-negative integers by default.
 */
public class RadixSort {

  /**
   * Sorts an array using radix sort algorithm (ascending order).
   * Works with non-negative integers.
   * 
   * @param arr Array of non-negative integers to be sorted
   * @return Sorted array
   */
  public static int[] radixSort(int[] arr) {
    if (arr.length == 0) {
      return arr;
    }

    // Find the maximum number to know the number of digits
    int max = findMax(arr);

    // Do counting sort for every digit
    // exp is 10^i where i is the current digit position
    for (int exp = 1; max / exp > 0; exp *= 10) {
      countingSortByDigit(arr, exp);
    }

    return arr;
  }

  /**
   * A function to do counting sort of arr[] according to the digit represented by
   * exp.
   * 
   * @param arr Array to be sorted
   * @param exp Exponent representing the current digit position (1, 10, 100, ...)
   */
  private static void countingSortByDigit(int[] arr, int exp) {
    int n = arr.length;
    int[] output = new int[n];
    int[] count = new int[10]; // Digits are 0-9

    // Store count of occurrences of each digit
    for (int i = 0; i < n; i++) {
      int digit = (arr[i] / exp) % 10;
      count[digit]++;
    }

    // Change count[i] so that it contains the actual position of this digit in
    // output[]
    for (int i = 1; i < 10; i++) {
      count[i] += count[i - 1];
    }

    // Build the output array (traverse from right to maintain stability)
    for (int i = n - 1; i >= 0; i--) {
      int digit = (arr[i] / exp) % 10;
      output[count[digit] - 1] = arr[i];
      count[digit]--;
    }

    // Copy the output array to arr[]
    for (int i = 0; i < n; i++) {
      arr[i] = output[i];
    }
  }

  /**
   * Sorts an array in descending order using radix sort.
   * 
   * @param arr Array of non-negative integers to be sorted
   * @return Sorted array in descending order
   */
  public static int[] radixSortDescending(int[] arr) {
    if (arr.length == 0) {
      return arr;
    }

    int max = findMax(arr);

    for (int exp = 1; max / exp > 0; exp *= 10) {
      countingSortByDigitDescending(arr, exp);
    }

    return arr;
  }

  /**
   * Counting sort by digit for descending order.
   * 
   * @param arr Array to be sorted
   * @param exp Exponent representing the current digit position
   */
  private static void countingSortByDigitDescending(int[] arr, int exp) {
    int n = arr.length;
    int[] output = new int[n];
    int[] count = new int[10];

    for (int i = 0; i < n; i++) {
      int digit = (arr[i] / exp) % 10;
      count[digit]++;
    }

    // For descending order, accumulate from right to left
    for (int i = 8; i >= 0; i--) {
      count[i] += count[i + 1];
    }

    for (int i = n - 1; i >= 0; i--) {
      int digit = (arr[i] / exp) % 10;
      output[count[digit] - 1] = arr[i];
      count[digit]--;
    }

    for (int i = 0; i < n; i++) {
      arr[i] = output[i];
    }
  }

  /**
   * Sorts an array that may contain negative numbers using radix sort.
   * 
   * @param arr Array of integers (may contain negative numbers)
   * @return Sorted array
   */
  public static int[] radixSortWithNegatives(int[] arr) {
    if (arr.length == 0) {
      return arr;
    }

    // Separate positive and negative numbers
    int negativeCount = 0;
    for (int i = 0; i < arr.length; i++) {
      if (arr[i] < 0) {
        negativeCount++;
      }
    }

    int[] negative = new int[negativeCount];
    int[] positive = new int[arr.length - negativeCount];

    int negIndex = 0, posIndex = 0;
    for (int i = 0; i < arr.length; i++) {
      if (arr[i] < 0) {
        negative[negIndex++] = -arr[i]; // Store as positive
      } else {
        positive[posIndex++] = arr[i];
      }
    }

    // Sort both arrays
    if (negative.length > 0) {
      radixSort(negative);
    }
    if (positive.length > 0) {
      radixSort(positive);
    }

    // Merge: negatives in reverse order (descending), then positives
    int index = 0;
    for (int i = negative.length - 1; i >= 0; i--) {
      arr[index++] = -negative[i];
    }
    for (int i = 0; i < positive.length; i++) {
      arr[index++] = positive[i];
    }

    return arr;
  }

  /**
   * Radix sort for strings (sorts strings of equal length).
   * 
   * @param arr Array of strings to be sorted
   * @return Sorted array
   */
  public static String[] radixSortStrings(String[] arr) {
    if (arr.length == 0) {
      return arr;
    }

    // Find maximum length
    int maxLen = 0;
    for (String s : arr) {
      if (s.length() > maxLen) {
        maxLen = s.length();
      }
    }

    // Sort by each character position from right to left
    for (int pos = maxLen - 1; pos >= 0; pos--) {
      countingSortByChar(arr, pos);
    }

    return arr;
  }

  /**
   * Counting sort by character position for strings.
   * 
   * @param arr Array of strings
   * @param pos Character position to sort by
   */
  private static void countingSortByChar(String[] arr, int pos) {
    int n = arr.length;
    String[] output = new String[n];
    int[] count = new int[256]; // ASCII characters

    for (int i = 0; i < n; i++) {
      int charIndex = pos < arr[i].length() ? arr[i].charAt(pos) : 0;
      count[charIndex]++;
    }

    for (int i = 1; i < 256; i++) {
      count[i] += count[i - 1];
    }

    for (int i = n - 1; i >= 0; i--) {
      int charIndex = pos < arr[i].length() ? arr[i].charAt(pos) : 0;
      output[count[charIndex] - 1] = arr[i];
      count[charIndex]--;
    }

    for (int i = 0; i < n; i++) {
      arr[i] = output[i];
    }
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
   * Prints statistics about the radix sort process.
   * 
   * @param arr Array to analyze
   */
  public static void printStatistics(int[] arr) {
    if (arr.length == 0) {
      System.out.println("Empty array");
      return;
    }

    int max = findMax(arr);
    int digits = String.valueOf(max).length();

    System.out.println("Array size: " + arr.length);
    System.out.println("Max value: " + max);
    System.out.println("Number of digits: " + digits);
    System.out.println("Number of passes: " + digits);
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
   * Utility method to print a string array
   */
  public static void printStringArray(String[] arr) {
    for (int i = 0; i < arr.length; i++) {
      System.out.print(arr[i] + " ");
    }
    System.out.println();
  }

  /**
   * Main method with example usage
   */
  public static void main(String[] args) {
    System.out.println("Radix Sort Examples:");

    // Test ascending sort with non-negative integers
    int[] arr1 = { 170, 45, 75, 90, 802, 24, 2, 66 };
    System.out.print("\nOriginal array: ");
    printArray(arr1);
    printStatistics(arr1);

    radixSort(arr1);
    System.out.print("Sorted array (ascending): ");
    printArray(arr1);

    // Test descending sort
    int[] arr2 = { 170, 45, 75, 90, 802, 24, 2, 66 };
    radixSortDescending(arr2);
    System.out.print("\nSorted array (descending): ");
    printArray(arr2);

    // Test with negative numbers
    int[] arr3 = { -5, -10, 0, -3, 8, 5, -1, 10 };
    System.out.print("\nArray with negative numbers: ");
    printArray(arr3);

    radixSortWithNegatives(arr3);
    System.out.print("Sorted array (with negatives): ");
    printArray(arr3);

    // Test with already sorted array
    int[] arr4 = { 1, 2, 3, 4, 5 };
    System.out.print("\nAlready sorted array: ");
    printArray(arr4);
    radixSort(arr4);
    System.out.print("After radix sort: ");
    printArray(arr4);

    // Test with single element
    int[] arr5 = { 42 };
    System.out.print("\nSingle element array: ");
    printArray(arr5);
    radixSort(arr5);
    System.out.print("After radix sort: ");
    printArray(arr5);

    // Test with same number of digits
    int[] arr6 = { 329, 457, 657, 839, 436, 720, 355 };
    System.out.print("\nArray with same digit count: ");
    printArray(arr6);
    radixSort(arr6);
    System.out.print("After radix sort: ");
    printArray(arr6);

    // Test string sorting
    String[] strArr = { "abc", "def", "aaa", "xyz", "zzz", "bbb" };
    System.out.print("\nString array: ");
    printStringArray(strArr);
    radixSortStrings(strArr);
    System.out.print("Sorted strings: ");
    printStringArray(strArr);
  }
}
