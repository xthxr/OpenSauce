/**
 * Radix Sort Algorithm
 * 
 * Time Complexity: O(d * (n + k)) where d is the number of digits
 * Space Complexity: O(n + k)
 * 
 * Non-comparative integer sorting algorithm that sorts numbers digit by digit
 * starting from the least significant digit to the most significant digit.
 */
class RadixSort {
  /**
   * Sorts an array using Radix Sort (ascending order).
   * Works only with non-negative integers.
   * 
   * @param arr Array of non-negative integers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    if (arr.length === 0) {
      return arr;
    }

    // Find the maximum number to know the number of digits
    const max = Math.max(...arr);

    // Perform counting sort for every digit
    // exp is 10^i where i is the current digit position
    for (let exp = 1; Math.floor(max / exp) > 0; exp *= 10) {
      this.countingSortByDigit(arr, exp);
    }

    return arr;
  }

  /**
   * Counting sort based on digit represented by exp.
   */
  private static countingSortByDigit(arr: number[], exp: number): void {
    const n = arr.length;
    const output = new Array(n);
    const count = new Array(10).fill(0);

    // Store count of occurrences
    for (let i = 0; i < n; i++) {
      const digit = Math.floor(arr[i] / exp) % 10;
      count[digit]++;
    }

    // Change count[i] to contain actual position
    for (let i = 1; i < 10; i++) {
      count[i] += count[i - 1];
    }

    // Build output array
    for (let i = n - 1; i >= 0; i--) {
      const digit = Math.floor(arr[i] / exp) % 10;
      output[count[digit] - 1] = arr[i];
      count[digit]--;
    }

    // Copy output array to arr
    for (let i = 0; i < n; i++) {
      arr[i] = output[i];
    }
  }

  /**
   * Sorts an array in descending order.
   */
  static sortDescending(arr: number[]): number[] {
    if (arr.length === 0) {
      return arr;
    }

    this.sort(arr);
    return arr.reverse();
  }

  /**
   * Sorts an array that may contain negative numbers.
   */
  static sortWithNegatives(arr: number[]): number[] {
    if (arr.length === 0) {
      return arr;
    }

    // Separate positive and negative numbers
    const negative: number[] = [];
    const positive: number[] = [];

    for (const num of arr) {
      if (num < 0) {
        negative.push(-num); // Convert to positive for sorting
      } else {
        positive.push(num);
      }
    }

    // Sort both arrays
    if (negative.length > 0) {
      this.sort(negative);
      // Reverse and convert back to negative
      for (let i = 0; i < negative.length; i++) {
        negative[i] = -negative[i];
      }
      negative.reverse();
    }

    if (positive.length > 0) {
      this.sort(positive);
    }

    // Merge arrays
    const result = [...negative, ...positive];
    for (let i = 0; i < arr.length; i++) {
      arr[i] = result[i];
    }

    return arr;
  }
}
