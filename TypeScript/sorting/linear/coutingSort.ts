/**
 * Counting Sort Algorithm
 * 
 * Time Complexity: O(n + k) where n is the number of elements and k is the range
 * Space Complexity: O(k)
 * 
 * Integer sorting algorithm that operates by counting the number of objects
 * that have distinct key values.
 */
class CountingSort {
  /**
   * Sorts an array using Counting Sort (ascending order).
   * Works only with non-negative integers.
   * 
   * @param arr Array of non-negative integers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    if (arr.length === 0) {
      return arr;
    }

    const max = Math.max(...arr);
    return this.sortByRange(arr, max);
  }

  /**
   * Sorts an array using counting sort with a specified range.
   */
  private static sortByRange(arr: number[], max: number): number[] {
    const n = arr.length;

    // Create counting array
    const count = new Array(max + 1).fill(0);

    // Store count of each element
    for (let i = 0; i < n; i++) {
      count[arr[i]]++;
    }

    // Modify count array to store actual positions
    for (let i = 1; i <= max; i++) {
      count[i] += count[i - 1];
    }

    // Build output array
    const output = new Array(n);

    // Place elements in sorted order (traverse from right to maintain stability)
    for (let i = n - 1; i >= 0; i--) {
      output[count[arr[i]] - 1] = arr[i];
      count[arr[i]]--;
    }

    // Copy output array to original array
    for (let i = 0; i < n; i++) {
      arr[i] = output[i];
    }

    return arr;
  }

  /**
   * Sorts an array in descending order.
   */
  static sortDescending(arr: number[]): number[] {
    if (arr.length === 0) {
      return arr;
    }

    const n = arr.length;
    const max = Math.max(...arr);

    // Create counting array
    const count = new Array(max + 1).fill(0);

    // Store count of each element
    for (let i = 0; i < n; i++) {
      count[arr[i]]++;
    }

    // Place elements in descending order
    let index = 0;
    for (let i = max; i >= 0; i--) {
      while (count[i] > 0) {
        arr[index++] = i;
        count[i]--;
      }
    }

    return arr;
  }

  /**
   * Sorts an array that may contain negative numbers.
   */
  static sortWithNegatives(arr: number[]): number[] {
    if (arr.length === 0) {
      return arr;
    }

    const n = arr.length;

    // Find minimum and maximum elements
    let min = arr[0];
    let max = arr[0];
    for (let i = 1; i < n; i++) {
      if (arr[i] < min) min = arr[i];
      if (arr[i] > max) max = arr[i];
    }

    const range = max - min + 1;

    // Create counting array
    const count = new Array(range).fill(0);

    // Store count of each element (with offset for negative numbers)
    for (let i = 0; i < n; i++) {
      count[arr[i] - min]++;
    }

    // Modify count array to store actual positions
    for (let i = 1; i < range; i++) {
      count[i] += count[i - 1];
    }

    // Build output array
    const output = new Array(n);

    for (let i = n - 1; i >= 0; i--) {
      output[count[arr[i] - min] - 1] = arr[i];
      count[arr[i] - min]--;
    }

    // Copy output array to original array
    for (let i = 0; i < n; i++) {
      arr[i] = output[i];
    }

    return arr;
  }
}
