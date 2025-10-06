/**
 * Bubble Sort Algorithm
 * 
 * Time Complexity: O(n²) worst case, O(n) best case
 * Space Complexity: O(1)
 * 
 * Compares adjacent elements and swaps them if they are in the wrong order.
 * Repeats the process until no swaps are needed.
 */
class BubbleSort {
  /**
   * Sorts an array using the Bubble Sort algorithm (ascending order).
   * 
   * @param arr Array of numbers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    const n = arr.length;

    for (let i = 0; i < n - 1; i++) {
      let swapped = false;

      // Last i elements are already in place
      for (let j = 0; j < n - i - 1; j++) {
        if (arr[j] > arr[j + 1]) {
          // Swap elements
          [arr[j], arr[j + 1]] = [arr[j + 1], arr[j]];
          swapped = true;
        }
      }

      // If no swaps were made, array is already sorted
      if (!swapped) break;
    }

    return arr;
  }

  /**
   * Sorts an array in descending order.
   */
  static sortDescending(arr: number[]): number[] {
    const n = arr.length;

    for (let i = 0; i < n - 1; i++) {
      let swapped = false;

      for (let j = 0; j < n - i - 1; j++) {
        if (arr[j] < arr[j + 1]) {
          [arr[j], arr[j + 1]] = [arr[j + 1], arr[j]];
          swapped = true;
        }
      }

      if (!swapped) break;
    }

    return arr;
  }
}
