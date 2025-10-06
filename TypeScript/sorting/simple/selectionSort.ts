/**
 * Selection Sort Algorithm
 * 
 * Time Complexity: O(n²) in all cases
 * Space Complexity: O(1)
 * 
 * Repeatedly selects the smallest element from the unsorted portion
 * and moves it to the beginning.
 */
class SelectionSort {
  /**
   * Sorts an array using the Selection Sort algorithm (ascending order).
   * 
   * @param arr Array of numbers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    const n = arr.length;

    for (let i = 0; i < n - 1; i++) {
      // Find the index of the smallest element
      let minIdx = i;

      for (let j = i + 1; j < n; j++) {
        if (arr[j] < arr[minIdx]) {
          minIdx = j;
        }
      }

      // Swap the smallest element found with the first unsorted element
      if (minIdx !== i) {
        [arr[i], arr[minIdx]] = [arr[minIdx], arr[i]];
      }
    }

    return arr;
  }

  /**
   * Sorts an array in descending order.
   */
  static sortDescending(arr: number[]): number[] {
    const n = arr.length;

    for (let i = 0; i < n - 1; i++) {
      let maxIdx = i;

      for (let j = i + 1; j < n; j++) {
        if (arr[j] > arr[maxIdx]) {
          maxIdx = j;
        }
      }

      if (maxIdx !== i) {
        [arr[i], arr[maxIdx]] = [arr[maxIdx], arr[i]];
      }
    }

    return arr;
  }
}
