/**
 * Insertion Sort Algorithm
  * 
 * Time Complexity: O(n²) worst case, O(n) best case
 * Space Complexity: O(1)
  * 
 * Builds the sorted array one item at a time by inserting each element
  * into its correct position.
 */
class InsertionSort {
  /**
   * Sorts an array using the Insertion Sort algorithm (ascending order).
   * 
   * @param arr Array of numbers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    const n = arr.length;

    for (let i = 1; i < n; i++) {
      const key = arr[i];
      let j = i - 1;

      // Move elements greater than key one position ahead
      while (j >= 0 && arr[j] > key) {
        arr[j + 1] = arr[j];
        j--;
      }

      arr[j + 1] = key;
    }

    return arr;
  }

  /**
   * Sorts an array in descending order.
   */
  static sortDescending(arr: number[]): number[] {
    const n = arr.length;

    for (let i = 1; i < n; i++) {
      const key = arr[i];
      let j = i - 1;

      while (j >= 0 && arr[j] < key) {
        arr[j + 1] = arr[j];
        j--;
      }

      arr[j + 1] = key;
    }

    return arr;
  }
}
