/**
 * Merge Sort Algorithm
 * 
 * Time Complexity: O(n log n) in all cases
 * Space Complexity: O(n)
 * 
 * Divide-and-conquer algorithm that divides the array into halves,
 * sorts them recursively, and then merges the sorted halves.
 */
class MergeSort {
  /**
   * Sorts an array using the Merge Sort algorithm (ascending order).
   * 
   * @param arr Array of numbers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    if (arr.length <= 1) {
      return arr;
    }

    return this.mergeSort(arr, 0, arr.length - 1);
  }

  /**
   * Recursive merge sort helper.
   */
  private static mergeSort(arr: number[], left: number, right: number): number[] {
    if (left >= right) {
      return arr;
    }

    const mid = Math.floor((left + right) / 2);

    this.mergeSort(arr, left, mid);
    this.mergeSort(arr, mid + 1, right);
    this.merge(arr, left, mid, right);

    return arr;
  }

  /**
   * Merges two sorted subarrays.
   */
  private static merge(arr: number[], left: number, mid: number, right: number): void {
    const leftSize = mid - left + 1;
    const rightSize = right - mid;

    // Create temporary arrays
    const leftArr = new Array(leftSize);
    const rightArr = new Array(rightSize);

    // Copy data to temporary arrays
    for (let i = 0; i < leftSize; i++) {
      leftArr[i] = arr[left + i];
    }
    for (let j = 0; j < rightSize; j++) {
      rightArr[j] = arr[mid + 1 + j];
    }

    // Merge the temporary arrays back
    let i = 0, j = 0, k = left;

    while (i < leftSize && j < rightSize) {
      if (leftArr[i] <= rightArr[j]) {
        arr[k] = leftArr[i];
        i++;
      } else {
        arr[k] = rightArr[j];
        j++;
      }
      k++;
    }

    // Copy remaining elements
    while (i < leftSize) {
      arr[k] = leftArr[i];
      i++;
      k++;
    }

    while (j < rightSize) {
      arr[k] = rightArr[j];
      j++;
      k++;
    }
  }

  /**
   * Sorts an array in descending order.
   */
  static sortDescending(arr: number[]): number[] {
    if (arr.length <= 1) {
      return arr;
    }

    this.sort(arr);
    return arr.reverse();
  }
}
