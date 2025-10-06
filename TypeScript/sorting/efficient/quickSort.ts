/**
 * Quick Sort Algorithm
 * 
 * Time Complexity: O(n log n) average, O(n²) worst case
 * Space Complexity: O(log n) due to recursion
 * 
 * Divide-and-conquer algorithm that selects a pivot element and partitions
 * the array around the pivot.
 */
class QuickSort {
  /**
   * Sorts an array using Quick Sort with last element as pivot (ascending order).
   * 
   * @param arr Array of numbers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    if (arr.length <= 1) {
      return arr;
    }

    return this.quickSort(arr, 0, arr.length - 1);
  }

  /**
   * Quick Sort with first element as pivot.
   */
  static sortWithFirstPivot(arr: number[]): number[] {
    if (arr.length <= 1) {
      return arr;
    }

    return this.quickSortFirstPivot(arr, 0, arr.length - 1);
  }

  /**
   * Quick Sort with middle element as pivot.
   */
  static sortWithMiddlePivot(arr: number[]): number[] {
    if (arr.length <= 1) {
      return arr;
    }

    return this.quickSortMiddlePivot(arr, 0, arr.length - 1);
  }

  /**
   * Quick Sort with random pivot.
   */
  static sortWithRandomPivot(arr: number[]): number[] {
    if (arr.length <= 1) {
      return arr;
    }

    return this.quickSortRandomPivot(arr, 0, arr.length - 1);
  }

  /**
   * Recursive quick sort (last element as pivot).
   */
  private static quickSort(arr: number[], low: number, high: number): number[] {
    if (low < high) {
      const pi = this.partitionLast(arr, low, high);

      this.quickSort(arr, low, pi - 1);
      this.quickSort(arr, pi + 1, high);
    }

    return arr;
  }

  /**
   * Recursive quick sort (first element as pivot).
   */
  private static quickSortFirstPivot(arr: number[], low: number, high: number): number[] {
    if (low < high) {
      const pi = this.partitionFirst(arr, low, high);

      this.quickSortFirstPivot(arr, low, pi - 1);
      this.quickSortFirstPivot(arr, pi + 1, high);
    }

    return arr;
  }

  /**
   * Recursive quick sort (middle element as pivot).
   */
  private static quickSortMiddlePivot(arr: number[], low: number, high: number): number[] {
    if (low < high) {
      const pi = this.partitionMiddle(arr, low, high);

      this.quickSortMiddlePivot(arr, low, pi - 1);
      this.quickSortMiddlePivot(arr, pi + 1, high);
    }

    return arr;
  }

  /**
   * Recursive quick sort (random pivot).
   */
  private static quickSortRandomPivot(arr: number[], low: number, high: number): number[] {
    if (low < high) {
      const pi = this.partitionRandom(arr, low, high);

      this.quickSortRandomPivot(arr, low, pi - 1);
      this.quickSortRandomPivot(arr, pi + 1, high);
    }

    return arr;
  }

  /**
   * Partition using last element as pivot.
   */
  private static partitionLast(arr: number[], low: number, high: number): number {
    const pivot = arr[high];
    let i = low - 1;

    for (let j = low; j < high; j++) {
      if (arr[j] < pivot) {
        i++;
        [arr[i], arr[j]] = [arr[j], arr[i]];
      }
    }

    [arr[i + 1], arr[high]] = [arr[high], arr[i + 1]];
    return i + 1;
  }

  /**
   * Partition using first element as pivot.
   */
  private static partitionFirst(arr: number[], low: number, high: number): number {
    // Move first element to end and use regular partition
    [arr[low], arr[high]] = [arr[high], arr[low]];
    return this.partitionLast(arr, low, high);
  }

  /**
   * Partition using middle element as pivot.
   */
  private static partitionMiddle(arr: number[], low: number, high: number): number {
    const mid = Math.floor((low + high) / 2);
    [arr[mid], arr[high]] = [arr[high], arr[mid]];
    return this.partitionLast(arr, low, high);
  }

  /**
   * Partition using random pivot.
   */
  private static partitionRandom(arr: number[], low: number, high: number): number {
    const randomIdx = Math.floor(Math.random() * (high - low + 1)) + low;
    [arr[randomIdx], arr[high]] = [arr[high], arr[randomIdx]];
    return this.partitionLast(arr, low, high);
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
