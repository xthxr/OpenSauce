/**
 * Heap Sort Algorithm
 * 
 * Time Complexity: O(n log n) in all cases
 * Space Complexity: O(1)
 * 
 * Uses a binary heap data structure to sort the array.
 * First builds a max heap, then repeatedly extracts the maximum element.
 */
class HeapSort {
  /**
   * Sorts an array using Heap Sort algorithm (ascending order).
   * 
   * @param arr Array of numbers to be sorted
   * @return Sorted array
   */
  static sort(arr: number[]): number[] {
    const n = arr.length;

    // Build max heap
    for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
      this.heapify(arr, n, i);
    }

    // Extract elements from heap one by one
    for (let i = n - 1; i > 0; i--) {
      // Move current root to end
      [arr[0], arr[i]] = [arr[i], arr[0]];

      // Heapify the reduced heap
      this.heapify(arr, i, 0);
    }

    return arr;
  }

  /**
   * Heapify a subtree rooted at index i.
   * 
   * @param arr Array to heapify
   * @param n Size of heap
   * @param i Root index of subtree
   */
  private static heapify(arr: number[], n: number, i: number): void {
    let largest = i;
    const left = 2 * i + 1;
    const right = 2 * i + 2;

    // If left child is larger than root
    if (left < n && arr[left] > arr[largest]) {
      largest = left;
    }

    // If right child is larger than largest so far
    if (right < n && arr[right] > arr[largest]) {
      largest = right;
    }

    // If largest is not root
    if (largest !== i) {
      [arr[i], arr[largest]] = [arr[largest], arr[i]];

      // Recursively heapify the affected subtree
      this.heapify(arr, n, largest);
    }
  }

  /**
   * Sorts an array in descending order.
   */
  static sortDescending(arr: number[]): number[] {
    const n = arr.length;

    // Build min heap (for descending order)
    for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
      this.heapifyMin(arr, n, i);
    }

    // Extract elements from heap one by one
    for (let i = n - 1; i > 0; i--) {
      [arr[0], arr[i]] = [arr[i], arr[0]];
      this.heapifyMin(arr, i, 0);
    }

    return arr;
  }

  /**
   * Heapify for min heap.
   */
  private static heapifyMin(arr: number[], n: number, i: number): void {
    let smallest = i;
    const left = 2 * i + 1;
    const right = 2 * i + 2;

    if (left < n && arr[left] < arr[smallest]) {
      smallest = left;
    }

    if (right < n && arr[right] < arr[smallest]) {
      smallest = right;
    }

    if (smallest !== i) {
      [arr[i], arr[smallest]] = [arr[smallest], arr[i]];
      this.heapifyMin(arr, n, smallest);
    }
  }
}
