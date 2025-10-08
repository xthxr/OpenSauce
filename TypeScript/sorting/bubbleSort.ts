/**
 * Bubble Sort Algorithm
 *
 * @description
 * Repeatedly steps through the array, compares adjacent elements, 
 * and swaps them if they are in the wrong order.
 * Best for small or nearly sorted datasets.
 *
 * @complexity
 * Time: O(n²)
 * Space: O(1)
 *
 * @param arr - The array of numbers to be sorted
 * @returns The sorted array (in ascending order)
 */
export function bubbleSort(arr: number[]): number[] {
  const n = arr.length;
  const sortedArray = [...arr];

  for (let i = 0; i < n - 1; i++) {
    for (let j = 0; j < n - i - 1; j++) {
      if (sortedArray[j] > sortedArray[j + 1]) {
        [sortedArray[j], sortedArray[j + 1]] = [sortedArray[j + 1], sortedArray[j]];
      }
    }
  }

  return sortedArray;
}
