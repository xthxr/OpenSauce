/**
 * Quick Sort Algorithm
 *
 * @description
 * A divide-and-conquer algorithm that picks a pivot element 
 * and partitions the array into elements less than and greater than the pivot,
 * then recursively sorts the partitions.
 *
 * @complexity
 * Time: O(n log n) average, O(n²) worst
 * Space: O(log n)
 *
 * @param arr - The array of numbers to be sorted
 * @returns The sorted array (in ascending order)
 */
export function quickSort(arr: number[]): number[] {
  if (arr.length <= 1) return arr;

  const pivot = arr[arr.length - 1];
  const left: number[] = [];
  const right: number[] = [];

  for (const num of arr.slice(0, -1)) {
    num < pivot ? left.push(num) : right.push(num);
  }

  return [...quickSort(left), pivot, ...quickSort(right)];
}
