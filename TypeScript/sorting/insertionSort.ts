/**
 * Insertion Sort Algorithm
 *
 * @description
 * Builds the sorted array one element at a time by 
 * inserting elements into their correct position.
 * Works efficiently for small or partially sorted arrays.
 *
 * @complexity
 * Time: O(n²)
 * Space: O(1)
 *
 * @param arr - The array of numbers to be sorted
 * @returns The sorted array (in ascending order)
 */
export function insertionSort(arr: number[]): number[] {
  const sortedArray = [...arr];

  for (let i = 1; i < sortedArray.length; i++) {
    const key = sortedArray[i];
    let j = i - 1;

    while (j >= 0 && sortedArray[j] > key) {
      sortedArray[j + 1] = sortedArray[j];
      j--;
    }
    sortedArray[j + 1] = key;
  }

  return sortedArray;
}
