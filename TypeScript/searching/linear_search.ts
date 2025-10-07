/**
 * @fileoverview Implements the Linear Search algorithm for an array.
 * @author HarshitPachori
 */

/**
 * Searches for a target value within an array by checking each element sequentially.
 *
 * Linear search is the simplest search algorithm. It checks every element in the list
 * until the desired element is found or the end of the list is reached.
 * Unlike Binary Search, the array **does not** need to be sorted.
 *
 * @param arr The array of numbers to search within.
 * @param target The value to search for.
 * @returns The index of the target in the array, or -1 if the target is not found.
 *
 * @complexity
 * Time Complexity: O(n) - In the worst case, the entire array must be scanned.
 * Space Complexity: O(1) - Only a few variables are used, regardless of the input size.
 */
function linearSearch(arr: number[], target: number): number {
    // Iterate through the array from the start
    for (let i: number = 0; i < arr.length; i++) {
        // Check if the current element is the target
        if (arr[i] === target) {
            return i; // Target found at index i
        }
    }

    return -1; // Target not found after checking all elements
}

// --- Example Usage ---

const unsortedArray: number[] = [34, 1, 8, 55, 22, 99, 16];

// 1. Target is present near the end
const targetPresentEnd: number = 99;
let resultPresentEnd: number = linearSearch(unsortedArray, targetPresentEnd);
console.log(`Searching for ${targetPresentEnd}: Found at index ${resultPresentEnd} (Expected: 5)`);
// Output: Searching for 99: Found at index 5 (Expected: 5)

// 2. Target is present at the start
const targetPresentStart: number = 34;
let resultPresentStart: number = linearSearch(unsortedArray, targetPresentStart);
console.log(`Searching for ${targetPresentStart}: Found at index ${resultPresentStart} (Expected: 0)`);
// Output: Searching for 34: Found at index 0 (Expected: 0)

// 3. Target is not present
const targetAbsent: number = 42;
let resultAbsent: number = linearSearch(unsortedArray, targetAbsent);
console.log(`Searching for ${targetAbsent}: Found at index ${resultAbsent} (Expected: -1)`);
// Output: Searching for 42: Found at index -1 (Expected: -1)