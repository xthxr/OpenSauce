/**
 * @fileoverview Implements the Binary Search algorithm for a sorted array.
 * @author HarshitPachori
 */

/**
 * Searches for a target value within a sorted array of numbers using the Binary Search algorithm.
 *
 * Binary search is an efficient algorithm for finding an item from a sorted list of items.
 * It works by repeatedly dividing in half the portion of the list that could contain the item,
 * until you've narrowed down the possible locations to just one.
 *
 * @param arr The sorted array of numbers to search within.
 * @param target The value to search for.
 * @returns The index of the target in the array, or -1 if the target is not found.
 *
 * @complexity
 * Time Complexity: O(log n) - The search space is halved in each iteration.
 * Space Complexity: O(1) - Only a few variables are used, regardless of the input size.
 */
function binarySearch(arr: number[], target: number): number {
    let low: number = 0;
    let high: number = arr.length - 1;

    while (low <= high) {
        // Calculate the middle index. Using bitwise shift for performance
        // and to prevent potential integer overflow compared to (low + high) / 2
        const mid: number = low + ((high - low) >> 1); // Equivalent to Math.floor((low + high) / 2)

        if (arr[mid] === target) {
            return mid; // Target found
        } else if (arr[mid] < target) {
            // Target is in the upper half
            low = mid + 1;
        } else {
            // Target is in the lower half
            high = mid - 1;
        }
    }

    return -1; // Target not found in the array
}

// --- Example Usage ---

const sortedArray: number[] = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91];

// 1. Target is present
const targetPresent: number = 23;
let resultPresent: number = binarySearch(sortedArray, targetPresent);
console.log(`Searching for ${targetPresent}: Found at index ${resultPresent} (Expected: 5)`);
// Output: Searching for 23: Found at index 5 (Expected: 5)

// 2. Target is not present
const targetAbsent: number = 42;
let resultAbsent: number = binarySearch(sortedArray, targetAbsent);
console.log(`Searching for ${targetAbsent}: Found at index ${resultAbsent} (Expected: -1)`);
// Output: Searching for 42: Found at index -1 (Expected: -1)

// 3. Target is the first element
const targetFirst: number = 2;
let resultFirst: number = binarySearch(sortedArray, targetFirst);
console.log(`Searching for ${targetFirst}: Found at index ${resultFirst} (Expected: 0)`);
// Output: Searching for 2: Found at index 0 (Expected: 0)

// 4. Target is the last element
const targetLast: number = 91;
let resultLast: number = binarySearch(sortedArray, targetLast);
console.log(`Searching for ${targetLast}: Found at index ${resultLast} (Expected: 9)`);
// Output: Searching for 91: Found at index 9 (Expected: 9)