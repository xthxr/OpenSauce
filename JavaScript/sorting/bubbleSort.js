/**
 * Bubble Sort Algorithm
 *
 * @description Bubble Sort is a simple comparison-based sorting algorithm. It repeatedly steps through the list, compares adjacent elements, and swaps them if they are in the wrong order. This process is repeated until the list is sorted.
 *
 * ### How it Works:
 * 1.  **Iterate through the array**: Start from the first element and compare it with the second.
 * 2.  **Compare and Swap**: If the first element is greater than the second, swap them.
 * 3.  **Move to the next pair**: Continue this process for each pair of adjacent elements to the end of the array. This completes one "pass". At the end of the first pass, the largest element will have "bubbled up" to the end of the array.
 * 4.  **Repeat**: Repeat the passes, each time reducing the number of elements to be sorted by one (since the largest elements are already in place).
 * 5.  **Optimization**: If a pass completes with no swaps, the array is already sorted, and the algorithm can stop early.
 *
 * ### Complexity Analysis:
 *
 * -   **Time Complexity**:
 * -   **Best Case**: `O(n)` - This occurs when the array is already sorted. With the optimization to stop when no swaps are made, it only takes one pass to confirm this.
 * -   **Average Case**: `O(n^2)` - This occurs when the elements are in a jumbled order.
 * -   **Worst Case**: `O(n^2)` - This occurs when the array is sorted in reverse order, requiring the maximum number of swaps.
 *
 * -   **Space Complexity**: `O(1)` - Bubble sort is an in-place sorting algorithm, meaning it doesn't require any extra space that scales with the input size.
 *
 * @param {number[]} arr - The array of numbers to be sorted.
 * @returns {number[]} The sorted array.
 *
 * @example
 * const unsortedArray = [64, 34, 25, 12, 22, 11, 90];
 * const sortedArray = bubbleSort(unsortedArray);
 * console.log(sortedArray); // Output: [11, 12, 22, 25, 34, 64, 90]
 *
 * @example
 * const anotherArray = [5, 1, 4, 2, 8];
 * console.log(bubbleSort(anotherArray)); // Output: [1, 2, 4, 5, 8]
 */
function bubbleSort(arr) {
    const n = arr.length;
    let swapped;

    for (let i = 0; i < n - 1; i++) {
        swapped = false;
        // The last i elements are already in place, so we don't need to check them
        for (let j = 0; j < n - i - 1; j++) {
            // Swap if the element found is greater than the next element
            if (arr[j] > arr[j + 1]) {
                // ES6 destructuring for a clean swap
                [arr[j], arr[j + 1]] = [arr[j + 1], arr[j]];
                swapped = true;
            }
        }

        // If no two elements were swapped by inner loop, then break
        if (!swapped) {
            break;
        }
    }

    return arr;
}

// Example usage and test cases
if (typeof module !== 'undefined' && !module.parent) {
    console.log("Example usage:");
    const testArray1 = [64, 34, 25, 12, 22, 11, 90];
    console.log("Original Array:", testArray1);
    console.log("Sorted Array:  ", bubbleSort([...testArray1])); // Use spread to avoid modifying the original array for demonstration

    console.log("\nAnother example:");
    const testArray2 = [5, 1, 4, 2, 8];
    console.log("Original Array:", testArray2);
    console.log("Sorted Array:  ", bubbleSort([...testArray2]));
}