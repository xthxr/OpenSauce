/**
 * simple yet iterative quick sort :)
 * this sorts an array and returns a new sorted array using iterative Quick Sort.
 * 
 * Complexity
 * -----------
 * Time: O(n log n) on average, O(n^2) worst case
 * Space: O(n log n) for Best , O(n) - worst case, Since we are using call stack 
 * 
 * Example usage and its result
 * -----------
 * Original Array: [ 2, 3, 1, 6, 4 ]
 * Ascending: [ 1, 2, 3, 4, 6 ]
 * Descending: [ 6, 4, 3, 2, 1 ]
 */

/**
 * Quick Sort (recursive) with ascending/descending order option
 */

const partition = (arr, low, high, desc = false) => {
    let pivot = arr[low]; //* Assuming pivot element is arr[low] you can assume arr[high] but the code little bit changes
    let i = low;
    let j = high;

    const order = desc
        ? (a, b) => a >= b   //? descending
        : (a, b) => a <= b;  //? ascending

    while (i < j) {
        while (order(arr[i], pivot) && i < high) i++;
        while (!order(arr[j], pivot) && j > low) j--;

        if (i < j) {
            [arr[i], arr[j]] = [arr[j], arr[i]];
        }
    }

    // Place pivot in correct position
    [arr[low], arr[j]] = [arr[j], arr[low]];
    return j;
};

const quickSort = (arr, low = 0, high = arr.length - 1, desc = false) => {
    if (low < high) {
        const pIndex = partition(arr, low, high, desc);
        quickSort(arr, low, pIndex - 1, desc);
        quickSort(arr, pIndex + 1, high, desc);
    }
    return arr;
};

// Example usage
let arr = [2, 3, 1, 6, 4];
console.log("Original Array:", arr);
//! Remember i am using spread operator i.e (...)
console.log("Ascending:", quickSort([...arr]));         // [1, 2, 3, 4, 6]
console.log("Descending:", quickSort([...arr], 0, arr.length - 1, true)); // [6, 4, 3, 2, 1]
