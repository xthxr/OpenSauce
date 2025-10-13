#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Linear Search Algorithm
// --------------------------------------------------------------------------------
// Description:
// Linear search (or sequential search) is the most basic method for finding a target
// value within a list or array. It sequentially checks each element until a match 
// is found or the entire array has been searched.

// Time Complexity:
// Worst Case: O(n) - The target element is at the end or not present.
// Average Case: O(n)
// Best Case: O(1) - The target element is the first element.

// Space Complexity:
// Auxiliary Space: O(1) - Only constant extra space is used for variables.
// --------------------------------------------------------------------------------

/**
 * Performs a linear search on an integer array.
 * @param arr The array to search through.
 * @param size The number of elements in the array.
 * @param target The value to search for.
 * @return The index of the target element, or -1 if not found.
 */
int linear_search(int arr[], int size, int target) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == target) {
            return i; // Element found at index i
        }
    }
    return -1; // Element not found
}

// Example Usage / User Input Test Case
int main() {
    int size;
    int target;

    // 1. Get Array Size
    printf("Enter the number of elements in the array: ");
    if (scanf("%d", &size) != 1 || size <= 0) {
        printf("Invalid size. Exiting.\n");
        return 1;
    }

    // Allocate memory for the array
    int* arr = (int*)malloc(size * sizeof(int));
    if (arr == NULL) {
        printf("Memory allocation failed. Exiting.\n");
        return 1;
    }

    // 2. Get Array Elements
    printf("Enter %d integers, separated by spaces:\n", size);
    for (int i = 0; i < size; i++) {
        if (scanf("%d", &arr[i]) != 1) {
            printf("Invalid input. Exiting.\n");
            free(arr);
            return 1;
        }
    }
    
    // 3. Get Target Element
    printf("Enter the element to search for (target): ");
    if (scanf("%d", &target) != 1) {
        printf("Invalid target input. Exiting.\n");
        free(arr);
        return 1;
    }

    // 4. Perform Search and Display Result
    int result = linear_search(arr, size, target);

    if (result != -1) {
        printf("\nResult: Element %d found at index %d.\n", target, result);
    } else {
        printf("\nResult: Element %d not found in the array.\n", target);
    }

    // Free allocated memory
    free(arr);
    return 0;
}