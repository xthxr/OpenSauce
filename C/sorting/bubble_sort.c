/*
 * File: bubble_sort.c
 * Description: Implementation of Bubble Sort algorithm in C
 * Author: Your Name
 * Date: YYYY-MM-DD
 *
 * Algorithm:
 *   - Repeatedly swap adjacent elements if they are in the wrong order.
 *   - After each pass, the largest element bubbles up to the end.
 *   - Continue until no swaps are needed (array is sorted).
 *
 * Time Complexity:
 *   - Best Case: O(n)      (already sorted, optimized with early exit)
 *   - Worst Case: O(n^2)   (reverse sorted array)
 *   - Average Case: O(n^2)
 *
 * Space Complexity: O(1) (in-place sorting)
 *
 * Example Usage:
 *   Input: arr = [5, 3, 8, 4, 2]
 *   Output: [2, 3, 4, 5, 8]
 */

#include <stdio.h>

// Function to perform bubble sort
void bubbleSort(int arr[], int n) {
    int i, j, temp;
    int swapped;

    for (i = 0; i < n - 1; i++) {
        swapped = 0; // flag to check if any swap happened in this pass

        for (j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                // Swap arr[j] and arr[j+1]
                temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
                swapped = 1;
            }
        }

        // If no swaps happened, the array is already sorted
        if (swapped == 0)
            break;
    }
}

// Utility function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++)
        printf("%d ", arr[i]);
    printf("\n");
}

// Example usage
int main() {
    int arr[] = {5, 3, 8, 4, 2};
    int n = sizeof(arr) / sizeof(arr[0]);

    printf("Original array: ");
    printArray(arr, n);

    bubbleSort(arr, n);

    printf("Sorted array:   ");
    printArray(arr, n);

    return 0;
}
