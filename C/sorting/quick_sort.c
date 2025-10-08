/*
 * File: quick_sort.c
 * Description: Implementation of Quick Sort algorithm in C
 * Author: Gompa Rani Prasanna
 *
 * Algorithm:
 *   - Quick Sort is a Divide and Conquer algorithm.
 *   - It picks a pivot element, partitions the array into two halves
 *     (elements less than pivot and greater than pivot), and recursively
 *     sorts the halves.
 *
 * Steps:
 *   1. Choose a pivot (here we use the last element).
 *   2. Partition the array so that elements smaller than the pivot are on the left,
 *      and elements greater are on the right.
 *   3. Recursively apply the same logic to left and right subarrays.
 *
 * Time Complexity:
 *   - Best Case: O(n log n)    (balanced partition)
 *   - Average Case: O(n log n)
 *   - Worst Case: O(n^2)       (already sorted or reverse-sorted array with poor pivot choice)
 *
 * Space Complexity: O(log n)   (due to recursion stack)
 *
 * Example Usage:
 *   Input: arr = [10, 7, 8, 9, 1, 5]
 *   Output: [1, 5, 7, 8, 9, 10]
 */

#include <stdio.h>

// Function to swap two elements
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Partition function that places pivot at correct position
int partition(int arr[], int low, int high) {
    int pivot = arr[high]; // pivot element
    int i = (low - 1);     // index of smaller element

    for (int j = low; j < high; j++) {
        // If current element is smaller than or equal to pivot
        if (arr[j] <= pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }

    // Place pivot in correct position
    swap(&arr[i + 1], &arr[high]);
    return (i + 1);
}

// Recursive quick sort function
void quickSort(int arr[], int low, int high) {
    if (low < high) {
        // Partition index
        int pi = partition(arr, low, high);

        // Recursively sort elements before and after partition
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

// Function to print array
void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++)
        printf("%d ", arr[i]);
    printf("\n");
}

// Example usage
int main() {
    int arr[] = {10, 7, 8, 9, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);

    printf("Original array:\n");
    printArray(arr, n);

    quickSort(arr, 0, n - 1);

    printf("\nSorted array:\n");
    printArray(arr, n);

    return 0;
}
