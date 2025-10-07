/*
 * File: merge_sort.c
 * Description: Implementation of Merge Sort algorithm in C
 * Author: Gompa Rani Prasanna
 *
 * Algorithm:
 *   - Merge Sort is a Divide and Conquer algorithm.
 *   - It divides the array into two halves, recursively sorts them,
 *     and then merges the two sorted halves.
 *
 * Steps:
 *   1. Divide the array into two halves.
 *   2. Recursively sort both halves.
 *   3. Merge the two sorted halves into one sorted array.
 *
 * Time Complexity:
 *   - Best Case: O(n log n)
 *   - Average Case: O(n log n)
 *   - Worst Case: O(n log n)
 *
 * Space Complexity: O(n) (for the temporary arrays used during merging)
 *
 * Example Usage:
 *   Input: arr = [38, 27, 43, 3, 9, 82, 10]
 *   Output: [3, 9, 10, 27, 38, 43, 82]
 */

#include <stdio.h>
#include <stdlib.h>

// Function to merge two sorted subarrays
// arr[l..m] and arr[m+1..r]
void merge(int arr[], int l, int m, int r) {
    int n1 = m - l + 1;
    int n2 = r - m;

    // Create temporary arrays
    int* L = (int*)malloc(n1 * sizeof(int));
    int* R = (int*)malloc(n2 * sizeof(int));

    // Copy data to temp arrays L[] and R[]
    for (int i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (int j = 0; j < n2; j++)
        R[j] = arr[m + 1 + j];

    // Merge the temp arrays back into arr[l..r]
    int i = 0, j = 0, k = l;
    while (i < n1 && j < n2) {
        if (L[i] <= R[j])
            arr[k++] = L[i++];
        else
            arr[k++] = R[j++];
    }

    // Copy remaining elements of L[], if any
    while (i < n1)
        arr[k++] = L[i++];

    // Copy remaining elements of R[], if any
    while (j < n2)
        arr[k++] = R[j++];

    // Free temporary arrays
    free(L);
    free(R);
}

// Recursive function to perform merge sort
void mergeSort(int arr[], int l, int r) {
    if (l < r) {
        int m = l + (r - l) / 2; // same as (l + r)/2 but avoids overflow
        mergeSort(arr, l, m);    // sort first half
        mergeSort(arr, m + 1, r); // sort second half
        merge(arr, l, m, r);     // merge the two halves
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
    int arr[] = {38, 27, 43, 3, 9, 82, 10};
    int n = sizeof(arr) / sizeof(arr[0]);

    printf("Original array:\n");
    printArray(arr, n);

    mergeSort(arr, 0, n - 1);

    printf("\nSorted array:\n");
    printArray(arr, n);

    return 0;
}
