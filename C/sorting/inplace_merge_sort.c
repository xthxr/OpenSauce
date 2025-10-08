/*
 * File: inplace_merge_sort.c
 * Description: Implementation of In-Place Merge Sort algorithm in C
 * Author: Gompa Rani Prasanna
 *
 * Algorithm:
 *   - This is a space-optimized version of Merge Sort that merges
 *     two sorted subarrays in-place, without using additional arrays.
 *   - It uses the standard divide-and-conquer approach:
 *       1. Recursively divide the array into halves.
 *       2. Sort each half.
 *       3. Merge the halves in-place using element shifting.
 *
 * Idea for In-Place Merge:
 *   - Compare elements from the left and right halves.
 *   - If an element in the right half is smaller, shift all elements
 *     of the left half to the right by one position and place it correctly.
 *   - This avoids auxiliary arrays but increases merge time.
 *
 * Time Complexity:
 *   - Best Case: O(n log n)
 *   - Average Case: O(n log n)
 *   - Worst Case: O(n^2)  (because of shifting elements during merge)
 *
 * Space Complexity: O(1) (in-place, no extra arrays)
 *
 * Example Usage:
 *   Input: arr = [5, 3, 8, 4, 2]
 *   Output: [2, 3, 4, 5, 8]
 */

#include <stdio.h>

// Function to merge two sorted subarrays in-place
// arr[l..m] and arr[m+1..r]
void inPlaceMerge(int arr[], int l, int m, int r) {
    int start = l;
    int mid = m + 1;

    // If the arrays are already sorted, no merge needed
    if (arr[m] <= arr[mid])
        return;

    // Merge process
    while (start <= m && mid <= r) {
        if (arr[start] <= arr[mid]) {
            start++;
        } else {
            // Element from right half smaller than left half element
            int value = arr[mid];
            int index = mid;

            // Shift all elements between start and mid right by one
            while (index != start) {
                arr[index] = arr[index - 1];
                index--;
            }
            arr[start] = value;

            // Update pointers
            start++;
            m++;
            mid++;
        }
    }
}

// Recursive merge sort function
void mergeSort(int arr[], int l, int r) {
    if (l < r) {
        int m = l + (r - l) / 2;

        // Sort both halves
        mergeSort(arr, l, m);
        mergeSort(arr, m + 1, r);

        // Merge them in-place
        inPlaceMerge(arr, l, m, r);
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

    printf("Original array:\n");
    printArray(arr, n);

    mergeSort(arr, 0, n - 1);

    printf("\nSorted array (In-place Merge Sort):\n");
    printArray(arr, n);

    return 0;
}
