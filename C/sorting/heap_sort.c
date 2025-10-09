#include <stdio.h>

/*
 * Heap Sort Algorithm in C
 * ------------------------
 * Builds a max heap and sorts the array in ascending order.
 *
 * Time Complexity:
 *   - Building the heap: O(n)
 *   - Heap operations: O(log n) each
 *   - Total: O(n log n)
 *
 * Space Complexity: O(1)
 *
 * Example:
 *   Input:  [12, 11, 13, 5, 6, 7]
 *   Output: [5, 6, 7, 11, 12, 13]
 */

/* Swap two integers by pointer */
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

/* 
 * maxHeap: ensures the max-heap property for a given subtree 
 * root: index of the current node
 * n: size of the heap
 */
void maxHeap(int arr[], int n, int root) {
    int largest = root;
    int left = 2 * root + 1;
    int right = 2 * root + 2;

    if (left < n && arr[left] > arr[largest])
        largest = left;

    if (right < n && arr[right] > arr[largest])
        largest = right;

    if (largest != root) {
        swap(&arr[root], &arr[largest]);
        maxHeap(arr, n, largest);
    }
}

/* 
 * heapSort: builds a max heap and sorts the array
 */
void heapSort(int arr[], int n) {
    // Build max heap
    for (int i = n / 2 - 1; i >= 0; i--)
        maxHeap(arr, n, i);

    // Extract elements one by one
    for (int i = n - 1; i > 0; i--) {
        swap(&arr[0], &arr[i]);
        maxHeap(arr, i, 0);
    }
}

/* Helper function to print the array */
void printArray(int arr[], int n) {
    for (int i = 0; i < n; ++i)
        printf("%d ", arr[i]);
    printf("\n");
}

/* Demonstration */
int main() {
    int arr[] = {12, 11, 13, 5, 6, 7};
    int n = sizeof(arr) / sizeof(arr[0]);

    printf("Original array:\n");
    printArray(arr, n);

    heapSort(arr, n);

    printf("Sorted array:\n");
    printArray(arr, n);

    return 0;
}
