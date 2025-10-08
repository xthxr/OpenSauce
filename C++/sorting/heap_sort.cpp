/*

Heap Sort is a comparison-based sorting algorithm that uses a binary heap data structure.
It first builds a max-heap (where the largest element is at the root), then repeatedly swaps the root with the last element and reduces the heap size until all elements are sorted.

Algorithm Steps:

Build a max heap from the input array.

Swap the root (largest element) with the last element.

Reduce heap size by one and heapify the root.

Repeat until the heap is empty.

Time Complexity:

Best Case: O(n log n)

Average Case: O(n log n)

Worst Case: O(n log n)

Space Complexity: O(1) (in-place sorting)

Stability: ❌ Not stable

*/

#include <iostream>
using namespace std;

// Function to heapify a subtree rooted with node i
void heapify(int arr[], int n, int i) {
    int largest = i;         // Initialize largest as root
    int left = 2 * i + 1;    // Left child
    int right = 2 * i + 2;   // Right child

    if (left < n && arr[left] > arr[largest])
        largest = left;

    if (right < n && arr[right] > arr[largest])
        largest = right;

    if (largest != i) {
        swap(arr[i], arr[largest]);
        heapify(arr, n, largest);
    }
}

// Main Heap Sort function
void heapSort(int arr[], int n) {
    // Build max heap
    for (int i = n / 2 - 1; i >= 0; i--)
        heapify(arr, n, i);

    // Extract elements from heap one by one
    for (int i = n - 1; i > 0; i--) {
        swap(arr[0], arr[i]);      // Move current root to end
        heapify(arr, i, 0);        // Heapify reduced heap
    }
}

int main() {
    int arr[] = {12, 11, 13, 5, 6, 7};
    int n = sizeof(arr) / sizeof(arr[0]);

    heapSort(arr, n);

    cout << "Sorted array: ";
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";

    return 0;
}
