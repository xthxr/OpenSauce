/*
 * File: heap_sort.c
 * Description: Implementation of Heap Sort algorithm in C
 * Author: VancentH
 * Date: 2025-10-10
 *
 * Algorithm:
 *   - Heap Sort builds a max-heap from the array, then repeatedly 
 *     swaps the root with the last element and reduces the heap 
 *     size until sorted.
 * 
 * Steps:
 *   1. Build a max heap from the input array
 *   2. Swap the root (largest element) with the last element
 *   3. Reduce heap size by one and heapify the root
 *   4. Repeat until the heap is empty
 * 
 * Time Complexity:
 *   - Best Case: O(n log n)
 *   - Average Case: O(n log n)
 *   - Worst Case: O(n log n)
 *
 * Space Complexity: O(1) (in-place sorting)
 * Stability: Not stable
 *
 * Example Usage:
 *   Input:  arr = [65, 11115, 48, 3, 233, 1, 2233]
 *   Output: arr = [1, 3, 48, 65, 233, 2233, 11115]
 */

#include <stdio.h>

// Swap two integer values
void swap(int *a, int *b)
{
  int temp = *a;
  *a = *b;
  *b = temp;
}

// Heapify function: maintains the heap property
void heapify(int arr[], int heapSize, int parentIndex)
{
  int largest = parentIndex;            // Initialize largest as parent node
  int leftChild = 2 * parentIndex + 1;  // Left child index
  int rightChild = 2 * parentIndex + 2; // Right child index

  // If left child is larger than parent
  if (leftChild < heapSize && arr[leftChild] > arr[largest])
    largest = leftChild;

  // If right child is larger than current largest
  if (rightChild < heapSize && arr[rightChild] > arr[largest])
    largest = rightChild;

  // If largest is not parent
  if (largest != parentIndex)
  {
    swap(&arr[parentIndex], &arr[largest]);
    // Recursively heapify the affected sub-tree
    heapify(arr, heapSize, largest);
  }
}

// Heap sort main function
void heapSort(int arr[], int n)
{
  // Build max heap (rearrange array)
  for (int i = n / 2 - 1; i >= 0; i--)
    heapify(arr, n, i);

  // Extract elements from heap one by one
  for (int i = n - 1; i >= 0; i--)
  {
    swap(&arr[0], &arr[i]); // Move current root to end
    heapify(arr, i, 0);     // Call heapify on the reduced heap
  }
}

// Print array contents
void print(int *arr, int n)
{
  for (int i = 0; i < n; i++)
    printf("%d, ", arr[i]);
  printf("\n");
}

int main()
{
  int arr[] = {65, 11115, 48, 3, 233, 1, 2233};
  int n = sizeof(arr) / sizeof(arr[0]);

  printf("Array length: %d\n", n);
  printf("Before sorting: ");
  print(arr, n);

  heapSort(arr, n);

  printf("After sorting: ");
  print(arr, n);

  return 0;
}