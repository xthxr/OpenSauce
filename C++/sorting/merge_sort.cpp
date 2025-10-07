/*
  Merge Sort Implementation in C++
  --------------------------------

  Algorithm:
    Merge Sort is a divide-and-conquer, comparison-based sorting algorithm.
    It recursively divides the array into two halves, sorts each half, and then merges the sorted halves.
    The merging process ensures that all elements are ordered correctly in the final array.

  Optimization Used:
    - The algorithm divides until single-element arrays (base case).
    - Merging is efficient, using temporary arrays only for the merging phase.

  Time Complexity:
    Best Case:    O(n log n)
    Average Case: O(n log n)
    Worst Case:   O(n log n)

  Space Complexity:
    O(n) auxiliary space (temporary arrays for merging).

  Stability:
    Stable (relative order of equal elements is preserved).

  In-Place:
    Not strictly in-place (uses extra memory for merging).

  Use Cases:
    - Suitable for large datasets due to guaranteed O(n log n) performance.
    - Useful for linked lists and external sorting.

  Example Usage:
    Input:  arr = [5, 1, 4, 2, 3]
    Output: [1, 2, 3, 4, 5]
*/
#include <iostream>
#include <vector>
using namespace std;

// Merge two sorted halves into one sorted array
void merge(vector<int> &arr, int left, int mid, int right)
{
    int n1 = mid - left + 1; // length of left half
    int n2 = right - mid;    // length of right half

    // Create temporary arrays
    vector<int> L(n1), R(n2);

    // Copy data to temporary arrays
    for (int i = 0; i < n1; ++i)
        L[i] = arr[left + i];
    for (int j = 0; j < n2; ++j)
        R[j] = arr[mid + 1 + j];

    // Merge the temp arrays back into arr[left..right]
    int i = 0, j = 0, k = left;
    while (i < n1 && j < n2)
        arr[k++] = (L[i] <= R[j]) ? L[i++] : R[j++];

    // Copy any remaining elements of L
    while (i < n1)
        arr[k++] = L[i++];

    // Copy any remaining elements of R
    while (j < n2)
        arr[k++] = R[j++];
}

// Recursive Merge Sort
void mergeSort(vector<int> &arr, int left, int right)
{
    if (left < right)
    {
        int mid = left + (right - left) / 2;

        // Sort first and second halves
        mergeSort(arr, left, mid);
        mergeSort(arr, mid + 1, right);

        // Merge the sorted halves
        merge(arr, left, mid, right);
    }
}

int main()
{
    vector<int> arr = {5, 1, 4, 2, 3};
    cout << "Original array: ";
    for (int x : arr)
        cout << x << " ";
    cout << endl;

    mergeSort(arr, 0, arr.size() - 1);

    cout << "Sorted array: ";
    for (int x : arr)
        cout << x << " ";
    cout << endl;
    return 0;
}