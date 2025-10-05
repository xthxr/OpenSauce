/*
 Quick Sort Implementation in C++
 --------------------------------
 Algorithm:
   Quick Sort is a Divide and Conquer algorithm. It picks a pivot element and partitions the array such that
   elements smaller than pivot are on the left, elements greater than pivot are on the right. It then
   recursively sorts the left and right partitions.

 Pivot Strategy Used:
   This implementation uses median-of-three (first, middle, last) to choose a pivot which helps reduce
   worst-case performance on already sorted or nearly sorted inputs compared to always picking the first or last element.

 Time Complexity:
   Best Case:    O(n log n)   (balanced partitions)
   Average Case: O(n log n)
   Worst Case:   O(n^2)       (highly unbalanced partitions, e.g., already sorted with poor pivot choice)

 Space Complexity:
   O(log n) auxiliary stack space on average due to recursion (worst O(n) in skewed recursion tree).

 Stability:
   Not stable (relative order of equal elements may change).

 In-Place:
   Yes (uses swapping within the array/vector, aside from recursion stack).

 Use Cases:
   Efficient general-purpose sorter when average-case performance matters and stability is not required.

 Example Usage:
   Input:  arr = [9, 3, 7, 1, 6, 2, 8]
   Output: [1, 2, 3, 6, 7, 8, 9]

 Compile and Run:
   g++ -std=c++17 -O2 quick_sort.cpp -o quick_sort
   ./quick_sort
*/

// Standard library includes (avoid non-standard <bits/stdc++.h> for portability)
#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>

using namespace std; // Acceptable for competitive/DSA contexts; consider removing in larger projects

// Partition using Lomuto scheme after choosing median-of-three pivot
int partition(vector<int>& arr, int low, int high) {
    // Median-of-three pivot selection
    int mid = low + (high - low) / 2;
    if (arr[mid] < arr[low]) swap(arr[mid], arr[low]);
    if (arr[high] < arr[low]) swap(arr[high], arr[low]);
    if (arr[mid] < arr[high]) swap(arr[mid], arr[high]);
    int pivot = arr[high];

    int i = low - 1;
    for (int j = low; j < high; ++j) {
        if (arr[j] <= pivot) { // <= keeps partition stable-ish for duplicates distribution
            ++i;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1;
}

void quickSort(vector<int>& arr, int low, int high) {
    // Tail call optimization style: always recurse on smaller partition first
    while (low < high) {
        // Switch to insertion sort for very small subarrays to improve constant factors
        if (high - low <= 16) {
            for (int i = low + 1; i <= high; ++i) {
                int key = arr[i];
                int j = i - 1;
                while (j >= low && arr[j] > key) {
                    arr[j + 1] = arr[j];
                    --j;
                }
                arr[j + 1] = key;
            }
            return;
        }

        int p = partition(arr, low, high);

        // Recurse on smaller side to ensure O(log n) stack depth
        if (p - 1 - low < high - (p + 1)) {
            quickSort(arr, low, p - 1);
            low = p + 1; // tail recurse on right side via loop
        } else {
            quickSort(arr, p + 1, high);
            high = p - 1; // continue with left side
        }
    }
}

// Convenience overload for users
void quickSort(vector<int>& arr) {
    if (!arr.empty()) quickSort(arr, 0, (int)arr.size() - 1);
}

// Simple test / demonstration
int main() {
    vector<int> data = {9, 3, 7, 1, 6, 2, 8, 5, 4, 4, 10};
    cout << "Original: ";
    for (int x : data) cout << x << ' '; cout << '\n';

    quickSort(data);

    cout << "Sorted:   ";
    for (int x : data) cout << x << ' '; cout << '\n';

    // Basic validation
    if (is_sorted(data.begin(), data.end())) {
        cout << "Array is sorted.\n";
    } else {
        cout << "Array is NOT sorted!\n";
    }
    return 0;
}
