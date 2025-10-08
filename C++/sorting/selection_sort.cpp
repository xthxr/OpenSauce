/*

Selection Sort is a simple comparison-based sorting algorithm.
It works by repeatedly finding the smallest (or largest) element from the unsorted part of the array and placing it at the correct position in the sorted part.

Algorithm Steps:

Start from the first element, assume it as the minimum.

Compare it with all other elements to find the smallest.

Swap the smallest element with the first element.

Repeat for the remaining unsorted part of the array.

Time Complexity:

Best Case: O(n²)

Average Case: O(n²)

Worst Case: O(n²)

Space Complexity: O(1) (in-place sorting)

Stability: Not stable (order of equal elements may change)

*/

#include <iostream>
using namespace std;

// Function to perform Selection Sort
void selectionSort(int arr[], int n) {
    for (int i = 0; i < n - 1; i++) {
        int minIndex = i;  // Assume current index has the smallest value
        for (int j = i + 1; j < n; j++) {
            if (arr[j] < arr[minIndex])
                minIndex = j;  // Update index of smaller element
        }
        // Swap the found minimum element with the first element
        swap(arr[i], arr[minIndex]);
    }
}

int main() {
    int arr[] = {64, 25, 12, 22, 11};
    int n = sizeof(arr) / sizeof(arr[0]);

    selectionSort(arr, n);

    cout << "Sorted array: ";
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";

    return 0;
}
