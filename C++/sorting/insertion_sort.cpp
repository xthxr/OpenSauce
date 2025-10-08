/*

Insertion Sort is a simple and intuitive algorithm that works similar to how we sort playing cards in our hands.
It builds the sorted array one element at a time, by inserting each new element into its correct position among the already-sorted elements.

Algorithm Steps:

Assume the first element is already sorted.

Take the next element and compare it with elements in the sorted part.

Shift all greater elements one position ahead.

Insert the element in its correct position.

Time Complexity:

Best Case: O(n) (already sorted)

Average Case: O(n²)

Worst Case: O(n²)

Space Complexity: O(1)

Stability: Stable (order of equal elements is preserved)

*/


#include <iostream>
using namespace std;

// Function to perform Insertion Sort
void insertionSort(int arr[], int n) {
    for (int i = 1; i < n; i++) {
        int key = arr[i];
        int j = i - 1;

        // Move elements greater than key to one position ahead
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            j--;
        }
        arr[j + 1] = key; // Place the key in the correct position
    }
}

int main() {
    int arr[] = {5, 2, 9, 1, 5, 6};
    int n = sizeof(arr) / sizeof(arr[0]);

    insertionSort(arr, n);

    cout << "Sorted array: ";
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";

    return 0;
}
