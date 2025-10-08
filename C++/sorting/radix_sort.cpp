/*

Radix Sort is a non-comparison-based sorting algorithm.
It sorts integers by processing individual digits.
It uses a stable sub-algorithm (like Counting Sort) to sort digits from least significant to most significant.

Algorithm Steps:

Find the maximum number to know the number of digits.

Sort elements digit by digit using Counting Sort as a subroutine.

Start from the least significant digit (LSD) and move to the most significant digit (MSD).

Time Complexity:

O(n × k), where k = number of digits.

Space Complexity: O(n + k)

Stability: ✅ Stable

*/

#include <iostream>
#include <vector>
using namespace std;

// Function to get the maximum value in arr[]
int getMax(int arr[], int n) {
    int maxVal = arr[0];
    for (int i = 1; i < n; i++)
        if (arr[i] > maxVal)
            maxVal = arr[i];
    return maxVal;
}

// Counting sort used by Radix Sort
void countingSort(int arr[], int n, int exp) {
     vector<int> output(n);
    int count[10] = {0};

    // Count occurrences of digits
    for (int i = 0; i < n; i++)
        count[(arr[i] / exp) % 10]++;

    // Change count[i] to actual positions
    for (int i = 1; i < 10; i++)
        count[i] += count[i - 1];

    // Build output array
    for (int i = n - 1; i >= 0; i--) {
        output[count[(arr[i] / exp) % 10] - 1] = arr[i];
        count[(arr[i] / exp) % 10]--;
    }

    // Copy output to arr[]
    for (int i = 0; i < n; i++)
        arr[i] = output[i];
}

// Main Radix Sort function
void radixSort(int arr[], int n) {
    int maxVal = getMax(arr, n);
    for (int exp = 1; maxVal / exp > 0; exp *= 10)
        countingSort(arr, n, exp);
}

int main() {
    int arr[] = {170, 45, 75, 90, 802, 24, 2, 66};
    int n = sizeof(arr) / sizeof(arr[0]);

    radixSort(arr, n);

    cout << "Sorted array: ";
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";

    return 0;
}
