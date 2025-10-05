/*  
  Bubble Sort Implementation in C++  
  --------------------------------  

  Algorithm:  
    Bubble Sort is a simple comparison-based sorting algorithm.  
    It repeatedly traverses the array, compares adjacent elements, and swaps them if they are in the wrong order.  
    This process is repeated until the array is sorted.  
    After each pass, the largest unsorted element "bubbles up" to its correct position at the end.

  Optimization Used:  
    This implementation includes an optimization where the algorithm stops early  
    if no swaps are made during a full pass — which shows the array is already sorted.

  Time Complexity:  
    Best Case:    O(n)      (when the array is already sorted — with the optimization)  
    Average Case: O(n^2)  
    Worst Case:   O(n^2)     (when the array is sorted in reverse order)

  Space Complexity:  
    O(1) auxiliary space (in-place sorting).

  Stability:  
    Stable (relative order of equal elements is preserved).

  In-Place:  
    Yes (uses swapping within the array/vector).

  Use Cases:  
    - Suitable for small datasets or educational purposes  
    - Not suitable for large datasets due to O(n^2) complexity.

  Example Usage:  
    Input:  arr = [5, 1, 4, 2, 3]  
    Output: [1, 2, 3, 4, 5]
*/
#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>
using namespace std;

// Bubble Sort Implementation
void bubbleSort(vector<int>& arr) {
    int n = arr.size();
    bool swapped;
    for (int i = 0; i < n - 1; ++i) {
        swapped = false;
        for (int j = 0; j < n - i - 1; ++j) {
            if (arr[j] > arr[j + 1]) {
                swap(arr[j], arr[j + 1]);
                swapped = true;
            }
        }
        // optimization- to stop if swaps are not happening
        if (!swapped) break;
    }
}

int main() {
    vector<int> arr = {5, 1, 4, 2, 3};
    cout <<"Original array: ";
    for (int x : arr) 
    {
      cout << x << " ";
    }
      cout <<endl;
    bubbleSort(arr);
    cout << "Sorted array: ";
    for (int x : arr) {
      cout << x << " ";
    }
    cout << endl;
    return 0;
}
