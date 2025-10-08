/*
 * Merge Sort Algorithm
 * 
 * Time Complexity: O(n log n) - Best, Average, Worst case
 * Space Complexity: O(n) - Additional space for temporary arrays
 * 
 * Merge Sort is a stable, divide-and-conquer sorting algorithm that works by
 * recursively dividing the array into two halves, sorting them, and then merging them.
 * It guarantees O(n log n) time complexity in all cases.
 */

#include <iostream>
#include <vector>
#include <chrono>
using namespace std;

/**
 * Merge two sorted subarrays into a single sorted array
 * 
 * @param arr Original array
 * @param left Starting index of left subarray
 * @param mid Middle index (end of left subarray)
 * @param right Ending index of right subarray
 */
void merge(vector<int>& arr, int left, int mid, int right) {
    // Calculate sizes of two subarrays to be merged
    int n1 = mid - left + 1;
    int n2 = right - mid;
    
    // Create temporary arrays
    vector<int> leftArr(n1);
    vector<int> rightArr(n2);
    
    // Copy data to temporary arrays
    for (int i = 0; i < n1; i++) {
        leftArr[i] = arr[left + i];
    }
    for (int j = 0; j < n2; j++) {
        rightArr[j] = arr[mid + 1 + j];
    }
    
    // Merge the temporary arrays back into arr[left..right]
    int i = 0;    // Initial index of first subarray
    int j = 0;    // Initial index of second subarray
    int k = left; // Initial index of merged subarray
    
    while (i < n1 && j < n2) {
        if (leftArr[i] <= rightArr[j]) {
            arr[k] = leftArr[i];
            i++;
        } else {
            arr[k] = rightArr[j];
            j++;
        }
        k++;
    }
    
    // Copy remaining elements of leftArr[], if any
    while (i < n1) {
        arr[k] = leftArr[i];
        i++;
        k++;
    }
    
    // Copy remaining elements of rightArr[], if any
    while (j < n2) {
        arr[k] = rightArr[j];
        j++;
        k++;
    }
}

/**
 * Main merge sort function that sorts arr[left..right] using merge()
 * 
 * @param arr Array to be sorted
 * @param left Starting index
 * @param right Ending index
 */
void mergeSort(vector<int>& arr, int left, int right) {
    if (left < right) {
        // Find the middle point
        int mid = left + (right - left) / 2;
        
        // Sort first and second halves
        mergeSort(arr, left, mid);
        mergeSort(arr, mid + 1, right);
        
        // Merge the sorted halves
        merge(arr, left, mid, right);
    }
}

/**
 * Iterative implementation of merge sort
 * 
 * @param arr Array to be sorted
 */
void mergeSortIterative(vector<int>& arr) {
    int n = arr.size();
    
    // For current size of subarrays to be merged
    for (int currentSize = 1; currentSize <= n - 1; currentSize = 2 * currentSize) {
        // For picking starting index of left subarray
        for (int leftStart = 0; leftStart < n - 1; leftStart += 2 * currentSize) {
            // Find ending point of left subarray
            int mid = min(leftStart + currentSize - 1, n - 1);
            
            // Find ending point of right subarray
            int rightEnd = min(leftStart + 2 * currentSize - 1, n - 1);
            
            // Merge subarrays arr[leftStart..mid] and arr[mid+1..rightEnd]
            merge(arr, leftStart, mid, rightEnd);
        }
    }
}

/**
 * Utility function to print array
 * 
 * @param arr Array to print
 * @param size Size of array
 */
void printArray(const vector<int>& arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

/**
 * Function to test if array is sorted
 * 
 * @param arr Array to test
 * @return true if sorted, false otherwise
 */
bool isSorted(const vector<int>& arr) {
    for (int i = 1; i < arr.size(); i++) {
        if (arr[i] < arr[i - 1]) {
            return false;
        }
    }
    return true;
}

int main() {
    // Test cases
    vector<vector<int>> testCases = {
        {64, 34, 25, 12, 22, 11, 90},
        {5, 2, 8, 1, 9, 3, 7, 4, 6},
        {1},
        {3, 3, 3, 3},
        {9, 8, 7, 6, 5, 4, 3, 2, 1},
        {1, 2, 3, 4, 5, 6, 7, 8, 9},
        {-5, -2, 0, 3, 7, -1, 4}
    };
    
    cout << "Merge Sort Algorithm Test Cases" << endl;
    cout << "================================" << endl;
    
    for (int i = 0; i < testCases.size(); i++) {
        cout << "\nTest Case " << (i + 1) << ":" << endl;
        
        vector<int> arr1 = testCases[i];
        vector<int> arr2 = testCases[i];
        
        cout << "Original array: ";
        printArray(arr1);
        
        // Recursive merge sort
        auto start = chrono::high_resolution_clock::now();
        mergeSort(arr1, 0, arr1.size() - 1);
        auto end = chrono::high_resolution_clock::now();
        auto duration = chrono::duration_cast<chrono::microseconds>(end - start);
        
        cout << "Recursive Merge Sort: ";
        printArray(arr1);
        cout << "Time taken: " << duration.count() << " microseconds" << endl;
        cout << "Is sorted: " << (isSorted(arr1) ? "Yes" : "No") << endl;
        
        // Iterative merge sort
        start = chrono::high_resolution_clock::now();
        mergeSortIterative(arr2);
        end = chrono::high_resolution_clock::now();
        duration = chrono::duration_cast<chrono::microseconds>(end - start);
        
        cout << "Iterative Merge Sort: ";
        printArray(arr2);
        cout << "Time taken: " << duration.count() << " microseconds" << endl;
        cout << "Is sorted: " << (isSorted(arr2) ? "Yes" : "No") << endl;
        
        // Verify both implementations give same result
        bool same = (arr1 == arr2);
        cout << "Both implementations match: " << (same ? "Yes" : "No") << endl;
    }
    
    return 0;
}