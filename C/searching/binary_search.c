/*
 * File: binary_search.c
 * Description: Iterative implementation of Binary Search algorithm
 * Author: Gompa Rani Prasanna
 *
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 */

#include <stdio.h>

// Function to perform binary search
int binarySearch(int arr[], int n, int target) {
    int low = 0, high = n - 1;
    
    while (low <= high) {
        int mid = low + (high - low) / 2; // Prevent overflow
        
        if (arr[mid] == target)
            return mid; // Target found
        else if (arr[mid] < target)
            low = mid + 1; // Search right half
        else
            high = mid - 1; // Search left half
    }
    
    return -1; // Not found
}

// Example usage
int main() {
    int arr[] = {1, 3, 5, 7, 9};
    int n = sizeof(arr) / sizeof(arr[0]);
    int target = 5;
    
    int result = binarySearch(arr, n, target);
    if (result != -1)
        printf("Element found at index %d\n", result);
    else
        printf("Element not found\n");
    
    return 0;
}
