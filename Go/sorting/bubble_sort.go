package main

import "fmt"

// SortOrder defines the type for sorting direction
type SortOrder int

// Constants for sorting order options
const (
	Ascending SortOrder = iota
	Descending
)

func main() {
	arr := []int{9, 1, 8, 2, 7, 3, 10, 6, 4, 5}

	fmt.Println("Original array:", arr)

	// Create a copy for ascending sort to preserve original array
	arrAsc := make([]int, len(arr))
	copy(arrAsc, arr)
	bubbleSort(arrAsc, Ascending)
	fmt.Println("Ascending sort:", arrAsc)

	// Create another copy for descending sort
	arrDesc := make([]int, len(arr))
	copy(arrDesc, arr)
	bubbleSort(arrDesc, Descending)
	fmt.Println("Descending sort:", arrDesc)
}

// bubbleSort implements the sort algorithm
// It sorts the array in-place based on the specified order
// Time Complexity: O(n²) in worst case, O(n) in best case (already sorted)
// Space Complexity: O(1) - only uses constant extra space
func bubbleSort(arr []int, order SortOrder) {
	n := len(arr)

	// Outer loop: iterate through all elements
	for i := 0; i < n-1; i++ {
		swapped := false // Flag to track if any swaps occurred in this iteration

		// Inner loop: compare adjacent elements
        	// Reduce range by i since last i elements are already in correct position
		for j := 0; j < n-1-i; j++ {
			shouldSwap := false

			// Determine if elements should be swapped based on sort order
			if order == Ascending {
				// For ascending order: swap if left element is greater than right
				if arr[j] > arr[j+1] {
					shouldSwap = true
				}
			} else {
				// For descending order: swap if left element is smaller than right
				if arr[j] < arr[j+1] {
					shouldSwap = true
				}
			}

			if shouldSwap {
				arr[j], arr[j+1] = arr[j+1], arr[j] // Go's tuple assignment for swapping
				swapped = true
			}
		}

		// Optimization: if no swaps occurred, array is already sorted
        	// This makes best-case time complexity O(n) for already sorted arrays
		if !swapped {
			break
		}
	}
}
