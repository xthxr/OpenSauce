package main

import (
    "fmt"
)

// partition rearranges the elements in the sub-array defined by [low...high]
// such that all elements less than or equal to the pivot are on the left,
// and all elements greater than the pivot are on the right.
// It returns the final index of the pivot element.
func partition(arr []int, low, high int) int {
    // Choose the rightmost element as the pivot
    pivot := arr[high]
    
    // Index of the smaller element, which indicates the position of the pivot later
    i := low - 1 

    for j := low; j < high; j++ {
        // If the current element is smaller than or equal to the pivot
        if arr[j] <= pivot {
            // Increment index of smaller element
            i++
            // Swap arr[i] and arr[j]
            arr[i], arr[j] = arr[j], arr[i]
        }
    }

    // Swap the pivot element (arr[high]) with the element at i+1
    // The element at i+1 is the first element greater than the pivot, 
    // so swapping the pivot here places it in its correct sorted position.
    arr[i+1], arr[high] = arr[high], arr[i+1]
    
    // Return the partitioning index
    return i + 1
}

// QuickSort is the main recursive function that implements the Quick Sort algorithm.
// Time Complexity (Average/Best): O(n log n)
// Time Complexity (Worst): O(n^2) - occurs when the pivot selection consistently results in very unequal partitions (e.g., already sorted array).
// Space Complexity: O(log n) - due to the recursive stack frames.
func QuickSort(arr []int, low, high int) {
    if low < high {
        // pi is the partitioning index, arr[pi] is now at the right place
        pi := partition(arr, low, high)

        // Recursively sort the elements before partition and after partition
        QuickSort(arr, low, pi-1)
        QuickSort(arr, pi+1, high)
    }
}

func main() {
    // Example array to sort
    data := []int{10, 80, 30, 90, 40, 50, 70}
    n := len(data)

    fmt.Println("Original array:", data)

    // Call QuickSort on the entire array (from index 0 to n-1)
    QuickSort(data, 0, n-1)

    fmt.Println("Sorted array:", data)

    // Another example
    data2 := []int{5, 2, 8, 1, 9, 4, 7, 3, 6}
    n2 := len(data2)

    fmt.Println("\nOriginal array 2:", data2)
    QuickSort(data2, 0, n2-1)
    fmt.Println("Sorted array 2:", data2)
}
