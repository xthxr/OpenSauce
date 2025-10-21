package main

import (
    "fmt"
)

// partition rearranges elements in arr[low...high] such that all elements
// less than or equal to the pivot are on the left, and all greater are on the right.
// It returns the final index of the pivot element.
func partition(arr []int, low, high int) int {
    pivot := arr[high] // Choose the rightmost element as pivot
    i := low - 1       // Index of smaller element

    for j := low; j < high; j++ {
        if arr[j] <= pivot {
            i++
            arr[i], arr[j] = arr[j], arr[i]
        }
    }

    arr[i+1], arr[high] = arr[high], arr[i+1]
    return i + 1
}

// QuickSort recursively sorts the array using Quick Sort algorithm.
func QuickSort(arr []int, low, high int) {
    if low < high {
        pi := partition(arr, low, high)

        // Sort elements before and after partition
        QuickSort(arr, low, pi-1)
        QuickSort(arr, pi+1, high)
    }
}

func main() {
    // Example array 1
    data := []int{10, 80, 30, 90, 40, 50, 70}
    fmt.Println("Original array:", data)
    QuickSort(data, 0, len(data)-1)
    fmt.Println("Sorted array:", data)

    // Example array 2
    data2 := []int{5, 2, 8, 1, 9, 4, 7, 3, 6}
    fmt.Println("\nOriginal array 2:", data2)
    QuickSort(data2, 0, len(data2)-1)
    fmt.Println("Sorted array 2:", data2)
}
