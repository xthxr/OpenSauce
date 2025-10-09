/*
 * ## Problem: Merge Sort ##
 *
 * Merge Sort is a highly efficient, stable, comparison-based sorting algorithm.
 * It's a classic example of the "divide and conquer" paradigm.
 *
 * How it works:
 * 1.  **Divide**: The unsorted array is divided into two halves.
 * 2.  **Conquer**: Recursively sort each half.
 * 3.  **Combine**: Merge the two sorted halves back into a single sorted array.
 *
 * The core of the algorithm is the `merge` function, which merges two sorted subarrays
 * into a single sorted array.
 */
func mergeSort(_ array: [Int]) -> [Int] {
    // A list with zero or one element is already sorted. This is our base case.
    guard array.count > 1 else {
        return array
    }

    // Find the middle of the array to split it into two halves.
    let middleIndex = array.count / 2
    
    // Recursively sort the left half.
    let leftArray = mergeSort(Array(array[0..<middleIndex]))
    
    // Recursively sort the right half.
    let rightArray = mergeSort(Array(array[middleIndex..<array.count]))
    
    // Merge the two sorted halves back together.
    return merge(leftPile: leftArray, rightPile: rightArray)
}

func merge(leftPile: [Int], rightPile: [Int]) -> [Int] {
    // Initialize pointers for the left and right piles.
    var leftIndex = 0
    var rightIndex = 0

    // This will be our final merged and sorted array.
    var orderedPile: [Int] = []
    orderedPile.reserveCapacity(leftPile.count + rightPile.count)

    // Compare elements from both piles and append the smaller one to the ordered pile.
    while leftIndex < leftPile.count && rightIndex < rightPile.count {
        if leftPile[leftIndex] < rightPile[rightIndex] {
            orderedPile.append(leftPile[leftIndex])
            leftIndex += 1
        } else {
            orderedPile.append(rightPile[rightIndex])
            rightIndex += 1
        }
    }

    // If there are any remaining elements in the left pile, append them.
    while leftIndex < leftPile.count {
        orderedPile.append(leftPile[leftIndex])
        leftIndex += 1
    }

    // If there are any remaining elements in the right pile, append them.
    while rightIndex < rightPile.count {
        orderedPile.append(rightPile[rightIndex])
        rightIndex += 1
    }

    return orderedPile
}

// --- Test Cases ---
let unsortedArray = [38, 27, 43, 3, 9, 82, 10]
print("Unsorted Array: \(unsortedArray)")

let sortedArray = mergeSort(unsortedArray)
print("Sorted Array  : \(sortedArray)")

let anotherArray = [5, 1, 4, 8, 2, 9, 3, 7, 6]
print("\nUnsorted Array: \(anotherArray)")
let sortedAnotherArray = mergeSort(anotherArray)
print("Sorted Array  : \(sortedAnotherArray)")
