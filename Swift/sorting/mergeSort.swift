/*
 * 1. Brief description of the algorithm
 * Merge Sort is a highly efficient, stable, comparison-based sorting algorithm.
 * It's a classic example of the "divide and conquer" paradigm that works by
 * recursively splitting the array into halves, sorting them, and then merging them back.
 *
 * 2. Time complexity: O(n log n)
 * This is consistent for the best, average, and worst cases because the array is
 * always divided in half.
 *
 * 3. Space complexity: O(n)
 * This is because it requires additional memory to store the merged subarrays.
 */

// 4. Implementation
func mergeSort(_ array: [Int]) -> [Int] {
    // Base case: An array with zero or one element is already sorted.
    guard array.count > 1 else {
        return array
    }

    // Divide the array into two halves.
    let middleIndex = array.count / 2
    
    // Recursively sort the left and right halves.
    let leftArray = mergeSort(Array(array[0..<middleIndex]))
    let rightArray = mergeSort(Array(array[middleIndex..<array.count]))
    
    // Conquer by merging the two sorted halves.
    return merge(leftPile: leftArray, rightPile: rightArray)
}

func merge(leftPile: [Int], rightPile: [Int]) -> [Int] {
    var leftIndex = 0
    var rightIndex = 0
    var orderedPile: [Int] = []
    orderedPile.reserveCapacity(leftPile.count + rightPile.count)

    // Compare elements from both piles and append the smaller one.
    while leftIndex < leftPile.count && rightIndex < rightPile.count {
        if leftPile[leftIndex] < rightPile[rightIndex] {
            orderedPile.append(leftPile[leftIndex])
            leftIndex += 1
        } else {
            orderedPile.append(rightPile[rightIndex])
            rightIndex += 1
        }
    }

    // Append any remaining elements.
    orderedPile.append(contentsOf: leftPile[leftIndex...])
    orderedPile.append(contentsOf: rightPile[rightIndex...])

    return orderedPile
}

// 5. Example usage/test cases
let unsortedArray = [38, 27, 43, 3, 9, 82, 10]
print("Unsorted Array: \(unsortedArray)")
let sortedArray = mergeSort(unsortedArray)
print("Sorted Array  : \(sortedArray)") // Expected: [3, 9, 10, 27, 38, 43, 82]

let anotherArray = [5, 1, 4, 8, 2, 9, 3, 7, 6]
print("\nUnsorted Array: \(anotherArray)")
let sortedAnotherArray = mergeSort(anotherArray)
print("Sorted Array  : \(sortedAnotherArray)") // Expected: [1, 2, 3, 4, 5, 6, 7, 8, 9]

