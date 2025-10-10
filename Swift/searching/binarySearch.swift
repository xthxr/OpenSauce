/*
 * 1. Brief description of the algorithm
 * Binary search is an efficient algorithm for finding an item from a **sorted**
 * list of items. It works by repeatedly dividing the search interval in half.
 *
 * 2. Time complexity: O(log n)
 * With each comparison, the search space is halved.
 *
 * 3. Space complexity:
 * - Iterative: O(1) - Constant space is used.
 * - Recursive: O(log n) - Due to the call stack depth for recursion.
 */

// 4. Implementation

/**
 * Iterative Binary Search
 */
func binarySearchIterative(array: [Int], target: Int) -> Int? {
    var low = 0
    var high = array.count - 1

    while low <= high {
        let mid = low + (high - low) / 2
        if array[mid] == target {
            return mid
        } else if array[mid] < target {
            low = mid + 1
        } else {
            high = mid - 1
        }
    }
    return nil
}

/**
 * Recursive Binary Search
 */
func binarySearchRecursive(array: [Int], target: Int, range: Range<Int>) -> Int? {
    if range.lowerBound >= range.upperBound {
        return nil
    }

    let midIndex = range.lowerBound + (range.upperBound - range.lowerBound) / 2
    if array[midIndex] == target {
        return midIndex
    } else if array[midIndex] < target {
        return binarySearchRecursive(array: array, target: target, range: (midIndex + 1)..<range.upperBound)
    } else {
        return binarySearchRecursive(array: array, target: target, range: range.lowerBound..<midIndex)
    }
}


// 5. Example usage/test cases
let sortedArray = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
let target1 = 23
let target2 = 99

print("Array: \(sortedArray)")

print("\n--- Iterative ---")
if let index = binarySearchIterative(array: sortedArray, target: target1) {
    print("Element \(target1) found at index \(index).") // Expected: 5
} else {
    print("Element \(target1) not found in the array.")
}

if let index = binarySearchIterative(array: sortedArray, target: target2) {
    print("Element \(target2) found at index \(index).")
} else {
    print("Element \(target2) not found in the array.") // Expected: not found
}

print("\n--- Recursive ---")
if let index = binarySearchRecursive(array: sortedArray, target: target1, range: 0..<sortedArray.count) {
    print("Element \(target1) found at index \(index).") // Expected: 5
} else {
    print("Element \(target1) not found in the array.")
}

if let index = binarySearchRecursive(array: sortedArray, target: target2, range: 0..<sortedArray.count) {
    print("Element \(target2) found at index \(index).")
} else {
    print("Element \(target2) not found in the array.") // Expected: not found
}

