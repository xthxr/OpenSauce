/*
 * ## Problem: Binary Search ##
 *
 * Binary search is an efficient algorithm for finding an item from a **sorted** list of items.
 * It works by repeatedly dividing the search interval in half.
 *
 * How it works:
 * 1.  Compare the target value to the middle element of the array.
 * 2.  If they are not equal, the half in which the target cannot lie is eliminated,
 * and the search continues on the remaining half.
 * 3.  This process is repeated until the target value is found or the remaining half is empty.
 *
 * Prerequisite: The input array must be sorted.
 */

// An iterative implementation of Binary Search.
func binarySearchIterative(array: [Int], target: Int) -> Int? {
    // Set the initial search range.
    var low = 0
    var high = array.count - 1

    // Continue searching as long as the range is valid.
    while low <= high {
        // Find the middle index.
        // Using (low + (high - low) / 2) avoids potential overflow.
        let mid = low + (high - low) / 2

        // Check if the middle element is the target.
        if array[mid] == target {
            return mid // Target found, return its index.
        } else if array[mid] < target {
            // If the middle element is less than the target,
            // narrow the search to the right half.
            low = mid + 1
        } else {
            // If the middle element is greater than the target,
            // narrow the search to the left half.
            high = mid - 1
        }
    }
    
    // If the loop finishes, the target was not in the array.
    return nil
}

// A recursive implementation of Binary Search.
func binarySearchRecursive(array: [Int], target: Int, range: Range<Int>) -> Int? {
    // Base case: If the search range is empty, the element is not present.
    if range.lowerBound >= range.upperBound {
        return nil
    }

    // Find the middle index of the current range.
    let midIndex = range.lowerBound + (range.upperBound - range.lowerBound) / 2

    // Check if the middle element is the target.
    if array[midIndex] == target {
        return midIndex
    } else if array[midIndex] < target {
        // Search in the right half of the current range.
        return binarySearchRecursive(array: array, target: target, range: (midIndex + 1)..<range.upperBound)
    } else {
        // Search in the left half of the current range.
        return binarySearchRecursive(array: array, target: target, range: range.lowerBound..<midIndex)
    }
}


// --- Test Cases ---
let sortedArray = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
let target1 = 23
let target2 = 99

print("Array: \(sortedArray)")

print("\n--- Iterative ---")
if let index = binarySearchIterative(array: sortedArray, target: target1) {
    print("Element \(target1) found at index \(index).")
} else {
    print("Element \(target1) not found in the array.")
}

if let index = binarySearchIterative(array: sortedArray, target: target2) {
    print("Element \(target2) found at index \(index).")
} else {
    print("Element \(target2) not found in the array.")
}


print("\n--- Recursive ---")
if let index = binarySearchRecursive(array: sortedArray, target: target1, range: 0..<sortedArray.count) {
    print("Element \(target1) found at index \(index).")
} else {
    print("Element \(target1) not found in the array.")
}

if let index = binarySearchRecursive(array: sortedArray, target: target2, range: 0..<sortedArray.count) {
    print("Element \(target2) found at index \(index).")
} else {
    print("Element \(target2) not found in the array.")
}
