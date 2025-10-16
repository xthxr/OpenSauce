"""
Selection Sort Algorithm
"""


def selection_sort(arr: list) -> list:
    """
    Selection Sort Algorithm
    
    Divides the array into sorted and unsorted portions. Repeatedly finds
    the minimum element from the unsorted portion and places it at the end
    of the sorted portion.
    
    Time Complexity: O(n²) - always, regardless of input
    Space Complexity: O(1) - sorts in place, only constant extra space
    
    Args:
        arr: List of comparable elements to be sorted
    
    Returns:
        Sorted list in ascending order
    
    Example:
        >>> selection_sort([64, 34, 25, 12, 22, 11, 90])
        [11, 12, 22, 25, 34, 64, 90]
    """
    n = len(arr)
    
    # Traverse through all array elements
    for i in range(n):
        # Find the minimum element in remaining unsorted array
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j
        
        # Swap the found minimum element with the first element
        arr[i], arr[min_idx] = arr[min_idx], arr[i]
    
    return arr


if __name__ == "__main__":
    # Test Case 1
    arr1 = [64, 34, 25, 12, 22, 11, 90]
    print(f"Original array: {arr1}")
    result1 = selection_sort(arr1.copy())
    print(f"Sorted array: {result1}")
    
    # Test Case 2
    arr2 = [5, 2, 8, 1, 9]
    print(f"\nOriginal array: {arr2}")
    result2 = selection_sort(arr2.copy())
    print(f"Sorted array: {result2}")
    
    # Test Case 3
    arr3 = [1]
    print(f"\nOriginal array: {arr3}")
    result3 = selection_sort(arr3.copy())
    print(f"Sorted array: {result3}")
