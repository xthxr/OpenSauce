"""
Binary Search Algorithm

Time Complexity: O(log n)
Space Complexity: O(1)

Binary search is an efficient algorithm for finding a target value in a sorted array.
It works by repeatedly dividing the search interval in half.
"""

def binary_search(arr, target):
    """
    Performs binary search on a sorted array.
    
    Args:
        arr: Sorted list of comparable elements
        target: Element to search for
    
    Returns:
        Index of target if found, -1 otherwise
    
    Example:
        >>> binary_search([1, 3, 5, 7, 9, 11], 7)
        3
        >>> binary_search([1, 3, 5, 7, 9, 11], 4)
        -1
    """
    left, right = 0, len(arr) - 1
    
    while left <= right:
        mid = (left + right) // 2
        
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1


def binary_search_recursive(arr, target, left=0, right=None):
    """
    Recursive implementation of binary search.
    
    Args:
        arr: Sorted list of comparable elements
        target: Element to search for
        left: Left boundary of search (default: 0)
        right: Right boundary of search (default: len(arr) - 1)
    
    Returns:
        Index of target if found, -1 otherwise
    
    Example:
        >>> binary_search_recursive([1, 3, 5, 7, 9, 11], 9)
        4
    """
    if right is None:
        right = len(arr) - 1
    
    if left > right:
        return -1
    
    mid = (left + right) // 2
    
    if arr[mid] == target:
        return mid
    elif arr[mid] < target:
        return binary_search_recursive(arr, target, mid + 1, right)
    else:
        return binary_search_recursive(arr, target, left, mid - 1)


if __name__ == "__main__":
    # Example usage
    test_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    
    # Test iterative version
    print("Iterative Binary Search:")
    target = 7
    result = binary_search(test_array, target)
    if result != -1:
        print(f"Element {target} found at index {result}")
    else:
        print(f"Element {target} not found in array")
    
    # Test recursive version
    print("\nRecursive Binary Search:")
    target = 13
    result = binary_search_recursive(test_array, target)
    if result != -1:
        print(f"Element {target} found at index {result}")
    else:
        print(f"Element {target} not found in array")
    
    # Test with element not in array
    target = 8
    result = binary_search(test_array, target)
    if result != -1:
        print(f"\nElement {target} found at index {result}")
    else:
        print(f"\nElement {target} not found in array")
