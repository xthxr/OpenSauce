"""
Bubble Sort Algorithm

Time Complexity: O(n^2) in worst and average case, O(n) in best case (when array is already sorted)
Space Complexity: O(1)

Bubble sort is a simple sorting algorithm that repeatedly steps through the list,
compares adjacent elements and swaps them if they are in the wrong order.
"""

def bubble_sort(arr):
    """
    Sorts an array using bubble sort algorithm.
    
    Args:
        arr: List of comparable elements to be sorted
    
    Returns:
        Sorted list (modifies the input list in-place and returns it)
    
    Example:
        >>> bubble_sort([64, 34, 25, 12, 22, 11, 90])
        [11, 12, 22, 25, 34, 64, 90]
    """
    n = len(arr)
    
    # Traverse through all array elements
    for i in range(n):
        # Last i elements are already in place
        swapped = False
        
        for j in range(0, n - i - 1):
            # Swap if the element found is greater than the next element
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                swapped = True
        
        # If no swapping happened, array is already sorted
        if not swapped:
            break
    
    return arr


def bubble_sort_descending(arr):
    """
    Sorts an array in descending order using bubble sort.
    
    Args:
        arr: List of comparable elements to be sorted
    
    Returns:
        Sorted list in descending order
    
    Example:
        >>> bubble_sort_descending([64, 34, 25, 12, 22, 11, 90])
        [90, 64, 34, 25, 22, 12, 11]
    """
    n = len(arr)
    
    for i in range(n):
        swapped = False
        
        for j in range(0, n - i - 1):
            # Swap if the element found is smaller than the next element
            if arr[j] < arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                swapped = True
        
        if not swapped:
            break
    
    return arr


if __name__ == "__main__":
    # Example usage
    print("Bubble Sort Examples:")
    
    # Test ascending sort
    arr1 = [64, 34, 25, 12, 22, 11, 90]
    print(f"\nOriginal array: {arr1}")
    sorted_arr1 = bubble_sort(arr1.copy())
    print(f"Sorted array (ascending): {sorted_arr1}")
    
    # Test descending sort
    arr2 = [64, 34, 25, 12, 22, 11, 90]
    sorted_arr2 = bubble_sort_descending(arr2.copy())
    print(f"Sorted array (descending): {sorted_arr2}")
    
    # Test with already sorted array
    arr3 = [1, 2, 3, 4, 5]
    print(f"\nAlready sorted array: {arr3}")
    sorted_arr3 = bubble_sort(arr3.copy())
    print(f"After bubble sort: {sorted_arr3}")
    
    # Test with single element
    arr4 = [42]
    print(f"\nSingle element array: {arr4}")
    sorted_arr4 = bubble_sort(arr4.copy())
    print(f"After bubble sort: {sorted_arr4}")
