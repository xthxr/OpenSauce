from typing import List

"""
Selection Sort Algorithm

Time Complexity: O(n^2) - in all cases (worst, average, and best)
Space Complexity: O(1) - in-place sorting

Selection sort works by repeatedly finding the minimum element from the unsorted 
part of the array and swapping it with the element at the beginning of the unsorted section.
It maintains a sorted subarray at the left end and an unsorted subarray at the right end.
"""

def selection_sort(arr: List[int]) -> List[int]:
    """
    Sorts a list of integers using the Selection Sort algorithm.
    
    Args:
        arr: The list of integers to be sorted (modified in-place).
    
    Returns:
        The sorted list.
    
    Example:
        >>> selection_sort([64, 25, 12, 22, 11])
        [11, 12, 22, 25, 64]
    """
    n = len(arr)
    
    # Traverse through all array elements
    for i in range(n):
        # Find the index of the minimum element in the remaining unsorted array
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j
                
        # Swap the found minimum element with the element at position i
        if min_idx != i:
            arr[i], arr[min_idx] = arr[min_idx], arr[i]
            
    return arr

if __name__ == "__main__":
    # Test Case 1: Standard array
    arr1 = [64, 25, 12, 22, 11]
    print(f"Original: {arr1}")
    selection_sort(arr1)
    print(f"Sorted:   {arr1}")
    
    # Test Case 2: Reverse sorted
    arr2 = [5, 4, 3, 2, 1]
    print(f"\nOriginal: {arr2}")
    selection_sort(arr2)
    print(f"Sorted:   {arr2}")
    
    # Test Case 3: Duplicates
    arr4 = [3, 1, 4, 1, 5, 9, 2]
    print(f"\nOriginal: {arr4}")
    selection_sort(arr4)
    print(f"Sorted:   {arr4}")