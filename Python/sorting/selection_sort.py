# Selection Sort Algorithm (In-Place)
# --------------------------------------------------------------------------------
# Description:
# Selection sort is a simple, in-place comparison sort algorithm. It works by 
# finding the minimum element from the unsorted part of the array and swapping 
# it with the element at the beginning of the unsorted section.

# Time Complexity:
# Worst Case: O(n²) 
# Average Case: O(n²)
# Best Case: O(n²) (The number of comparisons remains the same regardless of input.)

# Space Complexity:
# Auxiliary Space: O(1) (The algorithm sorts in-place.)
# --------------------------------------------------------------------------------

def selection_sort(arr):
    """
    Sorts a list of integers using the Selection Sort algorithm in-place.
    Modifies the original list directly.
    
    Args:
        arr (list): The list of elements to be sorted.
        
    Returns:
        list: The sorted list (the same list object passed in).
    """
    n = len(arr)
    
    # Outer loop iterates through all elements to be placed in the sorted portion
    for i in range(n):
        # Assume the current position 'i' holds the index of the minimum element
        min_idx = i
        
        # Inner loop finds the actual minimum element index in the remaining unsorted array
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j
                
        # Swap the found minimum element (at min_idx) with the element at the current position 'i'
        # Only swap if the minimum element is not already at the correct position
        if min_idx != i:
            arr[i], arr[min_idx] = arr[min_idx], arr[i]

    return arr


# Example Usage / Test Cases
if __name__ == "__main__":
    
    # Test Case 1: Standard case
    list1 = [64, 25, 12, 22, 11]
    print(f"Original List 1: {list1}")
    selection_sort(list1)
    print(f"Sorted List 1:   {list1}") 
    # Expected Output: [11, 12, 22, 25, 64]
    
    print("-" * 25)

    # Test Case 2: Reverse sorted list (Worst Case)
    list2 = [5, 4, 3, 2, 1]
    print(f"Original List 2: {list2}")
    selection_sort(list2)
    print(f"Sorted List 2:   {list2}")
    # Expected Output: [1, 2, 3, 4, 5]
    
    print("-" * 25)
    
    # Test Case 3: List with duplicates
    list3 = [3, 1, 4, 1, 5, 9, 2, 6]
    print(f"Original List 3: {list3}")
    selection_sort(list3)
    print(f"Sorted List 3:   {list3}")
    # Expected Output: [1, 1, 2, 3, 4, 5, 6, 9]