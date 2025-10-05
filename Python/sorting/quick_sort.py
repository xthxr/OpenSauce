def quicksort(arr):
    """
    QuickSort algorithm implementation
    Time Complexity: O(n log n) average, O(n²) worst case
    Space Complexity: O(log n) due to recursion
    """
    # Base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr
    
    # Choose pivot (middle element)
    pivot = arr[len(arr) // 2]
    
    # Partition array into three parts
    left = [x for x in arr if x < pivot]      # Elements less than pivot
    middle = [x for x in arr if x == pivot]   # Elements equal to pivot
    right = [x for x in arr if x > pivot]     # Elements greater than pivot
    
    # Recursively sort left and right, combine with middle
    return quicksort(left) + middle + quicksort(right)


def quicksort_inplace(arr, low=0, high=None):
    """
    In-place QuickSort implementation (more memory efficient)
    Modifies the original array
    """
    if high is None:
        high = len(arr) - 1
    
    if low < high:
        # Partition and get pivot index
        pivot_index = partition(arr, low, high)
        
        # Recursively sort left and right partitions
        quicksort_inplace(arr, low, pivot_index - 1)
        quicksort_inplace(arr, pivot_index + 1, high)
    
    return arr


def partition(arr, low, high):
    """
    Partition helper function for in-place quicksort
    Uses last element as pivot
    """
    pivot = arr[high]
    i = low - 1
    
    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]  # Swap
    
    arr[i + 1], arr[high] = arr[high], arr[i + 1]  # Place pivot in correct position
    return i + 1


# Test the implementations
if __name__ == "__main__":
    # Test data
    test_arrays = [
        [64, 34, 25, 12, 22, 11, 90],
        [5, 2, 8, 1, 9],
        [1],
        [],
        [3, 3, 3, 3],
        [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    ]
    
    print("=" * 50)
    print("QUICKSORT - Simple Version")
    print("=" * 50)
    
    for arr in test_arrays:
        original = arr.copy()
        sorted_arr = quicksort(arr)
        print(f"Original: {original}")
        print(f"Sorted:   {sorted_arr}\n")
    
    print("=" * 50)
    print("QUICKSORT - In-Place Version")
    print("=" * 50)
    
    for arr in test_arrays:
        original = arr.copy()
        quicksort_inplace(arr)
        print(f"Original: {original}")
        print(f"Sorted:   {arr}\n")
    
    # Performance test with larger array
    import random
    import time
    
    large_array = [random.randint(1, 1000) for _ in range(1000)]
    
    start = time.time()
    sorted_array = quicksort(large_array.copy())
    end = time.time()
    print(f"Simple QuickSort - 1000 elements: {(end - start) * 1000:.2f}ms")
    
    start = time.time()
    quicksort_inplace(large_array.copy())
    end = time.time()
    print(f"In-place QuickSort - 1000 elements: {(end - start) * 1000:.2f}ms")