def mergesort(arr):
    """
    MergeSort algorithm implementation
    Time Complexity: O(n log n) in all cases
    Space Complexity: O(n) for temporary arrays
    """
    # Base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr
    
    # Divide: find the middle point
    mid = len(arr) // 2
    
    # Recursively sort left and right halves
    left_half = mergesort(arr[:mid])
    right_half = mergesort(arr[mid:])
    
    # Conquer: merge the sorted halves
    return merge(left_half, right_half)


def merge(left, right):
    """
    Merge two sorted arrays into one sorted array
    """
    result = []
    i = j = 0
    
    # Compare elements from left and right, add smaller to result
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    
    # Add remaining elements (one of these will be empty)
    result.extend(left[i:])
    result.extend(right[j:])
    
    return result


def mergesort_inplace(arr, left=0, right=None):
    """
    In-place MergeSort implementation (optimized for space)
    Modifies the original array
    """
    if right is None:
        right = len(arr) - 1
    
    if left < right:
        # Find the middle point
        mid = (left + right) // 2
        
        # Recursively sort first and second halves
        mergesort_inplace(arr, left, mid)
        mergesort_inplace(arr, mid + 1, right)
        
        # Merge the sorted halves
        merge_inplace(arr, left, mid, right)
    
    return arr


def merge_inplace(arr, left, mid, right):
    """
    Merge helper function for in-place mergesort
    Merges two sorted subarrays arr[left..mid] and arr[mid+1..right]
    """
    # Create temporary arrays
    left_arr = arr[left:mid + 1]
    right_arr = arr[mid + 1:right + 1]
    
    i = j = 0
    k = left
    
    # Merge temporary arrays back into arr[left..right]
    while i < len(left_arr) and j < len(right_arr):
        if left_arr[i] <= right_arr[j]:
            arr[k] = left_arr[i]
            i += 1
        else:
            arr[k] = right_arr[j]
            j += 1
        k += 1
    
    # Copy remaining elements
    while i < len(left_arr):
        arr[k] = left_arr[i]
        i += 1
        k += 1
    
    while j < len(right_arr):
        arr[k] = right_arr[j]
        j += 1
        k += 1


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
    print("MERGESORT - Simple Version")
    print("=" * 50)
    
    for arr in test_arrays:
        original = arr.copy()
        sorted_arr = mergesort(arr)
        print(f"Original: {original}")
        print(f"Sorted:   {sorted_arr}\n")
    
    print("=" * 50)
    print("MERGESORT - In-Place Version")
    print("=" * 50)
    
    for arr in test_arrays:
        original = arr.copy()
        mergesort_inplace(arr)
        print(f"Original: {original}")
        print(f"Sorted:   {arr}\n")
    
    # Performance test with larger array
    import random
    import time
    
    large_array = [random.randint(1, 1000) for _ in range(1000)]
    
    start = time.time()
    sorted_array = mergesort(large_array.copy())
    end = time.time()
    print(f"Simple MergeSort - 1000 elements: {(end - start) * 1000:.2f}ms")
    
    start = time.time()
    mergesort_inplace(large_array.copy())
    end = time.time()
    print(f"In-place MergeSort - 1000 elements: {(end - start) * 1000:.2f}ms")
    
    # Interactive input (uncomment to use)
    # print("\n" + "=" * 50)
    # print("INTERACTIVE MODE")
    # print("=" * 50)
    # n = int(input("Enter number of elements: "))
    # arr = list(map(int, input("Enter elements separated by space: ").split()))
    # sorted_arr = mergesort(arr)
    # print("Sorted array:", *sorted_arr)