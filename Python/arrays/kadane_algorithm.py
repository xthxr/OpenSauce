"""
Kadane's Algorithm - Maximum Subarray Sum

Time Complexity: O(n)
Space Complexity: O(1)

Kadane's algorithm finds the maximum sum of contiguous subarray in linear time.
It's an efficient solution to the maximum subarray problem using dynamic programming approach.
"""

def kadane_algorithm(arr):
    """
    Find the maximum sum of contiguous subarray using Kadane's algorithm.
    
    Args:
        arr: List of integers (can contain negative numbers)
    
    Returns:
        int: Maximum sum of contiguous subarray
    
    Example:
        >>> kadane_algorithm([-2, 1, -3, 4, -1, 2, 1, -5, 4])
        6
        >>> kadane_algorithm([1, 2, 3, 4, 5])
        15
        >>> kadane_algorithm([-1, -2, -3, -4])
        -1
    """
    if not arr:
        return 0
    
    max_ending_here = max_so_far = arr[0]
    
    for i in range(1, len(arr)):
        # Either extend the existing subarray or start a new one
        max_ending_here = max(arr[i], max_ending_here + arr[i])
        # Update the maximum sum found so far
        max_so_far = max(max_so_far, max_ending_here)
    
    return max_so_far

def kadane_with_indices(arr):
    """
    Find the maximum sum of contiguous subarray and return the indices.
    
    Args:
        arr: List of integers
    
    Returns:
        tuple: (max_sum, start_index, end_index)
    
    Example:
        >>> kadane_with_indices([-2, 1, -3, 4, -1, 2, 1, -5, 4])
        (6, 3, 6)
    """
    if not arr:
        return 0, 0, 0
    
    max_ending_here = max_so_far = arr[0]
    start = end = 0
    temp_start = 0
    
    for i in range(1, len(arr)):
        if max_ending_here < 0:
            max_ending_here = arr[i]
            temp_start = i
        else:
            max_ending_here += arr[i]
        
        if max_ending_here > max_so_far:
            max_so_far = max_ending_here
            start = temp_start
            end = i
    
    return max_so_far, start, end

def kadane_2d(matrix):
    """
    Find maximum sum rectangle in 2D matrix using Kadane's algorithm.
    
    Args:
        matrix: 2D list of integers
    
    Returns:
        int: Maximum sum of rectangle
    
    Example:
        >>> matrix = [[1, 2, -1, -4, -20],
        ...           [-8, -3, 4, 2, 1],
        ...           [3, 8, 10, 1, 3],
        ...           [-4, -1, 1, 7, -6]]
        >>> kadane_2d(matrix)
        29
    """
    if not matrix or not matrix[0]:
        return 0
    
    rows, cols = len(matrix), len(matrix[0])
    max_sum = float('-inf')
    
    # Try all possible left boundaries
    for left in range(cols):
        temp = [0] * rows
        
        # Try all possible right boundaries
        for right in range(left, cols):
            # Calculate sum for each row between left and right
            for i in range(rows):
                temp[i] += matrix[i][right]
            
            # Apply Kadane's algorithm on the temp array
            current_sum = kadane_algorithm(temp)
            max_sum = max(max_sum, current_sum)
    
    return max_sum

if __name__ == "__main__":
    # Test cases
    test_cases = [
        [-2, 1, -3, 4, -1, 2, 1, -5, 4],
        [1, 2, 3, 4, 5],
        [-1, -2, -3, -4],
        [5, -4, -2, 6, -1],
        [1],
        []
    ]
    
    print("Kadane's Algorithm Test Cases")
    print("=" * 40)
    
    for i, arr in enumerate(test_cases):
        if arr:  # Skip empty array
            max_sum = kadane_algorithm(arr)
            max_sum_with_indices, start, end = kadane_with_indices(arr)
            
            print(f"\nTest Case {i + 1}:")
            print(f"Array: {arr}")
            print(f"Maximum subarray sum: {max_sum}")
            print(f"Subarray: {arr[start:end+1]} (indices {start} to {end})")
            print(f"Verification: {sum(arr[start:end+1])} = {max_sum}")
    
    # Test 2D Kadane's algorithm
    print("\n" + "=" * 40)
    print("2D Kadane's Algorithm Test")
    matrix = [
        [1, 2, -1, -4, -20],
        [-8, -3, 4, 2, 1],
        [3, 8, 10, 1, 3],
        [-4, -1, 1, 7, -6]
    ]
    print(f"Matrix: {matrix}")
    print(f"Maximum rectangle sum: {kadane_2d(matrix)}")
