"""
Sliding Window Maximum Problem
Given an array and a window size k, find the maximum element in each sliding window.

Example: arr = [1,3,-1,-3,5,3,6,7], k = 3
Output: [3,3,5,5,6,7]

Time Complexity: O(n) using deque
Space Complexity: O(k)
"""

from collections import deque


def sliding_window_maximum(nums, k):
    """
    Find maximum in each sliding window of size k
    Uses monotonic deque approach
    
    Args:
        nums: List of integers
        k: Window size
    
    Returns:
        List of maximum values for each window
    
    Time: O(n), each element added and removed at most once
    Space: O(k) for the deque
    """
    if not nums or k <= 0:
        return []
    
    if k == 1:
        return nums
    
    result = []
    dq = deque()  # Store indices
    
    for i in range(len(nums)):
        # Remove elements outside current window
        if dq and dq[0] < i - k + 1:
            dq.popleft()
        
        # Remove smaller elements from rear (maintain decreasing order)
        while dq and nums[dq[-1]] < nums[i]:
            dq.pop()
        
        # Add current element index
        dq.append(i)
        
        # Add to result if window is complete
        if i >= k - 1:
            result.append(nums[dq[0]])  # Front has the maximum
    
    return result


def sliding_window_maximum_naive(nums, k):
    """
    Naive approach - Check each window separately
    
    Time: O(n*k)
    Space: O(1)
    """
    if not nums or k <= 0:
        return []
    
    result = []
    for i in range(len(nums) - k + 1):
        window_max = max(nums[i:i+k])
        result.append(window_max)
    
    return result


def sliding_window_minimum(nums, k):
    """
    Find minimum in each sliding window of size k
    Similar to maximum but maintains increasing order in deque
    
    Time: O(n)
    Space: O(k)
    """
    if not nums or k <= 0:
        return []
    
    if k == 1:
        return nums
    
    result = []
    dq = deque()
    
    for i in range(len(nums)):
        # Remove elements outside current window
        if dq and dq[0] < i - k + 1:
            dq.popleft()
        
        # Remove larger elements from rear (maintain increasing order)
        while dq and nums[dq[-1]] > nums[i]:
            dq.pop()
        
        dq.append(i)
        
        if i >= k - 1:
            result.append(nums[dq[0]])
    
    return result


def sliding_window_range(nums, k):
    """
    Find range (max - min) in each sliding window
    
    Time: O(n)
    Space: O(k)
    """
    if not nums or k <= 0:
        return []
    
    max_vals = sliding_window_maximum(nums, k)
    min_vals = sliding_window_minimum(nums, k)
    
    return [max_val - min_val for max_val, min_val in zip(max_vals, min_vals)]


class MonotonicQueue:
    """
    Monotonic Queue implementation for sliding window problems
    Maintains elements in decreasing order for maximum queries
    """
    
    def __init__(self):
        """Initialize with a deque"""
        self.dq = deque()
    
    def push(self, val):
        """
        Add element while maintaining decreasing order
        Remove all smaller elements from rear
        """
        while self.dq and self.dq[-1] < val:
            self.dq.pop()
        self.dq.append(val)
    
    def pop(self, val):
        """
        Remove element if it's at the front
        Only remove if it matches (it might have been removed during push)
        """
        if self.dq and self.dq[0] == val:
            self.dq.popleft()
    
    def get_max(self):
        """Get maximum element (front of deque)"""
        return self.dq[0] if self.dq else None
    
    def __len__(self):
        """Return size of queue"""
        return len(self.dq)


def sliding_window_max_with_monotonic_queue(nums, k):
    """
    Using MonotonicQueue class for cleaner implementation
    
    Time: O(n)
    Space: O(k)
    """
    if not nums or k <= 0:
        return []
    
    result = []
    mq = MonotonicQueue()
    
    # Initialize first window
    for i in range(k):
        mq.push(nums[i])
    result.append(mq.get_max())
    
    # Process remaining windows
    for i in range(k, len(nums)):
        mq.pop(nums[i - k])  # Remove element leaving window
        mq.push(nums[i])     # Add new element
        result.append(mq.get_max())
    
    return result


def max_sum_subarray_of_size_k(nums, k):
    """
    Find maximum sum of any subarray of size k
    Uses sliding window technique
    
    Time: O(n)
    Space: O(1)
    """
    if not nums or k <= 0 or k > len(nums):
        return 0
    
    # Calculate sum of first window
    window_sum = sum(nums[:k])
    max_sum = window_sum
    
    # Slide the window
    for i in range(k, len(nums)):
        window_sum = window_sum - nums[i - k] + nums[i]
        max_sum = max(max_sum, window_sum)
    
    return max_sum


# Example usage and test cases
if __name__ == "__main__":
    print("=" * 60)
    print("Sliding Window Maximum Problem")
    print("=" * 60)
    
    # Test case 1
    arr1 = [1, 3, -1, -3, 5, 3, 6, 7]
    k1 = 3
    print(f"\nArray: {arr1}")
    print(f"Window size: {k1}")
    print(f"Maximum in each window: {sliding_window_maximum(arr1, k1)}")
    print(f"Expected: [3, 3, 5, 5, 6, 7]")
    
    # Test case 2
    arr2 = [1, -1]
    k2 = 1
    print(f"\nArray: {arr2}")
    print(f"Window size: {k2}")
    print(f"Maximum in each window: {sliding_window_maximum(arr2, k2)}")
    print(f"Expected: [1, -1]")
    
    # Test case 3
    arr3 = [9, 11]
    k3 = 2
    print(f"\nArray: {arr3}")
    print(f"Window size: {k3}")
    print(f"Maximum in each window: {sliding_window_maximum(arr3, k3)}")
    print(f"Expected: [11]")
    
    # Test case 4 - Larger window
    arr4 = [4, -2, 8, 1, 6, 3, 7, 2]
    k4 = 4
    print(f"\nArray: {arr4}")
    print(f"Window size: {k4}")
    print(f"Maximum in each window: {sliding_window_maximum(arr4, k4)}")
    print(f"Expected: [8, 8, 8, 7, 7]")
    
    print("\n" + "=" * 60)
    print("Sliding Window Minimum")
    print("=" * 60)
    
    print(f"\nArray: {arr1}")
    print(f"Window size: {k1}")
    print(f"Minimum in each window: {sliding_window_minimum(arr1, k1)}")
    print(f"Expected: [-1, -3, -3, -3, 3, 3]")
    
    print("\n" + "=" * 60)
    print("Sliding Window Range (Max - Min)")
    print("=" * 60)
    
    print(f"\nArray: {arr1}")
    print(f"Window size: {k1}")
    print(f"Range in each window: {sliding_window_range(arr1, k1)}")
    
    print("\n" + "=" * 60)
    print("Using Monotonic Queue Class")
    print("=" * 60)
    
    print(f"\nArray: {arr1}")
    print(f"Window size: {k1}")
    print(f"Maximum in each window: {sliding_window_max_with_monotonic_queue(arr1, k1)}")
    
    print("\n" + "=" * 60)
    print("Maximum Sum Subarray of Size K")
    print("=" * 60)
    
    arr5 = [2, 1, 5, 1, 3, 2]
    k5 = 3
    print(f"\nArray: {arr5}")
    print(f"Window size: {k5}")
    print(f"Maximum sum: {max_sum_subarray_of_size_k(arr5, k5)}")
    print(f"Expected: 9 (subarray [5, 1, 3])")
    
    arr6 = [2, 3, 4, 1, 5]
    k6 = 2
    print(f"\nArray: {arr6}")
    print(f"Window size: {k6}")
    print(f"Maximum sum: {max_sum_subarray_of_size_k(arr6, k6)}")
    print(f"Expected: 7 (subarray [3, 4])")
    
    print("\n" + "=" * 60)
    print("Performance Comparison: Naive vs Optimized")
    print("=" * 60)
    
    import time
    import random
    
    # Generate large test array
    large_arr = [random.randint(-100, 100) for _ in range(10000)]
    k_large = 100
    
    # Test optimized approach
    start = time.time()
    result_opt = sliding_window_maximum(large_arr, k_large)
    time_opt = time.time() - start
    
    # Test naive approach
    start = time.time()
    result_naive = sliding_window_maximum_naive(large_arr, k_large)
    time_naive = time.time() - start
    
    print(f"\nArray size: {len(large_arr)}")
    print(f"Window size: {k_large}")
    print(f"\nOptimized (Deque) approach: {time_opt:.6f}s")
    print(f"Naive approach: {time_naive:.6f}s")
    print(f"Speedup: {time_naive/time_opt:.1f}x")
    print(f"Results match: {result_opt == result_naive}")
    
    print("\n" + "=" * 60)
    print("Step-by-Step Example")
    print("=" * 60)
    
    arr_demo = [1, 3, -1, -3, 5, 3, 6, 7]
    k_demo = 3
    print(f"\nArray: {arr_demo}")
    print(f"Window size: {k_demo}\n")
    
    dq = deque()
    result = []
    
    for i in range(len(arr_demo)):
        # Remove elements outside window
        if dq and dq[0] < i - k_demo + 1:
            print(f"Step {i+1}: Remove index {dq[0]} (outside window)")
            dq.popleft()
        
        # Remove smaller elements
        removed = []
        while dq and arr_demo[dq[-1]] < arr_demo[i]:
            removed.append(dq.pop())
        if removed:
            print(f"Step {i+1}: Remove indices {removed} (smaller than {arr_demo[i]})")
        
        # Add current element
        dq.append(i)
        print(f"Step {i+1}: Add index {i} (value={arr_demo[i]}), Deque indices: {list(dq)}")
        
        # Add to result if window complete
        if i >= k_demo - 1:
            max_val = arr_demo[dq[0]]
            result.append(max_val)
            print(f"         Window [{i-k_demo+1}:{i+1}] = {arr_demo[i-k_demo+1:i+1]}, Max = {max_val}\n")
    
    print(f"Final result: {result}")
