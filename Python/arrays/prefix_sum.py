"""
Prefix Sum Array

Given an array of numbers, construct a prefix sum array where each element at index i 
contains the sum of all elements from index 0 to i in the original array.

Algorithm steps:
1. Initialize a prefix sum array of the same size as input.
2. Set the first element of prefix sum array equal to the first element of input array.
3. Iterate through the array from index 1:
    - prefix_sum[i] = prefix_sum[i-1] + arr[i]
4. Return the prefix sum array.

Example:
    Input: [1, 2, 3, 4]
    Output: [1, 3, 6, 10]

Time Complexity: O(n) where n is the number of elements
Space Complexity: O(n) for the prefix sum array
"""

class PrefixSum:
    @staticmethod
    def compute(arr):
        """
        Computes the prefix sum of the input array.
        """
        if not arr:
            return []
        
        n = len(arr)
        prefix_sum = [0] * n
        prefix_sum[0] = arr[0]
        
        for i in range(1, n):
            prefix_sum[i] = prefix_sum[i-1] + arr[i]
        
        return prefix_sum


# Example Usage
if __name__ == "__main__":
    test_cases = [
        [1, 2, 3, 4],
        [5, 3, 8, 6]
    ]

    for case in test_cases:
        print("Input Array:", case)
        print("Prefix Sum Array:", PrefixSum.compute(case))
        print("-" * 30)

