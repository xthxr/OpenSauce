"""
53. Maximum Subarray

Given an integer array nums, find the subarray with the largest sum, and return its sum.

Approach (Kadane’s Algorithm):
1. Iterate through the array while maintaining two values:
   - current_sum: maximum subarray sum ending at the current index
   - max_sum: global maximum subarray sum found so far
2. For each element:
   - Add it to current_sum.
   - If current_sum becomes smaller than the current element, reset it to the element.
   - Update max_sum whenever current_sum exceeds it.

Time Complexity: O(n)
Space Complexity: O(1)

Example 1:
    Input: nums = [-2,1,-3,4,-1,2,1,-5,4]
    Output: 6
    Explanation: The subarray [4,-1,2,1] has the largest sum = 6.

Example 2:
    Input: nums = [1]
    Output: 1

Example 3:
    Input: nums = [5,4,-1,7,8]
    Output: 23

Constraints:
    1 <= nums.length <= 10^5
    -10^4 <= nums[i] <= 10^4
"""

class Solution(object):
    def maxSubArray(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """
        max_sum = current_sum = nums[0]

        for i in range(1, len(nums)):
            # Either extend the current subarray or start a new one
            current_sum = max(nums[i], current_sum + nums[i])
            # Update the global maximum sum
            max_sum = max(max_sum, current_sum)

        return max_sum


# Example Usage
if __name__ == "__main__":
    obj = Solution()
    test_cases = [
        [-2, 1, -3, 4, -1, 2, 1, -5, 4],
        [1],
        [5, 4, -1, 7, 8],
        [-1, -2, -3, -4]
    ]

    for nums in test_cases:
        print("Input:", nums)
        print("Maximum Subarray Sum:", obj.maxSubArray(nums))
        print("-" * 40)
