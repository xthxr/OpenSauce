"""
15. 3Sum

Given an integer array nums, return all unique triplets [nums[i], nums[j], nums[k]] 
such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.

The solution set must not contain duplicate triplets.

Approach:
1. Sort the array (important for handling duplicates efficiently).
2. Iterate each number as a potential first element of the triplet.
3. For each number, use the two-pointer technique to find pairs that sum to -nums[i].
4. Skip duplicates to ensure unique triplets.

Time Complexity: O(n^2)
Space Complexity: O(1) (excluding output list)

Example 1:
    Input: nums = [-1,0,1,2,-1,-4]
    Output: [[-1,-1,2],[-1,0,1]]

Example 2:
    Input: nums = [0,1,1]
    Output: []

Example 3:
    Input: nums = [0,0,0]
    Output: [[0,0,0]]

Constraints:
    3 <= nums.length <= 3000
    -10^5 <= nums[i] <= 10^5
"""

class Solution(object):
    def threeSum(self, nums):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """
        res = []
        nums.sort()  # Sort the array for two-pointer approach
        
        for i in range(len(nums)):
            # Skip duplicate numbers for the first element
            if i > 0 and nums[i] == nums[i - 1]:
                continue

            # Two-pointer initialization
            left, right = i + 1, len(nums) - 1

            while left < right:
                total = nums[i] + nums[left] + nums[right]

                if total == 0:
                    res.append([nums[i], nums[left], nums[right]])
                    # Skip duplicates for left and right
                    while left < right and nums[left] == nums[left + 1]:
                        left += 1
                    while left < right and nums[right] == nums[right - 1]:
                        right -= 1
                    left += 1
                    right -= 1

                elif total < 0:
                    left += 1  # Need a bigger sum
                else:
                    right -= 1  # Need a smaller sum

        return res


# Example Usage
if __name__ == "__main__":
    obj = Solution()
    test_cases = [
        [-1, 0, 1, 2, -1, -4],
        [0, 1, 1],
        [0, 0, 0],
        [3, -2, 1, 0]
    ]

    for nums in test_cases:
        print("Input:", nums)
        print("Triplets that sum to 0:", obj.threeSum(nums))
        print("-" * 40)
