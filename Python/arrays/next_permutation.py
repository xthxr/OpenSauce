"""
Next Permutation of an Array

A permutation of an array of integers is an arrangement of its members into a sequence or linear order.

The next permutation of an array of integers is the next lexicographically greater permutation.
If no such permutation exists (the array is in descending order), rearrange the array to the lowest possible order (ascending).

Example 1:
    Input: nums = [1,2,3]
    Output: [1,3,2]

Example 2:
    Input: nums = [3,2,1]
    Output: [1,2,3]

Example 3:
    Input: nums = [1,1,5]
    Output: [1,5,1]

Constraints:
- 1 <= nums.length <= 100
- 0 <= nums[i] <= 100
"""

class Solution(object):
    def nextPermutation(self, nums):
        """
        Modify nums in-place to its next lexicographical permutation.
        """
        n = len(nums)
        i = n - 2

        # Step 1: Find the first decreasing element from the right
        while i >= 0 and nums[i] >= nums[i + 1]:
            i -= 1

        # Step 2: If we found such element, find the number just larger than it to swap
        if i >= 0:
            j = n - 1
            while nums[j] <= nums[i]:
                j -= 1
            # Swap
            nums[i], nums[j] = nums[j], nums[i]

        # Step 3: Reverse the portion of the array to the right of i
        left, right = i + 1, n - 1
        while left < right:
            nums[left], nums[right] = nums[right], nums[left]
            left += 1
            right -= 1

# Example Usage
if __name__ == "__main__":
    nums_list = [
        [1, 2, 3],
        [3, 2, 1],
        [1, 1, 5]
    ]
    
    obj = Solution()
    
    for nums in nums_list:
        print("Original Array:", nums)
        obj.nextPermutation(nums)
        print("Next Permutation:", nums)
        print("-" * 30)
