"""
🧩 LeetCode #238 - Product of Array Except Self
------------------------------------------------
Given an integer array nums, return an array answer such that answer[i]
is equal to the product of all the elements of nums except nums[i].
Solve without using division and in O(n) time complexity.
"""

from typing import List

class Solution:
    def productExceptSelf(self, nums: List[int]) -> List[int]:
        n = len(nums)
        answer = [1] * n

        # Compute prefix products
        prefix = 1
        for i in range(n):
            answer[i] = prefix
            prefix *= nums[i]

        # Compute suffix products and multiply
        suffix = 1
        for i in range(n - 1, -1, -1):
            answer[i] *= suffix
            suffix *= nums[i]

        return answer
