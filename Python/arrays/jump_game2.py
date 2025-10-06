"""
Problem: Jump Game II
https://leetcode.com/problems/jump-game-ii

Approach:
 - Use a greedy algorithm.
 - Track the farthest index reachable in the current range.
 - When reaching the end of that range, increment the jump count and update the range.
 - Repeat until the last index is reachable.

Time Complexity: O(n)
Space Complexity: O(1)
"""

from typing import List


class Solution:
    def jump(self, nums: List[int]) -> int:
        """
        Returns the minimum number of jumps needed to reach the last index.
        
        Args:
            nums (List[int]): Array where each element represents the maximum jump length from that position.

        Returns:
            int: Minimum number of jumps required to reach the end.
        """
        n = len(nums)
        jumps = 0          # Total jumps made
        farthest = 0       # Farthest reachable index in the current range
        end = 0            # End index of the current jump range

        # Traverse the array (excluding the last index)
        for i in range(n - 1):
            farthest = max(farthest, i + nums[i])

            # When we reach the end of the current range, make a jump
            if i == end:
                jumps += 1
                end = farthest

                # Optional optimization: break early if we can already reach the end
                if end >= n - 1:
                    break

        return jumps



# Example Test Runs

if __name__ == "__main__":
    solution = Solution()
    test_cases = [
        ([2, 3, 1, 1, 4], 2),
        ([2, 3, 0, 1, 4], 2),
        ([1, 2, 3], 2),
        ([0], 0),
        ([1, 1, 1, 1], 3)
    ]

    for nums, expected in test_cases:
        result = solution.jump(nums)
        print(f"nums = {nums} → jumps = {result} (expected: {expected})")
