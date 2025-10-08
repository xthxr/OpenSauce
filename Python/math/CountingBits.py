"""
Counting Bits

Given an integer n, return an array ans of length n + 1 such that for each i (0 <= i <= n), 
ans[i] is the number of 1's in the binary representation of i.

Example 1:
    Input: n = 2
    Output: [0,1,1]
    Explanation:
        0 --> 0
        1 --> 1
        2 --> 10

Example 2:
    Input: n = 5
    Output: [0,1,1,2,1,2]
    Explanation:
        0 --> 0
        1 --> 1
        2 --> 10
        3 --> 11
        4 --> 100
        5 --> 101

Constraints:
- 0 <= n <= 10^5
"""

class Solution(object):
    def countBits(self, n):
        """
        Return a list where each element represents the number of 1's in the binary representation of its index.
        Uses dynamic programming: ans[i] = ans[i >> 1] + (i & 1)
        """
        ans = [0] * (n + 1)
        for i in range(1, n + 1):
            ans[i] = ans[i >> 1] + (i & 1)
        return ans

# Example Usage
if __name__ == "__main__":
    test_cases = [2, 5, 10]
    
    obj = Solution()
    
    for n in test_cases:
        print("Input n:", n)
        print("Counting Bits:", obj.countBits(n))
        print("-" * 30)
