"""
Power of Three

Given an integer n, return True if it is a power of three. Otherwise, return False.
An integer n is a power of three if there exists an integer x such that n == 3^x.

Example 1:
    Input: n = 27
    Output: True
    Explanation: 27 = 3^3

Example 2:
    Input: n = 0
    Output: False
    Explanation: There is no x where 3^x = 0.

Example 3:
    Input: n = -1
    Output: False
    Explanation: There is no x where 3^x = -1.

Constraints:
- -2^31 <= n <= 2^31 - 1

Follow-up: Could you solve it without loops/recursion?
"""

class Solution(object):
    def isPowerOfThree(self, n):
        """
        Check if n is a power of three.
        Uses the mathematical property: if n > 0 and 1162261467 % n == 0, then n is a power of three.
        (1162261467 is 3^19, the largest power of 3 that fits in 32-bit signed integer)
        """
        return n > 0 and 1162261467 % n == 0

# Example Usage
if __name__ == "__main__":
    test_cases = [27, 0, -1, 1, 9, 45]
    
    obj = Solution()
    
    for n in test_cases:
        print("Input:", n)
        print("Is Power of Three:", obj.isPowerOfThree(n))
        print("-" * 30)
