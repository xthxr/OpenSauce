"""
66. Plus One

You are given a large integer represented as an integer array digits, where each digits[i] is the ith digit of the integer.
The digits are ordered from most significant to least significant in left-to-right order. The large integer does not contain any leading 0's.

Increment the large integer by one and return the resulting array of digits.

Example 1:
    Input: digits = [1,2,3]
    Output: [1,2,4]
    Explanation: 123 + 1 = 124

Example 2:
    Input: digits = [4,3,2,1]
    Output: [4,3,2,2]

Example 3:
    Input: digits = [9]
    Output: [1,0]

Constraints:
    1 <= digits.length <= 100
    0 <= digits[i] <= 9
    digits does not contain any leading 0's
"""

class Solution(object):
    def plusOne(self, digits):
        """
        Increment the integer represented by digits by one.
        """
        n = len(digits)
        
        # Start adding from the last digit
        for i in range(n - 1, -1, -1):
            if digits[i] < 9:
                digits[i] += 1
                return digits
            # Set current digit to 0 and continue carry
            digits[i] = 0
        
        # If all digits were 9, we need an extra digit at the front
        return [1] + digits


# Example Usage
if __name__ == "__main__":
    obj = Solution()
    test_cases = [
        [1, 2, 3],
        [4, 3, 2, 1],
        [9],
        [9, 9, 9],
        [0]
    ]

    for digits in test_cases:
        print("Input:", digits)
        print("Plus One Result:", obj.plusOne(digits))
        print("-" * 30)
