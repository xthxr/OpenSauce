"""
Palindrome Number

Given an integer x, return True if x is a palindrome, and False otherwise.
A palindrome number reads the same backward as forward.

Example 1:
    Input: x = 121
    Output: True
    Explanation: 121 reads as 121 from left to right and from right to left.

Example 2:
    Input: x = -121
    Output: False
    Explanation: From left to right, it reads -121. From right to left, it becomes 121-. Therefore it is not a palindrome.

Example 3:
    Input: x = 10
    Output: False
    Explanation: Reads 01 from right to left. Therefore it is not a palindrome.

Constraints:
- -2^31 <= x <= 2^31 - 1
"""

class Solution(object):
    def isPalindrome(self, x):
        """
        Check if the integer x is a palindrome.
        """
        # Negative numbers are not palindrome
        if x < 0:
            return False
        
        # Convert integer to string and compare with its reverse
        str_x = str(x)
        return str_x == str_x[::-1]

# Example Usage
if __name__ == "__main__":
    test_cases = [121, -121, 10, 12321, 0]
    
    obj = Solution()
    
    for x in test_cases:
        print("Input:", x)
        print("Is Palindrome:", obj.isPalindrome(x))
        print("-" * 30)
