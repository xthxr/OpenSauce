"""
Valid Palindrome

A phrase is a palindrome if, after converting all uppercase letters into lowercase letters 
and removing all non-alphanumeric characters, it reads the same forward and backward. 
Alphanumeric characters include letters and numbers.

Given a string s, return True if it is a palindrome, or False otherwise.

Example 1:
    Input: s = "A man, a plan, a canal: Panama"
    Output: True
    Explanation: "amanaplanacanalpanama" is a palindrome.

Example 2:
    Input: s = "race a car"
    Output: False
    Explanation: "raceacar" is not a palindrome.

Example 3:
    Input: s = " "
    Output: True
    Explanation: s is an empty string "" after removing non-alphanumeric characters.
                 Since an empty string reads the same forward and backward, it is a palindrome.

Constraints:
- 1 <= s.length <= 2 * 10^5
- s consists only of printable ASCII characters.
"""

class Solution(object):
    def isPalindrome(self, s):
        """
        Check if the string s is a valid palindrome ignoring case and non-alphanumeric characters.
        """
        # Filter only alphanumeric characters and convert to lowercase
        filtered = [char.lower() for char in s if char.isalnum()]
        
        # Compare filtered list with its reverse
        return filtered == filtered[::-1]

# Example Usage
if __name__ == "__main__":
    test_cases = [
        "A man, a plan, a canal: Panama",
        "race a car",
        " ",
        "No lemon, no melon",
        "Was it a car or a cat I saw?"
    ]
    
    obj = Solution()
    
    for s in test_cases:
        print("Input String:", s)
        print("Is Valid Palindrome:", obj.isPalindrome(s))
        print("-" * 30)
