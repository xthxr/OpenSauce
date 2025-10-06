"""
Longest Palindromic Substring

Given a string s, return the longest palindromic substring in s.

Example 1:
    Input: s = "babad"
    Output: "bab"
    Explanation: "aba" is also a valid answer.

Example 2:
    Input: s = "cbbd"
    Output: "bb"

Constraints:
- 1 <= s.length <= 1000
- s consists of only digits and English letters.
"""

class Solution(object):
    def longestPalindrome(self, s):
        """
        Find the longest palindromic substring using expand around center approach.
        """
        if not s:
            return ""
        
        start, end = 0, 0
        
        def expandAroundCenter(left, right):
            while left >= 0 and right < len(s) and s[left] == s[right]:
                left -= 1
                right += 1
            return left + 1, right - 1
        
        for i in range(len(s)):
            # Odd length palindrome
            l1, r1 = expandAroundCenter(i, i)
            # Even length palindrome
            l2, r2 = expandAroundCenter(i, i + 1)
            
            # Update longest palindrome
            if r1 - l1 > end - start:
                start, end = l1, r1
            if r2 - l2 > end - start:
                start, end = l2, r2
        
        return s[start:end+1]

# Example Usage
if __name__ == "__main__":
    test_cases = ["babad", "cbbd", "a", "ac", "racecar"]
    
    obj = Solution()
    
    for s in test_cases:
        print("Input String:", s)
        print("Longest Palindromic Substring:", obj.longestPalindrome(s))
        print("-" * 30)
