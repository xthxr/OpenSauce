"""
Find the Index of the First Occurrence in a String

Given two strings needle and haystack, return the index of the first occurrence 
of needle in haystack, or -1 if needle is not part of haystack.

Example 1:
    Input: haystack = "sadbutsad", needle = "sad"
    Output: 0
    Explanation: "sad" occurs at index 0 and 6. The first occurrence is at index 0.

Example 2:
    Input: haystack = "leetcode", needle = "leeto"
    Output: -1
    Explanation: "leeto" did not occur in "leetcode", so we return -1.

Constraints:
- 1 <= haystack.length, needle.length <= 10^4
- haystack and needle consist of only lowercase English characters.
"""

class Solution(object):
    def strStr(self, haystack, needle):
        """
        Return the index of the first occurrence of needle in haystack, or -1 if not found.
        """
        if not needle:
            return 0  # Empty needle occurs at index 0
        
        # Loop through haystack and check substring matches
        for i in range(len(haystack) - len(needle) + 1):
            if haystack[i:i+len(needle)] == needle:
                return i
        
        return -1

# Example Usage
if __name__ == "__main__":
    test_cases = [
        ("sadbutsad", "sad"),
        ("leetcode", "leeto"),
        ("hello", "ll"),
        ("aaaaa", "bba"),
        ("abc", "")
    ]
    
    obj = Solution()
    
    for haystack, needle in test_cases:
        print("Haystack:", haystack)
        print("Needle:", needle)
        print("First Occurrence Index:", obj.strStr(haystack, needle))
        print("-" * 30)
