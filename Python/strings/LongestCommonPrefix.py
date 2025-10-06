"""
Longest Common Prefix

Write a function to find the longest common prefix string amongst an array of strings.
If there is no common prefix, return an empty string "".

Example 1:
    Input: strs = ["flower","flow","flight"]
    Output: "fl"

Example 2:
    Input: strs = ["dog","racecar","car"]
    Output: ""
    Explanation: There is no common prefix among the input strings.

Constraints:
- 1 <= strs.length <= 200
- 0 <= strs[i].length <= 200
- strs[i] consists of only lowercase English letters if it is non-empty.
"""

class Solution(object):
    def longestCommonPrefix(self, strs):
        """
        Find the longest common prefix among a list of strings.
        """
        if not strs:
            return ""
        
        # Start with the first string as the prefix
        prefix = strs[0]
        
        for s in strs[1:]:
            # Shorten the prefix until it matches the start of the current string
            while not s.startswith(prefix):
                prefix = prefix[:-1]
                if not prefix:
                    return ""
        
        return prefix

# Example Usage
if __name__ == "__main__":
    test_cases = [
        ["flower","flow","flight"],
        ["dog","racecar","car"],
        ["interview","internet","internal"],
        ["a","a","a"],
        ["", "b", "c"]
    ]
    
    obj = Solution()
    
    for strs in test_cases:
        print("Input Strings:", strs)
        print("Longest Common Prefix:", obj.longestCommonPrefix(strs))
        print("-" * 30)
