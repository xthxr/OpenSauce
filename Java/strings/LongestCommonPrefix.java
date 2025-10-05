//  This is Leetcode based DSA question --
// ✅ Problem Statement

/**
 * Longest Common Prefix
 *
 * Time Complexity: O(n * m)   // n = number of strings, m = length of shortest string
 * Space Complexity: O(1)
 *
 * Finds the longest prefix string common to all given strings.
 */

public class LongestCommonPrefix {

    /**
     * Finds the longest common prefix among a list of strings.
     *
     * @param strs Array of strings
     * @return     Longest common prefix
     */
    public String longestCommonPrefix(String[] strs) {
        if (strs == null || strs.length == 0) return "";

        String prefix = strs[0];

        for (int i = 1; i < strs.length; i++) {
            while (strs[i].indexOf(prefix) != 0) {
                prefix = prefix.substring(0, prefix.length() - 1);
                if (prefix.isEmpty()) return "";
            }
        }

        return prefix;
    }

    // Example usage
    public static void main(String[] args) {
        LongestCommonPrefix solver = new LongestCommonPrefix();
        String[] words = {"flower", "flow", "flight"};

        System.out.println("Longest Common Prefix: " + solver.longestCommonPrefix(words));
    }
}


