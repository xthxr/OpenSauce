/*
 Problem: Minimum Window Substring
 Category: Strings / Sliding Window
 Description: Find the smallest substring in 's' that contains all characters of 't'.
 Time Complexity: O(n)
 Space Complexity: O(1)  (constant 128 ASCII array)
*/

public class minWindowSubstring {
    public static void main(String[] args) {
        // Test Case 1
        String s1 = "ADOBECODEBANC";
        String t1 = "ABC";
        System.out.println("Test 1 Output: " + minWindow(s1, t1));
        // Expected: "BANC" (smallest substring containing A, B, and C)

        // Test Case 2
        String s2 = "a";
        String t2 = "a";
        System.out.println("Test 2 Output: " + minWindow(s2, t2));
        // Expected: "a" (both strings are same, window = "a")

        // Test Case 3
        String s3 = "a";
        String t3 = "aa";
        System.out.println("Test 3 Output: " + minWindow(s3, t3));
        // Expected: "" (no valid window - not enough 'a's)
    }

    public static String minWindow(String s, String t) {
        int [] map = new int[128];
        for (char c : t.toCharArray()) {
            map[c]++;
        }
        int start = 0, end = 0, minStart = 0, minLen = Integer.MAX_VALUE, counter = t.length();
        while (end < s.length()) {
            final char c1 = s.charAt(end);
            if (map[c1] > 0) counter--;
            map[c1]--;
            end++;
            while (counter == 0) {
                if (minLen > end - start) {
                    minLen = end - start;
                    minStart = start;
                }
                final char c2 = s.charAt(start);
                map[c2]++;
                if (map[c2] > 0) counter++;
                start++;
            }
        }

        return minLen == Integer.MAX_VALUE ? "" : s.substring(minStart, minStart + minLen);
    }
}
