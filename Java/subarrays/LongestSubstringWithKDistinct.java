import java.util.HashMap;

/**
 * Longest Substring with K Distinct Characters
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(k)
 * 
 * Find the length of longest substring with at most K distinct characters.
 * Uses sliding window with HashMap.
 */
public class LongestSubstringWithKDistinct {
    
    /**
     * Finds length of longest substring with at most k distinct characters.
     * 
     * @param s Input string
     * @param k Maximum distinct characters allowed
     * @return Length of longest valid substring
     */
    public static int lengthOfLongestSubstringKDistinct(String s, int k) {
        if (s == null || s.length() == 0 || k == 0) {
            return 0;
        }
        
        HashMap<Character, Integer> charCount = new HashMap<>();
        int maxLength = 0;
        int left = 0;
        
        for (int right = 0; right < s.length(); right++) {
            char rightChar = s.charAt(right);
            charCount.put(rightChar, charCount.getOrDefault(rightChar, 0) + 1);
            
            // Shrink window if we have more than k distinct characters
            while (charCount.size() > k) {
                char leftChar = s.charAt(left);
                charCount.put(leftChar, charCount.get(leftChar) - 1);
                if (charCount.get(leftChar) == 0) {
                    charCount.remove(leftChar);
                }
                left++;
            }
            
            maxLength = Math.max(maxLength, right - left + 1);
        }
        
        return maxLength;
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Longest Substring with K Distinct ===\n");
        
        // Test case 1
        String s1 = "eceba";
        int k1 = 2;
        System.out.println("Test 1: \"" + s1 + "\", k = " + k1);
        System.out.println("Length: " + lengthOfLongestSubstringKDistinct(s1, k1) + "\n");
        
        // Test case 2
        String s2 = "aa";
        int k2 = 1;
        System.out.println("Test 2: \"" + s2 + "\", k = " + k2);
        System.out.println("Length: " + lengthOfLongestSubstringKDistinct(s2, k2) + "\n");
    }
}
