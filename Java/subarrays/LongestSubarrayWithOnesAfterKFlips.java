/**
 * Longest Subarray with 1s After K Flips
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 * 
 * Find the length of longest subarray with all 1s after flipping at most K zeros.
 * Uses sliding window technique with two pointers.
 * 
 * LeetCode Problem #1004: https://leetcode.com/problems/max-consecutive-ones-iii/
 */
public class LongestSubarrayWithOnesAfterKFlips {
    
    /**
     * Finds the length of longest subarray with 1s after flipping at most k zeros.
     * 
     * @param arr Binary array (0s and 1s)
     * @param k Maximum number of flips allowed
     * @return Length of longest subarray
     */
    public static int longestOnes(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        int maxLength = 0;
        int left = 0;
        int zeroCount = 0;
        
        for (int right = 0; right < arr.length; right++) {
            // Count zeros in current window
            if (arr[right] == 0) {
                zeroCount++;
            }
            
            // Shrink window if we have more than k zeros
            while (zeroCount > k) {
                if (arr[left] == 0) {
                    zeroCount--;
                }
                left++;
            }
            
            // Update maximum length
            maxLength = Math.max(maxLength, right - left + 1);
        }
        
        return maxLength;
    }
    
    /**
     * Finds the longest subarray and returns start and end indices.
     * 
     * @param arr Binary array (0s and 1s)
     * @param k Maximum number of flips allowed
     * @return Array containing [start_index, end_index, length]
     */
    public static int[] longestOnesWithIndices(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return new int[]{-1, -1, 0};
        }
        
        int maxLength = 0;
        int left = 0;
        int zeroCount = 0;
        int bestStart = 0, bestEnd = 0;
        
        for (int right = 0; right < arr.length; right++) {
            if (arr[right] == 0) {
                zeroCount++;
            }
            
            while (zeroCount > k) {
                if (arr[left] == 0) {
                    zeroCount--;
                }
                left++;
            }
            
            if (right - left + 1 > maxLength) {
                maxLength = right - left + 1;
                bestStart = left;
                bestEnd = right;
            }
        }
        
        return new int[]{bestStart, bestEnd, maxLength};
    }
    
    /**
     * Finds longest consecutive 1s without any flips.
     * 
     * @param arr Binary array (0s and 1s)
     * @return Length of longest consecutive 1s
     */
    public static int longestConsecutiveOnes(int[] arr) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        int maxLength = 0;
        int currentLength = 0;
        
        for (int num : arr) {
            if (num == 1) {
                currentLength++;
                maxLength = Math.max(maxLength, currentLength);
            } else {
                currentLength = 0;
            }
        }
        
        return maxLength;
    }
    
    /**
     * Finds longest subarray with at most k zeros (alternative problem).
     * 
     * @param arr Array of integers
     * @param k Maximum number of zeros allowed
     * @return Length of longest subarray with at most k zeros
     */
    public static int longestSubarrayWithKZeros(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        int maxLength = 0;
        int left = 0;
        int zeroCount = 0;
        
        for (int right = 0; right < arr.length; right++) {
            if (arr[right] == 0) {
                zeroCount++;
            }
            
            while (zeroCount > k && left <= right) {
                if (arr[left] == 0) {
                    zeroCount--;
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
        System.out.println("=== Longest Subarray with 1s After K Flips ===\n");
        
        // Test case 1: Basic example
        int[] arr1 = {1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0};
        int k1 = 2;
        System.out.println("Test 1: [1,1,1,0,0,0,1,1,1,1,0], k = " + k1);
        System.out.println("Longest length: " + longestOnes(arr1, k1));
        int[] result1 = longestOnesWithIndices(arr1, k1);
        System.out.print("Subarray (indices " + result1[0] + " to " + result1[1] + "): [");
        for (int i = result1[0]; i <= result1[1]; i++) {
            System.out.print(arr1[i] + (i < result1[1] ? "," : ""));
        }
        System.out.println("]\n");
        
        // Test case 2: All zeros
        int[] arr2 = {0, 0, 0, 0};
        int k2 = 2;
        System.out.println("Test 2: [0,0,0,0], k = " + k2);
        System.out.println("Longest length: " + longestOnes(arr2, k2) + "\n");
        
        // Test case 3: All ones
        int[] arr3 = {1, 1, 1, 1};
        int k3 = 0;
        System.out.println("Test 3: [1,1,1,1], k = " + k3);
        System.out.println("Longest length: " + longestOnes(arr3, k3) + "\n");
        
        // Test case 4: No flips allowed
        int[] arr4 = {1, 0, 1, 1, 0, 1};
        int k4 = 0;
        System.out.println("Test 4: [1,0,1,1,0,1], k = " + k4);
        System.out.println("Longest length: " + longestOnes(arr4, k4));
        System.out.println("Longest consecutive 1s: " + longestConsecutiveOnes(arr4) + "\n");
        
        // Test case 5: Can flip all zeros
        int[] arr5 = {0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1};
        int k5 = 3;
        System.out.println("Test 5: [0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], k = " + k5);
        System.out.println("Longest length: " + longestOnes(arr5, k5));
        int[] result5 = longestOnesWithIndices(arr5, k5);
        System.out.println("Subarray indices: [" + result5[0] + ", " + result5[1] + "]\n");
        
        // Test case 6: Single element
        int[] arr6 = {0};
        int k6 = 1;
        System.out.println("Test 6: [0], k = " + k6);
        System.out.println("Longest length: " + longestOnes(arr6, k6) + "\n");
    }
}
