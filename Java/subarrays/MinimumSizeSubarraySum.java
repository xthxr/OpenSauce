/**
 * Minimum Size Subarray Sum
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 * 
 * Find the minimal length of a contiguous subarray whose sum is >= target.
 * Uses sliding window technique with two pointers.
 * 
 * LeetCode Problem #209: https://leetcode.com/problems/minimum-size-subarray-sum/
 */
public class MinimumSizeSubarraySum {
    
    /**
     * Finds the minimal length of subarray with sum >= target.
     * 
     * @param target Target sum
     * @param nums Array of positive integers
     * @return Minimal length, or 0 if no such subarray exists
     */
    public static int minSubArrayLen(int target, int[] nums) {
        if (nums == null || nums.length == 0) {
            return 0;
        }
        
        int minLength = Integer.MAX_VALUE;
        int left = 0;
        int currentSum = 0;
        
        for (int right = 0; right < nums.length; right++) {
            currentSum += nums[right];
            
            // Shrink window while sum is still >= target
            while (currentSum >= target) {
                minLength = Math.min(minLength, right - left + 1);
                currentSum -= nums[left];
                left++;
            }
        }
        
        return minLength == Integer.MAX_VALUE ? 0 : minLength;
    }
    
    /**
     * Finds the minimal length subarray and returns start and end indices.
     * 
     * @param target Target sum
     * @param nums Array of positive integers
     * @return Array containing [start_index, end_index, length] or [-1, -1, 0] if not found
     */
    public static int[] minSubArrayLenWithIndices(int target, int[] nums) {
        if (nums == null || nums.length == 0) {
            return new int[]{-1, -1, 0};
        }
        
        int minLength = Integer.MAX_VALUE;
        int left = 0;
        int currentSum = 0;
        int bestStart = -1, bestEnd = -1;
        
        for (int right = 0; right < nums.length; right++) {
            currentSum += nums[right];
            
            while (currentSum >= target) {
                if (right - left + 1 < minLength) {
                    minLength = right - left + 1;
                    bestStart = left;
                    bestEnd = right;
                }
                currentSum -= nums[left];
                left++;
            }
        }
        
        return minLength == Integer.MAX_VALUE ? 
               new int[]{-1, -1, 0} : 
               new int[]{bestStart, bestEnd, minLength};
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Minimum Size Subarray Sum ===\n");
        
        // Test case 1: Basic example
        int[] arr1 = {2, 3, 1, 2, 4, 3};
        int target1 = 7;
        System.out.println("Test 1: [2,3,1,2,4,3], target = " + target1);
        System.out.println("Minimum length: " + minSubArrayLen(target1, arr1));
        int[] result1 = minSubArrayLenWithIndices(target1, arr1);
        if (result1[0] != -1) {
            System.out.print("Subarray: [");
            for (int i = result1[0]; i <= result1[1]; i++) {
                System.out.print(arr1[i] + (i < result1[1] ? "," : ""));
            }
            System.out.println("]\n");
        }
        
        // Test case 2: No valid subarray
        int[] arr2 = {1, 1, 1, 1, 1};
        int target2 = 11;
        System.out.println("Test 2: [1,1,1,1,1], target = " + target2);
        System.out.println("Minimum length: " + minSubArrayLen(target2, arr2) + "\n");
        
        // Test case 3: Single element is enough
        int[] arr3 = {1, 4, 4};
        int target3 = 4;
        System.out.println("Test 3: [1,4,4], target = " + target3);
        System.out.println("Minimum length: " + minSubArrayLen(target3, arr3) + "\n");
        
        // Test case 4: Entire array needed
        int[] arr4 = {1, 2, 3, 4, 5};
        int target4 = 15;
        System.out.println("Test 4: [1,2,3,4,5], target = " + target4);
        System.out.println("Minimum length: " + minSubArrayLen(target4, arr4) + "\n");
    }
}
