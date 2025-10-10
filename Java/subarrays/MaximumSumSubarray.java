/**
 * Maximum Sum Subarray (Kadane's Algorithm)
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 * 
 * Find the contiguous subarray with the maximum sum.
 * This is one of the most fundamental subarray problems.
 * 
 * LeetCode Problem #53: https://leetcode.com/problems/maximum-subarray/
 */
public class MaximumSumSubarray {
    
    /**
     * Finds the maximum sum of any contiguous subarray using Kadane's Algorithm.
     * 
     * @param nums Array of integers
     * @return Maximum sum of contiguous subarray
     */
    public static int maxSubArray(int[] nums) {
        if (nums == null || nums.length == 0) {
            throw new IllegalArgumentException("Array cannot be null or empty");
        }
        
        int maxSum = nums[0];
        int currentSum = nums[0];
        
        for (int i = 1; i < nums.length; i++) {
            // Either extend the existing subarray or start a new one
            currentSum = Math.max(nums[i], currentSum + nums[i]);
            maxSum = Math.max(maxSum, currentSum);
        }
        
        return maxSum;
    }
    
    /**
     * Finds the maximum sum subarray and returns the subarray itself.
     * 
     * @param nums Array of integers
     * @return Array containing [start_index, end_index, max_sum]
     */
    public static int[] maxSubArrayWithIndices(int[] nums) {
        if (nums == null || nums.length == 0) {
            throw new IllegalArgumentException("Array cannot be null or empty");
        }
        
        int maxSum = nums[0];
        int currentSum = nums[0];
        int start = 0, end = 0, tempStart = 0;
        
        for (int i = 1; i < nums.length; i++) {
            if (currentSum < 0) {
                currentSum = nums[i];
                tempStart = i;
            } else {
                currentSum += nums[i];
            }
            
            if (currentSum > maxSum) {
                maxSum = currentSum;
                start = tempStart;
                end = i;
            }
        }
        
        return new int[]{start, end, maxSum};
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Maximum Sum Subarray (Kadane's Algorithm) ===\n");
        
        // Test case 1: Mixed positive and negative numbers
        int[] arr1 = {-2, 1, -3, 4, -1, 2, 1, -5, 4};
        System.out.println("Test 1: [-2, 1, -3, 4, -1, 2, 1, -5, 4]");
        System.out.println("Maximum Sum: " + maxSubArray(arr1));
        int[] result1 = maxSubArrayWithIndices(arr1);
        System.out.println("Subarray indices: [" + result1[0] + ", " + result1[1] + "]");
        System.out.print("Subarray: [");
        for (int i = result1[0]; i <= result1[1]; i++) {
            System.out.print(arr1[i] + (i < result1[1] ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Test case 2: All negative numbers
        int[] arr2 = {-5, -2, -8, -1};
        System.out.println("Test 2: [-5, -2, -8, -1]");
        System.out.println("Maximum Sum: " + maxSubArray(arr2));
        int[] result2 = maxSubArrayWithIndices(arr2);
        System.out.println("Subarray indices: [" + result2[0] + ", " + result2[1] + "]\n");
        
        // Test case 3: All positive numbers
        int[] arr3 = {1, 2, 3, 4, 5};
        System.out.println("Test 3: [1, 2, 3, 4, 5]");
        System.out.println("Maximum Sum: " + maxSubArray(arr3));
        int[] result3 = maxSubArrayWithIndices(arr3);
        System.out.println("Subarray indices: [" + result3[0] + ", " + result3[1] + "]\n");
        
        // Test case 4: Single element
        int[] arr4 = {-3};
        System.out.println("Test 4: [-3]");
        System.out.println("Maximum Sum: " + maxSubArray(arr4) + "\n");
    }
}
