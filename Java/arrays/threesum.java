//  This is Leetcode based DSA question --
// ✅ Problem Statement

// Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]] 
// such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.

/**
 * Three Sum Problem
 * 
 * Time Complexity: O(n^2)
 * Space Complexity: O(1) excluding output space
 * 
 * Finds all unique triplets in the array that sum to zero using two-pointer technique.
 */

import java.util.*;

public class ThreeSum {
    
    /**
     * Finds all unique triplets that sum to zero.
     * 
     * @param nums Array of integers
     * @return List of triplets that sum to zero
     */
    public List<List<Integer>> threeSum(int[] nums) {
        List<List<Integer>> result = new ArrayList<>();
        Arrays.sort(nums);
        
        for (int i = 0; i < nums.length - 2; i++) {
            if (i > 0 && nums[i] == nums[i - 1]) continue; // Skip duplicates
            
            int left = i + 1;
            int right = nums.length - 1;
            
            while (left < right) {
                int sum = nums[i] + nums[left] + nums[right];
                
                if (sum == 0) {
                    result.add(Arrays.asList(nums[i], nums[left], nums[right]));
                    
                    while (left < right && nums[left] == nums[left + 1]) left++;
                    while (left < right && nums[right] == nums[right - 1]) right--;
                    
                    left++;
                    right--;
                } else if (sum < 0) {
                    left++;
                } else {
                    right--;
                }
            }
        }
        
        return result;
    }
    
    /**
     * Main method with example usage
     */
    public static void main(String[] args) {
        ThreeSum solution = new ThreeSum();
        
        int[] nums1 = {-1, 0, 1, 2, -1, -4};
        System.out.println("Input: " + Arrays.toString(nums1));
        System.out.println("Output: " + solution.threeSum(nums1));
        
        int[] nums2 = {0, 1, 1};
        System.out.println("Input: " + Arrays.toString(nums2));
        System.out.println("Output: " + solution.threeSum(nums2));
    }
}