/**
 * Subarrays Divisible by K
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(k)
 * 
 * Count the number of subarrays whose sum is divisible by K.
 * Uses prefix sum modulo and HashMap technique.
 * 
 * LeetCode Problem #974: https://leetcode.com/problems/subarray-sums-divisible-by-k/
 */
public class SubarraysDivisibleByK {
    
    /**
     * Counts subarrays with sum divisible by K.
     * 
     * @param nums Array of integers
     * @param k Divisor
     * @return Count of subarrays divisible by K
     */
    public static int subarraysDivByK(int[] nums, int k) {
        if (nums == null || nums.length == 0) {
            return 0;
        }
        
        java.util.HashMap<Integer, Integer> remainderCount = new java.util.HashMap<>();
        remainderCount.put(0, 1); // Initialize with 0 remainder
        
        int count = 0;
        int prefixSum = 0;
        
        for (int num : nums) {
            prefixSum += num;
            
            // Calculate remainder (handle negative numbers properly)
            int remainder = prefixSum % k;
            if (remainder < 0) {
                remainder += k;
            }
            
            // If this remainder was seen before, add those counts
            count += remainderCount.getOrDefault(remainder, 0);
            
            // Update remainder count
            remainderCount.put(remainder, remainderCount.getOrDefault(remainder, 0) + 1);
        }
        
        return count;
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Subarrays Divisible by K ===\n");
        
        // Test case 1
        int[] arr1 = {4, 5, 0, -2, -3, 1};
        int k1 = 5;
        System.out.println("Test 1: [4,5,0,-2,-3,1], k = " + k1);
        System.out.println("Count: " + subarraysDivByK(arr1, k1) + "\n");
        
        // Test case 2
        int[] arr2 = {5};
        int k2 = 9;
        System.out.println("Test 2: [5], k = " + k2);
        System.out.println("Count: " + subarraysDivByK(arr2, k2) + "\n");
    }
}
