/**
 * Count Subarrays with Sum K
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(n)
 * 
 * Count the number of subarrays with sum equal to K.
 * Uses prefix sum and HashMap technique.
 * 
 * LeetCode Problem #560: https://leetcode.com/problems/subarray-sum-equals-k/
 */
public class CountSubarraysWithSumK {
    
    /**
     * Counts the number of subarrays with sum equal to K.
     * 
     * @param arr Array of integers
     * @param k Target sum
     * @return Count of subarrays with sum K
     */
    public static int countSubarrays(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        java.util.HashMap<Integer, Integer> sumFrequency = new java.util.HashMap<>();
        int count = 0;
        int currentSum = 0;
        
        // Initialize with 0 sum having frequency 1
        sumFrequency.put(0, 1);
        
        for (int i = 0; i < arr.length; i++) {
            currentSum += arr[i];
            
            // If (currentSum - k) exists, it means there are subarrays ending at i with sum k
            if (sumFrequency.containsKey(currentSum - k)) {
                count += sumFrequency.get(currentSum - k);
            }
            
            // Update frequency of current sum
            sumFrequency.put(currentSum, sumFrequency.getOrDefault(currentSum, 0) + 1);
        }
        
        return count;
    }
    
    /**
     * Counts subarrays and prints all of them.
     * 
     * @param arr Array of integers
     * @param k Target sum
     * @return Count of subarrays with sum K
     */
    public static int countAndPrintSubarrays(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        java.util.HashMap<Integer, java.util.ArrayList<Integer>> sumIndices = new java.util.HashMap<>();
        int count = 0;
        int currentSum = 0;
        
        sumIndices.put(0, new java.util.ArrayList<>());
        sumIndices.get(0).add(-1);
        
        System.out.println("Subarrays with sum " + k + ":");
        
        for (int i = 0; i < arr.length; i++) {
            currentSum += arr[i];
            
            if (sumIndices.containsKey(currentSum - k)) {
                for (int startIdx : sumIndices.get(currentSum - k)) {
                    count++;
                    System.out.print("  [");
                    for (int j = startIdx + 1; j <= i; j++) {
                        System.out.print(arr[j] + (j < i ? ", " : ""));
                    }
                    System.out.println("] (indices " + (startIdx + 1) + " to " + i + ")");
                }
            }
            
            if (!sumIndices.containsKey(currentSum)) {
                sumIndices.put(currentSum, new java.util.ArrayList<>());
            }
            sumIndices.get(currentSum).add(i);
        }
        
        return count;
    }
    
    /**
     * Brute force approach for verification (O(n^2)).
     * 
     * @param arr Array of integers
     * @param k Target sum
     * @return Count of subarrays with sum K
     */
    public static int countSubarraysBruteForce(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        int count = 0;
        
        for (int i = 0; i < arr.length; i++) {
            int sum = 0;
            for (int j = i; j < arr.length; j++) {
                sum += arr[j];
                if (sum == k) {
                    count++;
                }
            }
        }
        
        return count;
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Count Subarrays with Sum K ===\n");
        
        // Test case 1: Basic example
        int[] arr1 = {1, 1, 1};
        int k1 = 2;
        System.out.println("Test 1: [1, 1, 1], K = " + k1);
        System.out.println("Count (Optimized): " + countSubarrays(arr1, k1));
        System.out.println("Count (Brute Force): " + countSubarraysBruteForce(arr1, k1) + "\n");
        
        // Test case 2: With negative numbers
        int[] arr2 = {1, -1, 0};
        int k2 = 0;
        System.out.println("Test 2: [1, -1, 0], K = " + k2);
        countAndPrintSubarrays(arr2, k2);
        System.out.println();
        
        // Test case 3: Mixed positive and negative
        int[] arr3 = {3, 4, 7, 2, -3, 1, 4, 2};
        int k3 = 7;
        System.out.println("Test 3: [3, 4, 7, 2, -3, 1, 4, 2], K = " + k3);
        System.out.println("Count: " + countSubarrays(arr3, k3));
        countAndPrintSubarrays(arr3, k3);
        System.out.println();
        
        // Test case 4: No subarray with sum K
        int[] arr4 = {1, 2, 3};
        int k4 = 10;
        System.out.println("Test 4: [1, 2, 3], K = " + k4);
        System.out.println("Count: " + countSubarrays(arr4, k4) + "\n");
        
        // Test case 5: Large numbers
        int[] arr5 = {10, 2, -2, -20, 10};
        int k5 = -10;
        System.out.println("Test 5: [10, 2, -2, -20, 10], K = " + k5);
        System.out.println("Count: " + countSubarrays(arr5, k5));
        countAndPrintSubarrays(arr5, k5);
    }
}
