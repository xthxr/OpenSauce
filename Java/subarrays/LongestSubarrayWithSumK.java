/**
 * Longest Subarray with Sum K
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(n)
 * 
 * Find the length of the longest subarray with sum equal to K.
 * Uses prefix sum and HashMap technique.
 */
public class LongestSubarrayWithSumK {
    
    /**
     * Finds the length of the longest subarray with sum K (for positive numbers).
     * 
     * @param arr Array of positive integers
     * @param k Target sum
     * @return Length of longest subarray with sum K
     */
    public static int longestSubarrayPositive(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        int maxLength = 0;
        int currentSum = 0;
        int start = 0;
        
        for (int end = 0; end < arr.length; end++) {
            currentSum += arr[end];
            
            // Shrink window if sum exceeds k
            while (currentSum > k && start <= end) {
                currentSum -= arr[start];
                start++;
            }
            
            // Update max length if we found sum k
            if (currentSum == k) {
                maxLength = Math.max(maxLength, end - start + 1);
            }
        }
        
        return maxLength;
    }
    
    /**
     * Finds the length of the longest subarray with sum K (works with negative numbers).
     * 
     * @param arr Array of integers
     * @param k Target sum
     * @return Length of longest subarray with sum K
     */
    public static int longestSubarrayWithNegatives(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return 0;
        }
        
        java.util.HashMap<Integer, Integer> sumMap = new java.util.HashMap<>();
        int maxLength = 0;
        int currentSum = 0;
        
        for (int i = 0; i < arr.length; i++) {
            currentSum += arr[i];
            
            // If current sum equals k, entire subarray from 0 to i has sum k
            if (currentSum == k) {
                maxLength = i + 1;
            }
            
            // Check if (currentSum - k) exists in map
            if (sumMap.containsKey(currentSum - k)) {
                maxLength = Math.max(maxLength, i - sumMap.get(currentSum - k));
            }
            
            // Store current sum with its first occurrence index
            if (!sumMap.containsKey(currentSum)) {
                sumMap.put(currentSum, i);
            }
        }
        
        return maxLength;
    }
    
    /**
     * Finds the longest subarray with sum K and returns start and end indices.
     * 
     * @param arr Array of integers
     * @param k Target sum
     * @return Array containing [start_index, end_index, length] or [-1, -1, 0] if not found
     */
    public static int[] longestSubarrayWithIndices(int[] arr, int k) {
        if (arr == null || arr.length == 0) {
            return new int[]{-1, -1, 0};
        }
        
        java.util.HashMap<Integer, Integer> sumMap = new java.util.HashMap<>();
        int maxLength = 0;
        int currentSum = 0;
        int startIndex = -1, endIndex = -1;
        
        for (int i = 0; i < arr.length; i++) {
            currentSum += arr[i];
            
            if (currentSum == k) {
                if (i + 1 > maxLength) {
                    maxLength = i + 1;
                    startIndex = 0;
                    endIndex = i;
                }
            }
            
            if (sumMap.containsKey(currentSum - k)) {
                int length = i - sumMap.get(currentSum - k);
                if (length > maxLength) {
                    maxLength = length;
                    startIndex = sumMap.get(currentSum - k) + 1;
                    endIndex = i;
                }
            }
            
            if (!sumMap.containsKey(currentSum)) {
                sumMap.put(currentSum, i);
            }
        }
        
        return new int[]{startIndex, endIndex, maxLength};
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Longest Subarray with Sum K ===\n");
        
        // Test case 1: Positive numbers
        int[] arr1 = {10, 5, 2, 7, 1, 9};
        int k1 = 15;
        System.out.println("Test 1 (Positive): [10, 5, 2, 7, 1, 9], K = " + k1);
        System.out.println("Longest length: " + longestSubarrayPositive(arr1, k1));
        int[] result1 = longestSubarrayWithIndices(arr1, k1);
        if (result1[0] != -1) {
            System.out.print("Subarray: [");
            for (int i = result1[0]; i <= result1[1]; i++) {
                System.out.print(arr1[i] + (i < result1[1] ? ", " : ""));
            }
            System.out.println("]\n");
        }
        
        // Test case 2: With negative numbers
        int[] arr2 = {-5, 8, -14, 2, 4, 12};
        int k2 = -5;
        System.out.println("Test 2 (With negatives): [-5, 8, -14, 2, 4, 12], K = " + k2);
        System.out.println("Longest length: " + longestSubarrayWithNegatives(arr2, k2));
        int[] result2 = longestSubarrayWithIndices(arr2, k2);
        if (result2[0] != -1) {
            System.out.print("Subarray: [");
            for (int i = result2[0]; i <= result2[1]; i++) {
                System.out.print(arr2[i] + (i < result2[1] ? ", " : ""));
            }
            System.out.println("]\n");
        }
        
        // Test case 3: Multiple subarrays with same sum
        int[] arr3 = {1, 2, 3, 1, 1, 1, 1, 4, 2, 3};
        int k3 = 3;
        System.out.println("Test 3 (Multiple subarrays): [1, 2, 3, 1, 1, 1, 1, 4, 2, 3], K = " + k3);
        System.out.println("Longest length: " + longestSubarrayWithNegatives(arr3, k3));
        int[] result3 = longestSubarrayWithIndices(arr3, k3);
        if (result3[0] != -1) {
            System.out.print("Subarray: [");
            for (int i = result3[0]; i <= result3[1]; i++) {
                System.out.print(arr3[i] + (i < result3[1] ? ", " : ""));
            }
            System.out.println("]\n");
        }
        
        // Test case 4: No subarray with sum K
        int[] arr4 = {1, 2, 3, 4};
        int k4 = 20;
        System.out.println("Test 4 (Not found): [1, 2, 3, 4], K = " + k4);
        System.out.println("Longest length: " + longestSubarrayWithNegatives(arr4, k4) + "\n");
    }
}
