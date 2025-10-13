/**
 * Subarray with Given Sum
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 * 
 * Find a continuous subarray which adds up to a given sum.
 * This problem uses the sliding window technique for positive numbers.
 */
public class SubarrayWithGivenSum {
    
    /**
     * Finds a subarray with the given sum (works for positive numbers).
     * 
     * @param arr Array of positive integers
     * @param targetSum Target sum to find
     * @return Array containing [start_index, end_index] or [-1, -1] if not found
     */
    public static int[] findSubarrayWithSum(int[] arr, int targetSum) {
        if (arr == null || arr.length == 0) {
            return new int[]{-1, -1};
        }
        
        int currentSum = 0;
        int start = 0;
        
        for (int end = 0; end < arr.length; end++) {
            currentSum += arr[end];
            
            // Shrink window from left if sum exceeds target
            while (currentSum > targetSum && start <= end) {
                currentSum -= arr[start];
                start++;
            }
            
            // Check if we found the target sum
            if (currentSum == targetSum) {
                return new int[]{start, end};
            }
        }
        
        return new int[]{-1, -1}; // Not found
    }
    
    /**
     * Finds a subarray with the given sum (works for both positive and negative numbers).
     * Uses HashMap to store cumulative sums.
     * 
     * @param arr Array of integers
     * @param targetSum Target sum to find
     * @return Array containing [start_index, end_index] or [-1, -1] if not found
     */
    public static int[] findSubarrayWithSumHashMap(int[] arr, int targetSum) {
        if (arr == null || arr.length == 0) {
            return new int[]{-1, -1};
        }
        
        java.util.HashMap<Integer, Integer> sumMap = new java.util.HashMap<>();
        int currentSum = 0;
        
        for (int i = 0; i < arr.length; i++) {
            currentSum += arr[i];
            
            // Check if current sum equals target
            if (currentSum == targetSum) {
                return new int[]{0, i};
            }
            
            // Check if (currentSum - targetSum) exists in map
            if (sumMap.containsKey(currentSum - targetSum)) {
                return new int[]{sumMap.get(currentSum - targetSum) + 1, i};
            }
            
            // Store current sum with its index
            sumMap.put(currentSum, i);
        }
        
        return new int[]{-1, -1}; // Not found
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Subarray with Given Sum ===\n");
        
        // Test case 1: Positive numbers with sliding window
        int[] arr1 = {1, 2, 3, 7, 5};
        int sum1 = 12;
        System.out.println("Test 1 (Positive numbers): [1, 2, 3, 7, 5], Target Sum: " + sum1);
        int[] result1 = findSubarrayWithSum(arr1, sum1);
        if (result1[0] != -1) {
            System.out.println("Subarray found at indices [" + result1[0] + ", " + result1[1] + "]");
            System.out.print("Subarray: [");
            for (int i = result1[0]; i <= result1[1]; i++) {
                System.out.print(arr1[i] + (i < result1[1] ? ", " : ""));
            }
            System.out.println("]\n");
        } else {
            System.out.println("No subarray found\n");
        }
        
        // Test case 2: With negative numbers using HashMap
        int[] arr2 = {10, 2, -2, -20, 10};
        int sum2 = -10;
        System.out.println("Test 2 (With negatives): [10, 2, -2, -20, 10], Target Sum: " + sum2);
        int[] result2 = findSubarrayWithSumHashMap(arr2, sum2);
        if (result2[0] != -1) {
            System.out.println("Subarray found at indices [" + result2[0] + ", " + result2[1] + "]");
            System.out.print("Subarray: [");
            for (int i = result2[0]; i <= result2[1]; i++) {
                System.out.print(arr2[i] + (i < result2[1] ? ", " : ""));
            }
            System.out.println("]\n");
        } else {
            System.out.println("No subarray found\n");
        }
        
        // Test case 3: No subarray with given sum
        int[] arr3 = {1, 2, 3, 4};
        int sum3 = 15;
        System.out.println("Test 3 (Not found): [1, 2, 3, 4], Target Sum: " + sum3);
        int[] result3 = findSubarrayWithSum(arr3, sum3);
        if (result3[0] != -1) {
            System.out.println("Subarray found at indices [" + result3[0] + ", " + result3[1] + "]\n");
        } else {
            System.out.println("No subarray found\n");
        }
        
        // Test case 4: Entire array is the answer
        int[] arr4 = {5, 10, 15, 20};
        int sum4 = 50;
        System.out.println("Test 4 (Entire array): [5, 10, 15, 20], Target Sum: " + sum4);
        int[] result4 = findSubarrayWithSum(arr4, sum4);
        if (result4[0] != -1) {
            System.out.println("Subarray found at indices [" + result4[0] + ", " + result4[1] + "]\n");
        } else {
            System.out.println("No subarray found\n");
        }
    }
}
