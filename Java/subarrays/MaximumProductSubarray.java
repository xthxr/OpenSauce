/**
 * Maximum Product Subarray
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 * 
 * Find the contiguous subarray with the maximum product.
 * This problem requires tracking both maximum and minimum products
 * because a negative number can turn a minimum into a maximum.
 * 
 * LeetCode Problem #152: https://leetcode.com/problems/maximum-product-subarray/
 */
public class MaximumProductSubarray {
    
    /**
     * Finds the maximum product of any contiguous subarray.
     * 
     * @param nums Array of integers
     * @return Maximum product of contiguous subarray
     */
    public static int maxProduct(int[] nums) {
        if (nums == null || nums.length == 0) {
            throw new IllegalArgumentException("Array cannot be null or empty");
        }
        
        int maxProduct = nums[0];
        int currentMax = nums[0];
        int currentMin = nums[0];
        
        for (int i = 1; i < nums.length; i++) {
            // When multiplied by negative number, max becomes min and vice versa
            if (nums[i] < 0) {
                int temp = currentMax;
                currentMax = currentMin;
                currentMin = temp;
            }
            
            // Calculate max and min products ending at current position
            currentMax = Math.max(nums[i], currentMax * nums[i]);
            currentMin = Math.min(nums[i], currentMin * nums[i]);
            
            // Update global maximum
            maxProduct = Math.max(maxProduct, currentMax);
        }
        
        return maxProduct;
    }
    
    /**
     * Alternative approach: Check products from both left and right.
     * 
     * @param nums Array of integers
     * @return Maximum product of contiguous subarray
     */
    public static int maxProductAlternative(int[] nums) {
        if (nums == null || nums.length == 0) {
            throw new IllegalArgumentException("Array cannot be null or empty");
        }
        
        int maxProduct = Integer.MIN_VALUE;
        int leftProduct = 1;
        int rightProduct = 1;
        int n = nums.length;
        
        for (int i = 0; i < n; i++) {
            // Reset to 1 if product becomes 0
            leftProduct = (leftProduct == 0) ? 1 : leftProduct;
            rightProduct = (rightProduct == 0) ? 1 : rightProduct;
            
            leftProduct *= nums[i];
            rightProduct *= nums[n - 1 - i];
            
            maxProduct = Math.max(maxProduct, Math.max(leftProduct, rightProduct));
        }
        
        return maxProduct;
    }
    
    /**
     * Finds the maximum product subarray and returns the subarray.
     * 
     * @param nums Array of integers
     * @return Array containing [start_index, end_index, max_product]
     */
    public static long[] maxProductWithIndices(int[] nums) {
        if (nums == null || nums.length == 0) {
            throw new IllegalArgumentException("Array cannot be null or empty");
        }
        
        long maxProduct = nums[0];
        long currentMax = nums[0];
        long currentMin = nums[0];
        int start = 0, end = 0;
        int tempStart = 0;
        
        for (int i = 1; i < nums.length; i++) {
            if (nums[i] < 0) {
                long temp = currentMax;
                currentMax = currentMin;
                currentMin = temp;
            }
            
            if (nums[i] > currentMax * nums[i]) {
                currentMax = nums[i];
                tempStart = i;
            } else {
                currentMax = currentMax * nums[i];
            }
            
            currentMin = Math.min(nums[i], currentMin * nums[i]);
            
            if (currentMax > maxProduct) {
                maxProduct = currentMax;
                start = tempStart;
                end = i;
            }
        }
        
        return new long[]{start, end, maxProduct};
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Maximum Product Subarray ===\n");
        
        // Test case 1: Mixed positive and negative
        int[] arr1 = {2, 3, -2, 4};
        System.out.println("Test 1: [2, 3, -2, 4]");
        System.out.println("Maximum Product: " + maxProduct(arr1));
        long[] result1 = maxProductWithIndices(arr1);
        System.out.print("Subarray: [");
        for (int i = (int)result1[0]; i <= result1[1]; i++) {
            System.out.print(arr1[i] + (i < result1[1] ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Test case 2: With zeros
        int[] arr2 = {-2, 0, -1};
        System.out.println("Test 2: [-2, 0, -1]");
        System.out.println("Maximum Product: " + maxProduct(arr2) + "\n");
        
        // Test case 3: All negative numbers (even count)
        int[] arr3 = {-2, -3, -4};
        System.out.println("Test 3: [-2, -3, -4]");
        System.out.println("Maximum Product: " + maxProduct(arr3));
        long[] result3 = maxProductWithIndices(arr3);
        System.out.print("Subarray: [");
        for (int i = (int)result3[0]; i <= result3[1]; i++) {
            System.out.print(arr3[i] + (i < result3[1] ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Test case 4: Single negative number
        int[] arr4 = {-2};
        System.out.println("Test 4: [-2]");
        System.out.println("Maximum Product: " + maxProduct(arr4) + "\n");
        
        // Test case 5: Large products
        int[] arr5 = {2, 3, -2, 4, -1};
        System.out.println("Test 5: [2, 3, -2, 4, -1]");
        System.out.println("Maximum Product (Method 1): " + maxProduct(arr5));
        System.out.println("Maximum Product (Method 2): " + maxProductAlternative(arr5) + "\n");
        
        // Test case 6: All positive numbers
        int[] arr6 = {1, 2, 3, 4};
        System.out.println("Test 6: [1, 2, 3, 4]");
        System.out.println("Maximum Product: " + maxProduct(arr6) + "\n");
    }
}
