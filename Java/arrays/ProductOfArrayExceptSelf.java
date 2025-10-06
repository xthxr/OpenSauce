/**
 * Problem: Product of Array Except Self
 * LeetCode: https://leetcode.com/problems/product-of-array-except-self/
 *
 * Approach:
 *  - Use prefix and suffix products.
 *  - First pass: store the prefix products (product of all numbers before current index).
 *  - Second pass (reverse): multiply by suffix products (product of all numbers after current index).
 *  - This avoids using division and ensures O(n) time complexity.
 *
 * Time Complexity: O(n)
 * Space Complexity: O(1) (excluding output array)
 */

public class ProductOfArrayExceptSelf {
    public int[] productExceptSelf(int[] nums) {
        int n = nums.length;
        int[] result = new int[n];

        // Step 1: Compute prefix products
        result[0] = 1;
        for (int i = 1; i < n; i++) {
            result[i] = result[i - 1] * nums[i - 1];
        }

        // Step 2: Compute suffix products and multiply
        int suffix = 1;
        for (int i = n - 1; i >= 0; i--) {
            result[i] *= suffix;
            suffix *= nums[i];
        }

        return result;
    }
}
