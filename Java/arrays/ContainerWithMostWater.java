/**
 * Problem: Container With Most Water
 * LeetCode: https://leetcode.com/problems/container-with-most-water/
 *
 * Approach:
 *  - Use two-pointer technique.
 *  - Start with pointers at both ends of the array.
 *  - Calculate area formed between the lines at both pointers.
 *  - Move the pointer with the smaller height inward to try for a bigger area.
 *
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */

public class Solution {

    public int maxArea(int[] height) {
        int n = height.length;
        int left = 0;
        int right = n - 1;
        int maxArea = 0;

        while (left < right) {
            int width = right - left;
            int minHeight = Math.min(height[left], height[right]);
            int area = width * minHeight;
            maxArea = Math.max(maxArea, area);

            // Move the smaller height pointer inward
            if (height[left] < height[right]) {
                left++;
            } else {
                right--;
            }
        }

        return maxArea;
    }
}
