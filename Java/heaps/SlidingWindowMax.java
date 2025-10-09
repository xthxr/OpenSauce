import java.util.*;

/**
 * Problem: Sliding Window Maximum
 *
 * LeetCode: 239
 *
 * Approach:
 * - Use a Deque to store indices of useful elements in window
 * - Always keep decreasing order in deque
 * - Front of deque = max element index for current window
 *
 * Time Complexity: O(n) 
 *   -> each element added/removed once
 * Space Complexity: O(k)
 */
public class SlidingWindowMaximum {

    public static int[] maxSlidingWindow(int[] nums, int k) {
        if (nums.length == 0 || k == 0) return new int[0];

        int n = nums.length;
        int[] result = new int[n - k + 1];
        Deque<Integer> dq = new LinkedList<>();

        for (int i = 0; i < n; i++) {
            // Step 1: Remove indices outside window
            while (!dq.isEmpty() && dq.peekFirst() <= i - k) {
                dq.pollFirst();
            }

            // Step 2: Maintain decreasing order
            while (!dq.isEmpty() && nums[dq.peekLast()] < nums[i]) {
                dq.pollLast();
            }

            dq.offerLast(i);

            // Step 3: Record max (starting from i >= k-1)
            if (i >= k - 1) {
                result[i - k + 1] = nums[dq.peekFirst()];
            }
        }
        return result;
    }

    public static void main(String[] args) {
        System.out.println("=== Sliding Window Maximum (LC 239) ===");

        int[] nums1 = {1,3,-1,-3,5,3,6,7};
        int k1 = 3;
        System.out.println("Input: " + Arrays.toString(nums1) + ", k=" + k1);
        System.out.println("Output: " + Arrays.toString(maxSlidingWindow(nums1, k1)));

        int[] nums2 = {9,10,9,-7,-4,-8,2,-6};
        int k2 = 5;
        System.out.println("\nInput: " + Arrays.toString(nums2) + ", k=" + k2);
        System.out.println("Output: " + Arrays.toString(maxSlidingWindow(nums2, k2)));
    }
}
