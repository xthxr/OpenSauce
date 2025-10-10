import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Sliding Window Maximum
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(k) where k is window size
 * 
 * Find the maximum element in each sliding window of size k.
 * Uses a deque to maintain elements in decreasing order.
 * 
 * LeetCode Problem #239: https://leetcode.com/problems/sliding-window-maximum/
 */
public class SlidingWindowMaximum {
    
    /**
     * Finds the maximum in each sliding window of size k using deque.
     * 
     * @param nums Array of integers
     * @param k Window size
     * @return Array of maximum values for each window
     */
    public static int[] maxSlidingWindow(int[] nums, int k) {
        if (nums == null || nums.length == 0 || k <= 0) {
            return new int[0];
        }
        
        if (k == 1) {
            return nums;
        }
        
        int n = nums.length;
        int[] result = new int[n - k + 1];
        Deque<Integer> deque = new ArrayDeque<>(); // Stores indices
        
        for (int i = 0; i < n; i++) {
            // Remove indices that are out of current window
            while (!deque.isEmpty() && deque.peekFirst() < i - k + 1) {
                deque.pollFirst();
            }
            
            // Remove indices whose values are smaller than current element
            while (!deque.isEmpty() && nums[deque.peekLast()] < nums[i]) {
                deque.pollLast();
            }
            
            deque.offerLast(i);
            
            // Add to result once we have a complete window
            if (i >= k - 1) {
                result[i - k + 1] = nums[deque.peekFirst()];
            }
        }
        
        return result;
    }
    
    /**
     * Brute force approach for verification (O(n*k)).
     * 
     * @param nums Array of integers
     * @param k Window size
     * @return Array of maximum values for each window
     */
    public static int[] maxSlidingWindowBruteForce(int[] nums, int k) {
        if (nums == null || nums.length == 0 || k <= 0) {
            return new int[0];
        }
        
        int n = nums.length;
        int[] result = new int[n - k + 1];
        
        for (int i = 0; i <= n - k; i++) {
            int max = nums[i];
            for (int j = i; j < i + k; j++) {
                max = Math.max(max, nums[j]);
            }
            result[i] = max;
        }
        
        return result;
    }
    
    /**
     * Finds minimum in each sliding window (similar approach).
     * 
     * @param nums Array of integers
     * @param k Window size
     * @return Array of minimum values for each window
     */
    public static int[] minSlidingWindow(int[] nums, int k) {
        if (nums == null || nums.length == 0 || k <= 0) {
            return new int[0];
        }
        
        int n = nums.length;
        int[] result = new int[n - k + 1];
        Deque<Integer> deque = new ArrayDeque<>();
        
        for (int i = 0; i < n; i++) {
            while (!deque.isEmpty() && deque.peekFirst() < i - k + 1) {
                deque.pollFirst();
            }
            
            // For minimum, remove elements greater than current
            while (!deque.isEmpty() && nums[deque.peekLast()] > nums[i]) {
                deque.pollLast();
            }
            
            deque.offerLast(i);
            
            if (i >= k - 1) {
                result[i - k + 1] = nums[deque.peekFirst()];
            }
        }
        
        return result;
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Sliding Window Maximum ===\n");
        
        // Test case 1: Basic example
        int[] arr1 = {1, 3, -1, -3, 5, 3, 6, 7};
        int k1 = 3;
        System.out.println("Test 1: [1, 3, -1, -3, 5, 3, 6, 7], k = " + k1);
        int[] result1 = maxSlidingWindow(arr1, k1);
        System.out.print("Maximum in each window: [");
        for (int i = 0; i < result1.length; i++) {
            System.out.print(result1[i] + (i < result1.length - 1 ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Test case 2: Window size = 1
        int[] arr2 = {1, -1, 3, 2};
        int k2 = 1;
        System.out.println("Test 2: [1, -1, 3, 2], k = " + k2);
        int[] result2 = maxSlidingWindow(arr2, k2);
        System.out.print("Maximum in each window: [");
        for (int i = 0; i < result2.length; i++) {
            System.out.print(result2[i] + (i < result2.length - 1 ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Test case 3: Decreasing array
        int[] arr3 = {9, 8, 7, 6, 5, 4, 3, 2, 1};
        int k3 = 3;
        System.out.println("Test 3: [9, 8, 7, 6, 5, 4, 3, 2, 1], k = " + k3);
        int[] result3 = maxSlidingWindow(arr3, k3);
        System.out.print("Maximum in each window: [");
        for (int i = 0; i < result3.length; i++) {
            System.out.print(result3[i] + (i < result3.length - 1 ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Test case 4: Increasing array
        int[] arr4 = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        int k4 = 3;
        System.out.println("Test 4: [1, 2, 3, 4, 5, 6, 7, 8, 9], k = " + k4);
        int[] result4 = maxSlidingWindow(arr4, k4);
        System.out.print("Maximum in each window: [");
        for (int i = 0; i < result4.length; i++) {
            System.out.print(result4[i] + (i < result4.length - 1 ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Test case 5: Minimum in sliding window
        int[] arr5 = {1, 3, -1, -3, 5, 3, 6, 7};
        int k5 = 3;
        System.out.println("Test 5 (Minimum): [1, 3, -1, -3, 5, 3, 6, 7], k = " + k5);
        int[] result5 = minSlidingWindow(arr5, k5);
        System.out.print("Minimum in each window: [");
        for (int i = 0; i < result5.length; i++) {
            System.out.print(result5[i] + (i < result5.length - 1 ? ", " : ""));
        }
        System.out.println("]\n");
        
        // Comparison with brute force
        System.out.println("Verification with brute force:");
        int[] bruteResult = maxSlidingWindowBruteForce(arr1, k1);
        System.out.print("Brute force result: [");
        for (int i = 0; i < bruteResult.length; i++) {
            System.out.print(bruteResult[i] + (i < bruteResult.length - 1 ? ", " : ""));
        }
        System.out.println("]");
    }
}
