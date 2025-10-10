import java.util.*;

/**
 * Problem: Find the Kth Largest Element in an Array
 * 
 * LeetCode: 215
 * 
 * Approach:
 * - Use a Min-Heap (PriorityQueue in Java).
 * - Keep pushing elements into the heap.
 * - If heap size exceeds k, remove the smallest element.
 * - After processing all elements, the root of the heap will be the kth largest.
 * 
 * Time Complexity: O(n log k) 
 *   -> inserting/removing in heap takes log k, and we do it for n elements
 * Space Complexity: O(k) 
 *   -> heap stores at most k elements
 */
public class KthLargestElement {

    /**
     * Finds the kth largest element in the array.
     *
     * @param nums input array
     * @param k    the "kth largest" to find
     * @return kth largest element
     */
    public static int findKthLargest(int[] nums, int k) {
        // Step 1: Create a min-heap
        PriorityQueue<Integer> minHeap = new PriorityQueue<>();

        // Step 2: Iterate through all numbers
        for (int num : nums) {
            minHeap.add(num); // push number to heap

            // Step 3: If size exceeds k, remove smallest
            if (minHeap.size() > k) {
                minHeap.poll();
            }
        }

        // Step 4: The root of the heap = kth largest element
        return minHeap.peek();
    }

    /**
     * Main method with test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Kth Largest Element in Array (LC 215) ===");

        // Example 1
        int[] nums1 = {3, 2, 1, 5, 6, 4};
        int k1 = 2;
        System.out.println("Array: " + Arrays.toString(nums1));
        System.out.println(k1 + "th largest element = " + findKthLargest(nums1, k1));

        // Example 2
        int[] nums2 = {7, 10, 4, 3, 20, 15};
        int k2 = 3;
        System.out.println("\nArray: " + Arrays.toString(nums2));
        System.out.println(k2 + "rd largest element = " + findKthLargest(nums2, k2));

        // Example 3 (edge case: k=1 -> largest element)
        int[] nums3 = {2, 1};
        int k3 = 1;
        System.out.println("\nArray: " + Arrays.toString(nums3));
        System.out.println(k3 + "st largest element = " + findKthLargest(nums3, k3));
    }
}
