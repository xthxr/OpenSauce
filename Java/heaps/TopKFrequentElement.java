import java.util.*;

/**
 * Problem: Top K Frequent Elements
 *
 * LeetCode: 347
 *
 * Approach:
 * - Count frequency of each element using HashMap
 * - Use a Min-Heap of size k to keep the top k frequent elements
 * - If heap size exceeds k, remove least frequent
 * - Extract elements from heap
 *
 * Time Complexity: O(n log k) 
 *   -> n for building frequency map, log k for heap operations
 * Space Complexity: O(n + k) 
 *   -> frequency map + heap
 */
public class TopKFrequentElements {

    public static int[] topKFrequent(int[] nums, int k) {
        // Step 1: Count frequency of each element
        Map<Integer, Integer> freqMap = new HashMap<>();
        for (int num : nums) {
            freqMap.put(num, freqMap.getOrDefault(num, 0) + 1);
        }

        // Step 2: Min-Heap to store top k frequent elements
        PriorityQueue<Map.Entry<Integer, Integer>> minHeap =
            new PriorityQueue<>(Comparator.comparingInt(Map.Entry::getValue));

        // Step 3: Add entries to heap
        for (Map.Entry<Integer, Integer> entry : freqMap.entrySet()) {
            minHeap.add(entry);
            if (minHeap.size() > k) {
                minHeap.poll();
            }
        }

        // Step 4: Extract result
        int[] result = new int[k];
        int i = 0;
        while (!minHeap.isEmpty()) {
            result[i++] = minHeap.poll().getKey();
        }
        return result;
    }

    public static void main(String[] args) {
        System.out.println("=== Top K Frequent Elements (LC 347) ===");

        int[] nums1 = {1,1,1,2,2,3};
        int k1 = 2;
        System.out.println("Input: " + Arrays.toString(nums1) + ", k=" + k1);
        System.out.println("Top K Frequent: " + Arrays.toString(topKFrequent(nums1, k1)));

        int[] nums2 = {4,1,-1,2,-1,2,3};
        int k2 = 2;
        System.out.println("\nInput: " + Arrays.toString(nums2) + ", k=" + k2);
        System.out.println("Top K Frequent: " + Arrays.toString(topKFrequent(nums2, k2)));
    }
}
