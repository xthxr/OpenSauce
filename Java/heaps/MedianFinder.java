import java.util.*;

/**
 * Problem: Find Median from Data Stream
 *
 * LeetCode: 295
 *
 * Approach:
 * - Use two heaps:
 *   -> Max-Heap for left half (smaller numbers)
 *   -> Min-Heap for right half (larger numbers)
 * - Balance heaps so size difference is at most 1
 * - Median:
 *   -> If equal size: avg of two heap tops
 *   -> Else: top of larger heap
 *
 * Time Complexity: O(log n) per insertion
 * Space Complexity: O(n)
 */
public class MedianFinder {

    private PriorityQueue<Integer> left;  // max-heap
    private PriorityQueue<Integer> right; // min-heap

    public MedianFinder() {
        left = new PriorityQueue<>(Collections.reverseOrder());
        right = new PriorityQueue<>();
    }

    public void addNum(int num) {
        // Step 1: Add to max-heap
        left.add(num);

        // Step 2: Balance (left top <= right top)
        right.add(left.poll());
        if (right.size() > left.size()) {
            left.add(right.poll());
        }
    }

    public double findMedian() {
        if (left.size() == right.size()) {
            return (left.peek() + right.peek()) / 2.0;
        }
        return left.peek();
    }

    public static void main(String[] args) {
        System.out.println("=== Median Finder (LC 295) ===");
        MedianFinder mf = new MedianFinder();

        mf.addNum(1);
        mf.addNum(2);
        System.out.println("Median after [1,2] = " + mf.findMedian()); // 1.5

        mf.addNum(3);
        System.out.println("Median after [1,2,3] = " + mf.findMedian()); // 2

        mf.addNum(4);
        System.out.println("Median after [1,2,3,4] = " + mf.findMedian()); // 2.5
    }
}
