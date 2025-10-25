/**
 * BucketSort Algorithm
 *
 * Time Complexity:
 *   - Best: O(n + k)
 *   - Average: O(n + k)
 *   - Worst: O(n^2) (if all elements go into one bucket)
 *
 * Space Complexity: O(n + k)
 *
 * This algorithm distributes elements into buckets, sorts each bucket,
 * and concatenates them to get the sorted array.
 */

import java.util.ArrayList;
import java.util.Collections;

public class BucketSort {

    /**
     * Sorts an array of integers using Bucket Sort.
     *
     * @param arr Array of integers to sort
     */
    public static void bucketSort(int[] arr) {
        if (arr.length <= 1) return;

        // 1. Find max and min values
        int max = arr[0];
        int min = arr[0];
        for (int num : arr) {
            if (num > max) max = num;
            if (num < min) min = num;
        }

        int bucketCount = (max - min) / arr.length + 1;

        // 2. Create buckets
        ArrayList<ArrayList<Integer>> buckets = new ArrayList<>(bucketCount);
        for (int i = 0; i < bucketCount; i++) {
            buckets.add(new ArrayList<>());
        }

        // 3. Distribute input array values into buckets
        for (int num : arr) {
            int idx = (num - min) / arr.length;
            buckets.get(idx).add(num);
        }

        // 4. Sort individual buckets and merge
        int index = 0;
        for (ArrayList<Integer> bucket : buckets) {
            Collections.sort(bucket); // You can replace with another sort if needed
            for (int num : bucket) {
                arr[index++] = num;
            }
        }
    }

    // Example usage
    public static void main(String[] args) {
        int[] arr = {42, 32, 33, 52, 37, 47, 51};
        System.out.println("Original Array:");
        for (int num : arr) System.out.print(num + " ");
        System.out.println();

        bucketSort(arr);

        System.out.println("Sorted Array:");
        for (int num : arr) System.out.print(num + " ");
    }
}
