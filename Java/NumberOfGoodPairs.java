/*
 * Problem: LeetCode 1512 - Number of Good Pairs
 * Category: Arrays
 * Difficulty: Medium
 * 
 * Approach:
 *  - Use a HashMap to count frequency of each number.
 *  - For each frequency n, add n*(n-1)/2 pairs to the result.
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(n)
 * 
 * Example:
 * Input: [1, 2, 3, 1, 1, 3]
 * Output: 4
 */

import java.util.HashMap;
import java.util.Map;

public class NumberOfGoodPairs {
    public int numIdenticalPairs(int[] nums) {
        HashMap<Integer, Integer> map = new HashMap<>();
        int sum = 0;

        for (int num : nums) {
            map.put(num, map.getOrDefault(num, 0) + 1);
        }

        for (Map.Entry<Integer, Integer> entry : map.entrySet()) {
            sum += (entry.getValue() * (entry.getValue() - 1)) / 2;
        }

        return sum;
    }

    public static void main(String[] args) {
        NumberOfGoodPairs solution = new NumberOfGoodPairs();
        int[] nums = {1, 2, 3, 1, 1, 3};
        System.out.println("Number of Good Pairs: " + solution.numIdenticalPairs(nums));
    }
}

