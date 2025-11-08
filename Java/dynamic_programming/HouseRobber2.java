import java.util.ArrayList;
/**
 * House Robber 2
 * Leetcode: #213
 * Input: nums = [2,3,2]
Output: 3
Explanation: You cannot rob house 1 (money = 2) and then rob house 3 (money = 2), because they are adjacent houses.
 * Time Complexity:O(N) 
 * Space Complexity:O(N)
 */
public class HouseRobber2 {

    /**
     * Helper to compute maximum robbery for a linear street of houses (not circular).
     * Uses iterative DP with O(1) extra space.
     */
    private int robLinear(ArrayList<Integer> arr) {
        int m = arr.size();
        if (m == 0) return 0;
        if (m == 1) return arr.get(0);

        int prev2 = arr.get(0); // dp[i-2]
        int prev = Math.max(arr.get(0), arr.get(1)); // dp[i-1]

        for (int i = 2; i < m; i++) {
            int take = arr.get(i) + prev2;
            int nonTake = prev;
            int curr = Math.max(take, nonTake);
            prev2 = prev;
            prev = curr;
        }
        return prev;
    }

    public int rob(int[] nums) {
        int n = nums.length;
        if (n == 0) return 0;
        if (n == 1) return nums[0];

        ArrayList<Integer> arr1 = new ArrayList<>(); // exclude first
        ArrayList<Integer> arr2 = new ArrayList<>(); // exclude last
        for (int i = 0; i < n; i++) {
            if (i != 0) arr1.add(nums[i]);
            if (i != n - 1) arr2.add(nums[i]);
        }
        int ans1 = robLinear(arr1);
        int ans2 = robLinear(arr2);
        return Math.max(ans1, ans2);
    }

      public static void main(String[] args) {
        HouseRobber2 hr = new HouseRobber2();

        //  Test Case 1: Example case
        int[] nums1 = {2, 3, 2};
        System.out.println("Test 1 (Expected 3): " + hr.rob(nums1));

        //  Test Case 2: Robbing last house is better
        int[] nums2 = {1, 2, 3, 1};
        System.out.println("Test 2 (Expected 4): " + hr.rob(nums2));

        //  Test Case 3: Large values
        int[] nums3 = {200, 3, 140, 20, 10};
        System.out.println("Test 3 (Expected 340): " + hr.rob(nums3));

        //  Test Case 4: Single house
        int[] nums4 = {5};
        System.out.println("Test 4 (Expected 5): " + hr.rob(nums4));

        //  Test Case 5: Two houses
        int[] nums5 = {2, 3};
        System.out.println("Test 5 (Expected 3): " + hr.rob(nums5));

        //  Test Case 6: All houses have same value
        int[] nums6 = {5, 5, 5, 5, 5};
        System.out.println("Test 6 (Expected 10): " + hr.rob(nums6));
    }
}