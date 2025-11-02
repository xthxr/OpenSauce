/* 
Problem: You are given an integer array cost where cost[i] is the cost of ith step on a staircase. Once you pay the cost, you can either climb one or two steps.
         You can either start from the step with index 0, or the step with index 1.
         Return the minimum cost to reach the top of the floor.

Time Complexity: O(n)
Space complexity: O(n)

Explanation:
- cost[i] → cost of step i
- Can climb 1 or 2 steps
- Goal → reach beyond last step with minimum cost
- Start from step 0 or 1 → take the smaller cost
- memo[i] → stores min cost from step i to avoid recalculation

Example:

Example1: 
Input: cost = [10,15,20]
Output: 15

Example2:
Input: cost = [1,100,1,1,1,100,1,1,100,1]
Output: 6
 */

import java.util.*;

class Solution {
    public int minCostClimbingStairs(int[] cost) {
        int n = cost.length;
        int[] memo = new int[n];
        Arrays.fill(memo, -1);

        int res = Math.min(helper(0, cost, memo), helper(1, cost, memo));

        return res;
    }

    public int helper(int i, int[]cost, int[] memo) {
            if(i >= cost.length) return 0;

            if(memo[i] != -1) return memo[i];

            int oneStep = helper(i+1, cost, memo);
            int twoStep = helper(i+2, cost, memo);

            memo[i] = cost[i] + Math.min(oneStep, twoStep);

            return memo[i];
        }

}