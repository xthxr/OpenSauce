/*
Problem: Subarray With Maximum Average
Topic: Arrays, Prefix Sum, Sliding Window
Description:
You are given an integer array `nums` of length n and an integer `k`.
Find the contiguous subarray of length k that has the maximum average value, 
and return this maximum average value.

Example:
Input: nums = [1, 12, -5, -6, 50, 3], k = 4
Output: 12.75
Explanation:
Subarray [12, -5, -6, 50] has the maximum average (51 / 4) = 12.75.

Constraints:
- 1 <= n <= 10^5
- -10^4 <= nums[i] <= 10^4
- 1 <= k <= n

Time Complexity: O(n)
Space Complexity: O(1)
*/

/*
Algorithm intuition (short):
- The goal is to find the subarray of length k with the maximum average.
- Instead of recalculating sums repeatedly, we use a sliding window of size k.
- Maintain the current sum of k elements, update it as we move the window by:
    sum = sum - nums[i - k] + nums[i]
- Keep track of the maximum sum seen.
- Since average = sum / k, maximizing sum also maximizes average.
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    static double findMaxAverage(const vector<int>& nums, int k) {
        int n = (int)nums.size();
        long long windowSum = 0;
        // Initialize sum of first k elements
        for (int i = 0; i < k; ++i) windowSum += nums[i];

        long long maxSum = windowSum;

        // Slide the window
        for (int i = k; i < n; ++i) {
            windowSum += nums[i] - nums[i - k];
            maxSum = max(maxSum, windowSum);
        }

        // Return the average as a double
        return (double)maxSum / k;
    }
};

// Example usage
