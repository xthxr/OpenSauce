/**
 * @file max_subarray_sum_circular.cpp
 * @brief Finds the maximum subarray sum in a circular array.
 *
 * Problem:
 * Given a circular integer array `nums`, find the maximum possible sum of a non-empty subarray.
 * A circular array means that the end of the array connects back to the start.
 *
 * Example:
 * Input: nums = [5, -3, 5]
 * Output: 10
 * Explanation: The subarray [5, 5] forms a circular subarray with sum = 10.
 *
 * Approach:
 * 1. Use Kadane’s algorithm to find:
 *    - The maximum subarray sum (non-circular).
 *    - The minimum subarray sum (to compute the circular case).
 * 2. For the circular case, total_sum - min_subarray_sum gives the max circular sum.
 * 3. Return the maximum of the two cases.
 *
 * Edge Case:
 * - If all numbers are negative, the circular sum becomes 0 (invalid), 
 *   so return the maximum element (non-circular result).
 *
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    int maxSubarraySumCircular(vector<int>& nums) {
        const int INF = 1 << 30;
        int minPrefixSum = 0, maxPrefixSum = -INF;
        int maxSubarraySum = -INF, minSubarraySum = INF;
        int currentPrefixSum = 0;

        for (int num : nums) {
            currentPrefixSum += num;

            // Non-wrapping case: max(prefix[j] - prefix[i])
            maxSubarraySum = max(maxSubarraySum, currentPrefixSum - minPrefixSum);

            // Wrapping helper: min(prefix[j] - prefix[i])
            minSubarraySum = min(minSubarraySum, currentPrefixSum - maxPrefixSum);

            // Update prefix boundaries
            minPrefixSum = min(minPrefixSum, currentPrefixSum);
            maxPrefixSum = max(maxPrefixSum, currentPrefixSum);
        }

        // Total sum of array
        int totalSum = currentPrefixSum;

        // If all numbers are negative, return non-circular result
        if (totalSum == minSubarraySum) return maxSubarraySum;

        // Max of non-wrapping and wrapping case
        return max(maxSubarraySum, totalSum - minSubarraySum);
    }
};

/**
 * Example usage:
 */
int main() {
    Solution sol;
    vector<int> nums = {5, -3, 5};
    cout << "Maximum Circular Subarray Sum: " 
         << sol.maxSubarraySumCircular(nums) << endl;
    return 0;
}
