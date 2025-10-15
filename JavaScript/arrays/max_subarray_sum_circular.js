/**
 * @file max_subarray_sum_circular.js
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
 * Approach (Kadane-based, explained):
 * 1. Use Kadane’s algorithm to compute the maximum subarray sum for the non-circular case:
 *    - Keep track of `maxEndingHere` (best subarray that ends at current index)
 *      and `maxSoFar` (best over all indices).
 * 2. Use a modified Kadane’s algorithm to compute the minimum subarray sum:
 *    - Keep track of `minEndingHere` (worst subarray that ends at current index)
 *      and `minSoFar` (worst over all indices).
 * 3. Let totalSum be the sum of all elements.
 *    - The best circular subarray that wraps uses all elements except the minimum subarray:
 *      `circularBest = totalSum - minSoFar`.
 * 4. The answer is the maximum of:
 *      - `maxSoFar` (non-wrapping best), and
 *      - `circularBest` (wrapping best), except when `circularBest` would be 0 because
 *        that means `minSoFar == totalSum` (i.e., all numbers are part of the min subarray),
 *        which happens when all elements are negative. In that case we should return `maxSoFar`.
 *
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */

'use strict';

/**
 * Returns the maximum subarray sum for a circular array.
 *
 * @param {number[]} nums - input array (non-empty)
 * @returns {number} maximum circular subarray sum
 */
function maxSubarraySumCircular(nums) {
  // Basic Kadane for maximum subarray sum
  let maxEndingHere = nums[0];
  let maxSoFar = nums[0];

  // Modified Kadane for minimum subarray sum
  let minEndingHere = nums[0];
  let minSoFar = nums[0];

  // Total sum of array
  let totalSum = nums[0];

  // Iterate from second element onwards
  for (let i = 1; i < nums.length; ++i) {
    const x = nums[i];
    // Standard Kadane update for max
    maxEndingHere = Math.max(x, maxEndingHere + x);
    maxSoFar = Math.max(maxSoFar, maxEndingHere);

    // Kadane-like update for min (flip the logic)
    minEndingHere = Math.min(x, minEndingHere + x);
    minSoFar = Math.min(minSoFar, minEndingHere);

    // Accumulate total sum
    totalSum += x;
  }

  // If all numbers are negative, maxSoFar is the maximum element (best non-wrapping)
  // In that case totalSum === minSoFar (because the minimum subarray is the entire array),
  // and totalSum - minSoFar would be 0 which is invalid since subarray must be non-empty.
  if (totalSum === minSoFar) {
    return maxSoFar;
  }

  // Otherwise return the maximum between non-wrapping and wrapping cases
  return Math.max(maxSoFar, totalSum - minSoFar);
}

/**
 * Example usage:
 */
const nums = [5, -3, 5];
console.log("Maximum Circular Subarray Sum:", maxSubarraySumCircular(nums)); // Expected: 10

// Export for Node.js or module systems (optional)
// module.exports = { maxSubarraySumCircular };
