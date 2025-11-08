/*
Problem: Container With Most Water
Topic: arrays
Description: You are given an integer array `height` of length n. There are n vertical lines drawn such that the two endpoints of the i-th line are (i, 0) and (i, height[i]).
Find two lines that together with the x-axis form a container, such that the container contains the most water.
Return the maximum amount of water a container can store.
Time Complexity: O(n)
Space Complexity: O(1)
*/

/*
Algorithm intuition (short):
- Use two pointers starting at both ends. The area is limited by the shorter line, so move the smaller pointer inward to try to find a taller line and potentially increase area.
- This guarantees O(n) time since each pointer moves at most n steps.
*/

'use strict';

/**
 * Computes area between indices l and r in the height array.
 * Kept as a small helper for readability.
 *
 * @param {number} l - left index
 * @param {number} r - right index
 * @param {number[]} height - array of heights
 * @returns {number} - area formed between lines at indices l and r
 */
function area(l, r, height) {
  // width = (r - l), height limited by the shorter line
  return Math.min(height[l], height[r]) * (r - l);
}

/**
 * Finds the maximum water container area using the two-pointer technique.
 *
 * @param {number[]} height - array of line heights
 * @returns {number} - maximum area
 */
function maxArea(height) {
  const n = height.length;
  let l = 0;          // left pointer at the start
  let r = n - 1;      // right pointer at the end
  let best = 0;       // stores the maximum area found so far

  // move pointers until they meet
  while (l < r) {
    // compute current area and update best
    best = Math.max(best, area(l, r, height));

    // move the pointer at the smaller height inward
    // (only moving the smaller one can possibly increase the limiting height)
    if (height[l] < height[r]) {
      l += 1; // try to find a taller left line
    } else {
      r -= 1; // try to find a taller right line
    }
  }

  return best;
}

/*
Time Complexity: O(n)
 - We use two pointers that move inward; each index is visited at most once.

Space Complexity: O(1)
 - Only a few scalar variables are used (l, r, best); no extra data structures.
*/

// Example usage:
const heights = [1,8,6,2,5,4,8,3,7];
console.log("Max water area:", maxArea(heights)); // Expected output: 49

// Export for module systems (optional)
// module.exports = { maxArea };
