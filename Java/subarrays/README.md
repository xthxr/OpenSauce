# Subarray Problems

This directory contains a comprehensive collection of subarray-based problems, which are essential for mastering Data Structures & Algorithms (DSA).

## Problems Included

### Basic Subarray Problems

1. **MaximumSumSubarray.java** - Kadane's Algorithm
   - Find the contiguous subarray with maximum sum
   - Time: O(n), Space: O(1)
   - LeetCode #53

2. **SubarrayWithGivenSum.java**
   - Find a subarray with a specific sum
   - Two approaches: Sliding window (positive) and HashMap (with negatives)
   - Time: O(n), Space: O(1) or O(n)

3. **LongestSubarrayWithSumK.java**
   - Find the longest subarray with sum equal to K
   - Uses prefix sum and HashMap
   - Time: O(n), Space: O(n)

4. **CountSubarraysWithSumK.java**
   - Count all subarrays with sum equal to K
   - Uses prefix sum frequency map
   - Time: O(n), Space: O(n)
   - LeetCode #560

### Product-Based Problems

5. **MaximumProductSubarray.java**
   - Find the contiguous subarray with maximum product
   - Tracks both max and min products (for negative numbers)
   - Time: O(n), Space: O(1)
   - LeetCode #152

### Sliding Window Problems

6. **SlidingWindowMaximum.java**
   - Find maximum in each sliding window of size k
   - Uses deque for efficient tracking
   - Time: O(n), Space: O(k)
   - LeetCode #239

7. **LongestSubarrayWithOnesAfterKFlips.java**
   - Find longest subarray of 1s after flipping at most k zeros
   - Sliding window with zero counting
   - Time: O(n), Space: O(1)
   - LeetCode #1004

8. **MinimumSizeSubarraySum.java**
   - Find minimal length subarray with sum >= target
   - Sliding window technique
   - Time: O(n), Space: O(1)
   - LeetCode #209

### Advanced Subarray Problems

9. **SubarraysDivisibleByK.java**
   - Count subarrays with sum divisible by K
   - Uses modulo arithmetic with prefix sums
   - Time: O(n), Space: O(k)
   - LeetCode #974

10. **LongestSubstringWithKDistinct.java**
    - Find longest substring with at most K distinct characters
    - Sliding window with HashMap
    - Time: O(n), Space: O(k)

11. **SubarrayWithZeroSum.java**
    - Check if zero-sum subarray exists
    - Find all zero-sum subarrays
    - Time: O(n), Space: O(n)

## Key Techniques

### 1. Kadane's Algorithm
Used for maximum sum subarray problems. Maintains current sum and resets when it becomes negative.

### 2. Sliding Window
Two-pointer technique where window expands and shrinks based on conditions. Efficient for contiguous subarray problems.

### 3. Prefix Sum with HashMap
Store cumulative sums to find subarrays with specific properties in O(n) time.

### 4. Two Pointers
Maintain left and right pointers to track window boundaries.

### 5. Deque for Window Extremes
Use double-ended queue to efficiently track maximum/minimum in sliding windows.

## Problem-Solving Patterns

### When to use Sliding Window:
- Finding subarrays with specific size or sum constraints
- Problems with "at most K" or "at least K" conditions
- Contiguous elements with optimization goals

### When to use Prefix Sum + HashMap:
- Finding subarrays with exact sum
- Counting subarrays with specific properties
- Problems involving sum differences

### When to use Kadane's Algorithm:
- Maximum/minimum sum subarray
- Can be extended for product, XOR, etc.

## Complexity Analysis

Most subarray problems can be solved in:
- **Time Complexity**: O(n) using optimal techniques
- **Space Complexity**: O(1) to O(n) depending on approach

Brute force approaches typically have O(n²) or O(n³) time complexity.

## Tips for Practice

1. Start with basic sum-based problems
2. Master the sliding window technique
3. Understand prefix sum concept thoroughly
4. Practice both "find" and "count" variations
5. Learn to handle edge cases (empty arrays, single elements, all negatives)

## Related Topics

- Arrays
- Two Pointers
- Hash Tables
- Dynamic Programming
- Greedy Algorithms

## Contributing

When adding new subarray problems:
- Follow the existing code structure
- Include time and space complexity
- Add comprehensive test cases
- Document the approach clearly
- Include LeetCode problem number if applicable
