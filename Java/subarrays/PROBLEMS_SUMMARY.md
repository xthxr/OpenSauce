# Subarray Problems - Quick Reference

## 📊 Problems Overview

| # | Problem | Difficulty | Technique | Time | Space | LeetCode |
|---|---------|-----------|-----------|------|-------|----------|
| 1 | Maximum Sum Subarray | Medium | Kadane's Algorithm | O(n) | O(1) | #53 |
| 2 | Subarray with Given Sum | Medium | Sliding Window / HashMap | O(n) | O(1)/O(n) | - |
| 3 | Longest Subarray with Sum K | Medium | Prefix Sum + HashMap | O(n) | O(n) | - |
| 4 | Count Subarrays with Sum K | Medium | Prefix Sum Frequency | O(n) | O(n) | #560 |
| 5 | Maximum Product Subarray | Medium | Track Min/Max | O(n) | O(1) | #152 |
| 6 | Sliding Window Maximum | Hard | Deque | O(n) | O(k) | #239 |
| 7 | Longest Subarray with 1s After K Flips | Medium | Sliding Window | O(n) | O(1) | #1004 |
| 8 | Minimum Size Subarray Sum | Medium | Sliding Window | O(n) | O(1) | #209 |
| 9 | Subarrays Divisible by K | Medium | Modulo + HashMap | O(n) | O(k) | #974 |
| 10 | Longest Substring with K Distinct | Medium | Sliding Window | O(n) | O(k) | - |
| 11 | Subarray with Zero Sum | Medium | Prefix Sum + HashSet | O(n) | O(n) | - |

## 🎯 Technique Classification

### Kadane's Algorithm
- **MaximumSumSubarray.java** - Classic Kadane's for maximum sum
- **MaximumProductSubarray.java** - Modified Kadane's tracking min/max

### Sliding Window (Two Pointers)
- **SubarrayWithGivenSum.java** - Basic sliding window for positive numbers
- **LongestSubarrayWithOnesAfterKFlips.java** - Window with constraint counting
- **MinimumSizeSubarraySum.java** - Minimum window size problem
- **SlidingWindowMaximum.java** - Advanced deque-based window
- **LongestSubstringWithKDistinct.java** - Character frequency window

### Prefix Sum + HashMap
- **LongestSubarrayWithSumK.java** - Find longest with exact sum
- **CountSubarraysWithSumK.java** - Count all with exact sum
- **SubarraysDivisibleByK.java** - Modulo arithmetic with prefix sums
- **SubarrayWithZeroSum.java** - Detect zero-sum subarrays

## 📚 Learning Path

### Beginner Level
1. Start with **MaximumSumSubarray.java** - Learn Kadane's Algorithm
2. Practice **SubarrayWithGivenSum.java** - Understand sliding window basics
3. Try **SubarrayWithZeroSum.java** - Learn prefix sum concept

### Intermediate Level
4. Solve **LongestSubarrayWithSumK.java** - Combine prefix sum + HashMap
5. Work on **CountSubarraysWithSumK.java** - Master frequency counting
6. Practice **MinimumSizeSubarraySum.java** - Optimize window size

### Advanced Level
7. Challenge **MaximumProductSubarray.java** - Handle edge cases (negatives, zeros)
8. Master **SlidingWindowMaximum.java** - Learn deque optimization
9. Solve **SubarraysDivisibleByK.java** - Apply modulo arithmetic

## 🔑 Key Concepts

### 1. Prefix Sum
```
prefixSum[i] = arr[0] + arr[1] + ... + arr[i]
subarray_sum(i, j) = prefixSum[j] - prefixSum[i-1]
```

### 2. Sliding Window Template
```java
int left = 0;
for (int right = 0; right < n; right++) {
    // Add right element to window
    
    while (window_invalid) {
        // Remove left element
        left++;
    }
    
    // Update result
}
```

### 3. Kadane's Algorithm
```java
maxSum = arr[0], currentSum = arr[0];
for (i = 1 to n-1) {
    currentSum = max(arr[i], currentSum + arr[i]);
    maxSum = max(maxSum, currentSum);
}
```

## 💡 Common Patterns

### Pattern 1: Find Subarray with Exact Sum
- Use HashMap to store prefix sums
- Check if (currentSum - target) exists

### Pattern 2: Find Longest/Shortest Subarray
- Use sliding window with two pointers
- Expand right, shrink left based on condition

### Pattern 3: Count Subarrays
- Use HashMap to store frequency
- Add frequency of matching prefix sums

### Pattern 4: Handle Negative Numbers
- Sliding window doesn't work directly
- Use prefix sum + HashMap approach

## 🧪 Test Your Understanding

After studying these problems, you should be able to:
- [ ] Implement Kadane's Algorithm from scratch
- [ ] Explain when to use sliding window vs prefix sum
- [ ] Handle edge cases (empty array, single element, all negatives)
- [ ] Optimize brute force O(n²) solutions to O(n)
- [ ] Choose appropriate data structure (HashMap, HashSet, Deque)

## 🚀 Practice Tips

1. **Start Simple**: Begin with brute force, then optimize
2. **Draw It Out**: Visualize the window/subarray movement
3. **Edge Cases**: Always test with empty, single element, all same values
4. **Time Yourself**: Try to solve within 30-45 minutes
5. **Code Without IDE**: Practice on paper or whiteboard

## 📖 Additional Resources

- **Kadane's Algorithm**: Understanding maximum subarray problem
- **Sliding Window**: Two-pointer technique for contiguous elements
- **Prefix Sum**: Cumulative sum for range queries
- **HashMap**: For O(1) lookup of previous states

## 🎓 Interview Frequency

**High Frequency** (Asked often):
- Maximum Sum Subarray
- Subarray with Given Sum
- Sliding Window Maximum

**Medium Frequency**:
- Maximum Product Subarray
- Count Subarrays with Sum K
- Longest Subarray with K Distinct

**Low Frequency** (Good to know):
- Subarrays Divisible by K
- Subarray with Zero Sum

---

**Happy Coding! 🎉**

Remember: The key to mastering subarray problems is understanding the underlying patterns and practicing regularly!
