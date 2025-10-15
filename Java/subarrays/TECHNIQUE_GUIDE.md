# Subarray Problems - Technique Selection Guide

## 🎯 Quick Decision Tree

```
Is the problem about subarrays?
│
├─ YES → Continue below
│
└─ NO → Check other data structures

Problem Type?
│
├─ Find MAXIMUM/MINIMUM SUM
│   └─ Use: Kadane's Algorithm
│       Files: MaximumSumSubarray.java
│       Time: O(n), Space: O(1)
│
├─ Find MAXIMUM/MINIMUM PRODUCT
│   └─ Use: Modified Kadane's (track min & max)
│       Files: MaximumProductSubarray.java
│       Time: O(n), Space: O(1)
│
├─ Find subarray with EXACT SUM = K
│   ├─ All positive numbers?
│   │   └─ Use: Sliding Window
│   │       Files: SubarrayWithGivenSum.java
│   │       Time: O(n), Space: O(1)
│   │
│   └─ Has negative numbers?
│       └─ Use: Prefix Sum + HashMap
│           Files: SubarrayWithGivenSum.java, LongestSubarrayWithSumK.java
│           Time: O(n), Space: O(n)
│
├─ COUNT subarrays with property
│   └─ Use: Prefix Sum + Frequency HashMap
│       Files: CountSubarraysWithSumK.java, SubarraysDivisibleByK.java
│       Time: O(n), Space: O(n)
│
├─ Find LONGEST subarray with constraint
│   └─ Use: Sliding Window with condition tracking
│       Files: LongestSubarrayWithOnesAfterKFlips.java
│       Time: O(n), Space: O(1)
│
├─ Find SHORTEST subarray with constraint
│   └─ Use: Sliding Window (expand & shrink)
│       Files: MinimumSizeSubarraySum.java
│       Time: O(n), Space: O(1)
│
├─ Find MAX/MIN in each WINDOW
│   └─ Use: Deque (monotonic queue)
│       Files: SlidingWindowMaximum.java
│       Time: O(n), Space: O(k)
│
└─ Check if subarray EXISTS
    └─ Use: Prefix Sum + HashSet
        Files: SubarrayWithZeroSum.java
        Time: O(n), Space: O(n)
```

## 📋 Technique Comparison Table

| Technique | When to Use | Pros | Cons | Example Problems |
|-----------|-------------|------|------|------------------|
| **Kadane's Algorithm** | Max/min sum in contiguous subarray | Very fast O(n), O(1) space | Only for sum-based problems | Max subarray sum |
| **Sliding Window** | Contiguous elements, monotonic property | O(n) time, often O(1) space | Doesn't work with negatives for sum | Longest subarray with K flips |
| **Prefix Sum + HashMap** | Exact sum/count problems | Handles negatives, flexible | O(n) space needed | Count subarrays with sum K |
| **Two Pointers** | Optimize window boundaries | Intuitive, efficient | Requires sorted/monotonic data | Minimum size subarray sum |
| **Deque** | Track window extremes | O(n) for max/min tracking | More complex implementation | Sliding window maximum |
| **Frequency Map** | Count occurrences/patterns | Flexible for various counts | O(n) space | Subarrays divisible by K |

## 🔍 Pattern Recognition

### Pattern 1: "Find Maximum/Minimum Sum"
**Keywords**: maximum sum, minimum sum, largest sum, smallest sum
**Technique**: Kadane's Algorithm
**Code Template**:
```java
int maxSum = arr[0], currentSum = arr[0];
for (int i = 1; i < n; i++) {
    currentSum = Math.max(arr[i], currentSum + arr[i]);
    maxSum = Math.max(maxSum, currentSum);
}
```

### Pattern 2: "Subarray with Sum = K" (Positive Numbers)
**Keywords**: sum equals, sum is exactly, target sum
**Technique**: Sliding Window
**Code Template**:
```java
int left = 0, sum = 0;
for (int right = 0; right < n; right++) {
    sum += arr[right];
    while (sum > target && left <= right) {
        sum -= arr[left++];
    }
    if (sum == target) {
        // Found subarray [left, right]
    }
}
```

### Pattern 3: "Count Subarrays with Sum = K"
**Keywords**: count, number of subarrays, how many
**Technique**: Prefix Sum + Frequency HashMap
**Code Template**:
```java
HashMap<Integer, Integer> map = new HashMap<>();
map.put(0, 1);
int count = 0, sum = 0;
for (int num : arr) {
    sum += num;
    count += map.getOrDefault(sum - k, 0);
    map.put(sum, map.getOrDefault(sum, 0) + 1);
}
```

### Pattern 4: "Longest Subarray with Constraint"
**Keywords**: longest, maximum length, at most K
**Technique**: Sliding Window with Constraint Tracking
**Code Template**:
```java
int left = 0, maxLen = 0, constraint = 0;
for (int right = 0; right < n; right++) {
    // Update constraint based on arr[right]
    while (constraint > limit) {
        // Remove arr[left] from constraint
        left++;
    }
    maxLen = Math.max(maxLen, right - left + 1);
}
```

### Pattern 5: "Sliding Window Maximum/Minimum"
**Keywords**: maximum in window, minimum in window, each window of size K
**Technique**: Deque (Monotonic Queue)
**Code Template**:
```java
Deque<Integer> deque = new ArrayDeque<>();
for (int i = 0; i < n; i++) {
    // Remove out-of-window indices
    while (!deque.isEmpty() && deque.peekFirst() < i - k + 1) {
        deque.pollFirst();
    }
    // Maintain decreasing order for maximum
    while (!deque.isEmpty() && arr[deque.peekLast()] < arr[i]) {
        deque.pollLast();
    }
    deque.offerLast(i);
    if (i >= k - 1) {
        result[i - k + 1] = arr[deque.peekFirst()];
    }
}
```

## 🎓 Learning Progression

### Level 1: Fundamentals (Start Here)
1. **MaximumSumSubarray.java** - Learn Kadane's Algorithm
   - Understand: How to track current vs global maximum
   - Practice: Handle all negative numbers case

2. **SubarrayWithGivenSum.java** - Basic Sliding Window
   - Understand: Two-pointer movement
   - Practice: When to expand, when to shrink

### Level 2: Intermediate Patterns
3. **LongestSubarrayWithSumK.java** - Prefix Sum + HashMap
   - Understand: Why HashMap stores first occurrence
   - Practice: Handle zero and negative numbers

4. **CountSubarraysWithSumK.java** - Frequency Counting
   - Understand: Why we count frequencies, not just store indices
   - Practice: The (sum - k) lookup logic

5. **MinimumSizeSubarraySum.java** - Optimized Window
   - Understand: Greedy shrinking of window
   - Practice: Minimum vs maximum window problems

### Level 3: Advanced Techniques
6. **MaximumProductSubarray.java** - Modified Kadane's
   - Understand: Why track both min and max
   - Practice: Handle zeros and negative numbers

7. **SlidingWindowMaximum.java** - Deque Optimization
   - Understand: Monotonic queue concept
   - Practice: Maintain decreasing/increasing order

8. **LongestSubarrayWithOnesAfterKFlips.java** - Constraint Window
   - Understand: Counting within window
   - Practice: K-constraint problems

### Level 4: Special Cases
9. **SubarraysDivisibleByK.java** - Modulo Arithmetic
   - Understand: Remainder properties
   - Practice: Handle negative remainders

10. **SubarrayWithZeroSum.java** - Existence Check
    - Understand: HashSet vs HashMap usage
    - Practice: Find all vs find one

## 🚨 Common Pitfalls

### ❌ Pitfall 1: Using Sliding Window with Negative Numbers
**Problem**: Sliding window assumes monotonic property
**Solution**: Use prefix sum + HashMap instead

### ❌ Pitfall 2: Forgetting to Handle Empty Subarrays
**Problem**: Some problems allow empty subarrays, some don't
**Solution**: Check problem constraints carefully

### ❌ Pitfall 3: Integer Overflow
**Problem**: Sum/product can exceed int range
**Solution**: Use long for accumulation

### ❌ Pitfall 4: Off-by-One Errors in Window
**Problem**: Incorrect window size calculation
**Solution**: Use (right - left + 1) for window size

### ❌ Pitfall 5: Not Handling All Negative Numbers
**Problem**: Kadane's fails if not handled properly
**Solution**: Initialize with first element, not 0

## 💡 Pro Tips

### Tip 1: Start with Brute Force
Always understand the O(n²) solution first, then optimize.

### Tip 2: Draw the Window
Visualize how left and right pointers move.

### Tip 3: Test Edge Cases
- Empty array: []
- Single element: [5]
- All same: [3, 3, 3, 3]
- All negative: [-1, -2, -3]
- With zeros: [0, 1, 0, 2]

### Tip 4: Verify with Small Examples
Use arrays of size 3-5 to trace through logic.

### Tip 5: Know Your Data Structure
- HashMap: O(1) lookup, stores key-value
- HashSet: O(1) lookup, stores keys only
- Deque: O(1) add/remove from both ends

## 🎯 Interview Strategy

### Step 1: Clarify (2 minutes)
- Can array be empty?
- Are there negative numbers?
- What about duplicates?
- Is the subarray contiguous?

### Step 2: Identify Pattern (3 minutes)
- Is it sum/product/count?
- Is it find/count/check existence?
- Are there constraints (K flips, divisibility)?

### Step 3: Choose Technique (2 minutes)
- Use decision tree above
- Explain time/space complexity
- Mention alternative approaches

### Step 4: Code (15-20 minutes)
- Start with template
- Handle edge cases
- Add comments for clarity

### Step 5: Test (5 minutes)
- Walk through with example
- Test edge cases
- Verify complexity

## 📚 Additional Practice

After mastering these 11 problems, try these variations:
- Maximum sum circular subarray
- Subarray with given XOR
- Longest subarray with equal 0s and 1s
- Maximum average subarray
- Subarray with given difference

---

**Remember**: The key is recognizing patterns, not memorizing solutions!

🎯 **Master the technique, solve any problem!**
