# 169. Majority Element

## Problem Description

Given an array `nums` of size `n`, return the majority element.

The majority element is the element that appears more than `⌊n / 2⌋` times. You may assume that the majority element always exists in the array.

## Examples

### Example 1:
```
Input: nums = [3,2,3]
Output: 3
```

### Example 2:
```
Input: nums = [2,2,1,1,1,2,2]
Output: 2
```

## Constraints

- `n == nums.length`
- `1 <= n <= 5 * 10^4`
- `-10^9 <= nums[i] <= 10^9`

## Follow-up

Could you solve the problem in linear time and in O(1) space?

---

## Solution Approaches & Intuition

This problem can be solved using multiple approaches with different time and space complexities. Below are the three main approaches implemented:

### 1. Brute Force Approach (Naive Method)
**Time Complexity:** O(N²) | **Space Complexity:** O(1)

**Intuition:**
- For each element in the array, count how many times it appears
- If the count exceeds n/2, return that element
- This is the most straightforward approach but inefficient for large arrays

**Implementation:**
```cpp
// Check each element against all other elements
for(int i=0; i<n; i++){
    int count=0;
    for(int j=0; j<n; j++){
        if(nums[i]==nums[j]) count++;
    }
    if(count>n/2) return nums[i];
}
```

### 2. Hash Map Approach (Better Solution)
**Time Complexity:** O(N log N) | **Space Complexity:** O(N)

**Intuition:**
- Use a hash map to store the frequency of each element
- Iterate through the map to find the element with frequency > n/2
- More efficient than brute force but uses extra space

**Implementation:**
```cpp
map<int, int> mpp;
for(int i=0; i<n; i++){
    mpp[nums[i]]++;
}
for(auto it:mpp){
    if(it.second>n/2) return it.first;
}
```

### 3. Moore's Voting Algorithm (Optimal Solution)
**Time Complexity:** O(N) | **Space Complexity:** O(1)

**Intuition:**
- The key insight is that if an element appears more than n/2 times, it will always "survive" the voting process
- Maintain a candidate and a count
- If count becomes 0, choose the current element as the new candidate
- If the current element matches the candidate, increment count; otherwise, decrement it
- The surviving candidate will be the majority element

**Algorithm Steps:**
1. Initialize count = 0 and candidate = 0
2. For each element:
   - If count is 0, set the current element as candidate
   - If current element equals candidate, increment count
   - Otherwise, decrement count
3. Return the candidate

**Implementation:**
```cpp
int count = 0;
int candidate = 0;
for (int num : nums) {
    if (count == 0) {
        candidate = num;
    }
    count += (num == candidate) ? 1 : -1;
}
return candidate;
```

**Why Moore's Algorithm Works:**
- If we pair up elements that are different, the majority element will always have some unpaired occurrences left
- The algorithm essentially cancels out different elements, leaving the majority element as the survivor

## Implementation

The complete solution with all three approaches can be found in the `169-Majority Element.cpp` file in this directory.