/*
Problem: Find Peak Element (LeetCode 162)
Difficulty: Medium
Topic: Binary Search
Description: A peak element is an element that is strictly greater than its neighbors.
Given a 0-indexed integer array nums, find a peak element, and return its index. If the array contains multiple peaks, return the index to any of the peaks.
You may imagine that nums[-1] = nums[n] = -∞. In other words, an element is always considered to be strictly greater than a neighbor that is outside the array.
You must write an algorithm that runs in O(log n) time.

Time Complexity: O(log n)
Space Complexity: O(1)
*/

/*
Algorithm intuition:
- Use binary search by checking the slope at mid point
- If nums[mid] < nums[mid+1], peak must be on the right side (go right)
- If nums[mid] > nums[mid+1], peak must be on the left side or mid itself (go left)
- This works because we're guaranteed to find a peak due to the -∞ boundary conditions
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    int findPeakElement(vector<int>& nums) {
        int left = 0, right = nums.size() - 1;
        
        while (left < right) {
            int mid = left + (right - left) / 2;
            
            // If mid element is smaller than next element,
            // peak must be on the right side
            if (nums[mid] < nums[mid + 1]) {
                left = mid + 1;
            } 
            // If mid element is greater than or equal to next element,
            // peak must be on the left side (including mid)
            else {
                right = mid;
            }
        }
        
        return left; // left == right at this point
    }
};

// Test function
int main() {
    Solution solution;
    
    // Test case 1: [1,2,3,1]
    vector<int> nums1 = {1, 2, 3, 1};
    int result1 = solution.findPeakElement(nums1);
    cout << "Test 1: " << result1 << " (Expected: 2, value: " << nums1[result1] << ")" << endl;
    
    // Test case 2: [1,2,1,3,5,6,4]
    vector<int> nums2 = {1, 2, 1, 3, 5, 6, 4};
    int result2 = solution.findPeakElement(nums2);
    cout << "Test 2: " << result2 << " (Expected: 1 or 5, value: " << nums2[result2] << ")" << endl;
    
    // Test case 3: [1]
    vector<int> nums3 = {1};
    int result3 = solution.findPeakElement(nums3);
    cout << "Test 3: " << result3 << " (Expected: 0, value: " << nums3[result3] << ")" << endl;
    
    // Test case 4: [1,2]
    vector<int> nums4 = {1, 2};
    int result4 = solution.findPeakElement(nums4);
    cout << "Test 4: " << result4 << " (Expected: 1, value: " << nums4[result4] << ")" << endl;
    
    return 0;
}