/*
Problem: Search in Rotated Sorted Array (LeetCode 33)
Difficulty: Medium
Topic: Binary Search
Description: There is an integer array nums sorted in ascending order (with distinct values).
Prior to being passed to your function, nums is possibly rotated at an unknown pivot index k (1 <= k < nums.length) 
such that the resulting array is [nums[k], nums[k+1], ..., nums[n-1], nums[0], nums[1], ..., nums[k-1]] (0-indexed).
For example, [0,1,2,4,5,6,7] might be rotated at pivot index 3 and become [4,5,6,7,0,1,2].
Given the array nums after the possible rotation and an integer target, return the index of target if it is in nums, or -1 if it is not in nums.
You must write an algorithm with O(log n) runtime complexity.

Time Complexity: O(log n)
Space Complexity: O(1)
*/

/*
Algorithm intuition:
- Use binary search but handle the rotation by checking which half is sorted
- If left half is sorted and target is in range, search left; otherwise search right
- If right half is sorted and target is in range, search right; otherwise search left
- This maintains O(log n) complexity even with rotation
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    int search(vector<int>& nums, int target) {
        int left = 0, right = nums.size() - 1;
        
        while (left <= right) {
            int mid = left + (right - left) / 2;
            
            // Found target
            if (nums[mid] == target) {
                return mid;
            }
            
            // Check if left half is sorted
            if (nums[left] <= nums[mid]) {
                // Target is in the sorted left half
                if (nums[left] <= target && target < nums[mid]) {
                    right = mid - 1;
                } else {
                    left = mid + 1;
                }
            }
            // Right half is sorted
            else {
                // Target is in the sorted right half
                if (nums[mid] < target && target <= nums[right]) {
                    left = mid + 1;
                } else {
                    right = mid - 1;
                }
            }
        }
        
        return -1; // Target not found
    }
};

// Test function
int main() {
    Solution solution;
    
    // Test case 1: [4,5,6,7,0,1,2], target = 0
    vector<int> nums1 = {4, 5, 6, 7, 0, 1, 2};
    int target1 = 0;
    cout << "Test 1: " << solution.search(nums1, target1) << " (Expected: 4)" << endl;
    
    // Test case 2: [4,5,6,7,0,1,2], target = 3
    vector<int> nums2 = {4, 5, 6, 7, 0, 1, 2};
    int target2 = 3;
    cout << "Test 2: " << solution.search(nums2, target2) << " (Expected: -1)" << endl;
    
    // Test case 3: [1], target = 0
    vector<int> nums3 = {1};
    int target3 = 0;
    cout << "Test 3: " << solution.search(nums3, target3) << " (Expected: -1)" << endl;
    
    return 0;
}