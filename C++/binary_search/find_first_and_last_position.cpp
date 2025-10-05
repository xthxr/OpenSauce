/*
Problem: Find First and Last Position of Element in Sorted Array (LeetCode 34)
Difficulty: Medium
Topic: Binary Search
Description: Given an array of integers nums sorted in non-decreasing order, find the starting and ending position of a given target value.
If target is not found in the array, return [-1, -1].
You must write an algorithm with O(log n) runtime complexity.

Time Complexity: O(log n)
Space Complexity: O(1)
*/

/*
Algorithm intuition:
- Use two separate binary searches: one to find the leftmost occurrence, another for the rightmost
- For leftmost: when nums[mid] == target, continue searching left to find the first occurrence
- For rightmost: when nums[mid] == target, continue searching right to find the last occurrence
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    vector<int> searchRange(vector<int>& nums, int target) {
        vector<int> result = {-1, -1};
        
        // Find first position
        result[0] = findFirst(nums, target);
        if (result[0] == -1) {
            return result; // Target not found
        }
        
        // Find last position
        result[1] = findLast(nums, target);
        
        return result;
    }
    
private:
    int findFirst(vector<int>& nums, int target) {
        int left = 0, right = nums.size() - 1;
        int first = -1;
        
        while (left <= right) {
            int mid = left + (right - left) / 2;
            
            if (nums[mid] == target) {
                first = mid;
                right = mid - 1; // Continue searching left
            } else if (nums[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        
        return first;
    }
    
    int findLast(vector<int>& nums, int target) {
        int left = 0, right = nums.size() - 1;
        int last = -1;
        
        while (left <= right) {
            int mid = left + (right - left) / 2;
            
            if (nums[mid] == target) {
                last = mid;
                left = mid + 1; // Continue searching right
            } else if (nums[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        
        return last;
    }
};

// Test function
int main() {
    Solution solution;
    
    // Test case 1: [5,7,7,8,8,10], target = 8
    vector<int> nums1 = {5, 7, 7, 8, 8, 10};
    int target1 = 8;
    vector<int> result1 = solution.searchRange(nums1, target1);
    cout << "Test 1: [" << result1[0] << ", " << result1[1] << "] (Expected: [3, 4])" << endl;
    
    // Test case 2: [5,7,7,8,8,10], target = 6
    vector<int> nums2 = {5, 7, 7, 8, 8, 10};
    int target2 = 6;
    vector<int> result2 = solution.searchRange(nums2, target2);
    cout << "Test 2: [" << result2[0] << ", " << result2[1] << "] (Expected: [-1, -1])" << endl;
    
    // Test case 3: [], target = 0
    vector<int> nums3 = {};
    int target3 = 0;
    vector<int> result3 = solution.searchRange(nums3, target3);
    cout << "Test 3: [" << result3[0] << ", " << result3[1] << "] (Expected: [-1, -1])" << endl;
    
    return 0;
}