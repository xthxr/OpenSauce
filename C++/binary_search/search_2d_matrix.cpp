/*
Problem: Search a 2D Matrix (LeetCode 74)
Difficulty: Medium
Topic: Binary Search
Description: You are given an m x n integer matrix with the following two properties:
- Each row is sorted in non-decreasing order.
- The first integer of each row is greater than the last integer of the previous row.
Given an integer target, return true if target is in matrix or false otherwise.
You must write a solution in O(log(m * n)) time complexity.

Time Complexity: O(log(m * n))
Space Complexity: O(1)
*/

/*
Algorithm intuition:
- Treat the 2D matrix as a flattened 1D sorted array
- Use binary search on the conceptual 1D array
- Convert 1D index to 2D coordinates: row = index / cols, col = index % cols
- This maintains O(log(m*n)) complexity
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    bool searchMatrix(vector<vector<int>>& matrix, int target) {
        if (matrix.empty() || matrix[0].empty()) {
            return false;
        }
        
        int m = matrix.size();
        int n = matrix[0].size();
        int left = 0, right = m * n - 1;
        
        while (left <= right) {
            int mid = left + (right - left) / 2;
            
            // Convert 1D index to 2D coordinates
            int row = mid / n;
            int col = mid % n;
            int midValue = matrix[row][col];
            
            if (midValue == target) {
                return true;
            } else if (midValue < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        
        return false;
    }
};

// Test function
int main() {
    Solution solution;
    
    // Test case 1: [[1,4,7,11],[2,5,8,12],[3,6,9,16]], target = 5
    vector<vector<int>> matrix1 = {
        {1, 4, 7, 11},
        {2, 5, 8, 12},
        {3, 6, 9, 16}
    };
    int target1 = 5;
    cout << "Test 1: " << (solution.searchMatrix(matrix1, target1) ? "true" : "false") 
         << " (Expected: true)" << endl;
    
    // Test case 2: [[1,4,7,11],[2,5,8,12],[3,6,9,16]], target = 13
    vector<vector<int>> matrix2 = {
        {1, 4, 7, 11},
        {2, 5, 8, 12},
        {3, 6, 9, 16}
    };
    int target2 = 13;
    cout << "Test 2: " << (solution.searchMatrix(matrix2, target2) ? "true" : "false") 
         << " (Expected: false)" << endl;
    
    // Test case 3: [[1]], target = 1
    vector<vector<int>> matrix3 = {{1}};
    int target3 = 1;
    cout << "Test 3: " << (solution.searchMatrix(matrix3, target3) ? "true" : "false") 
         << " (Expected: true)" << endl;
    
    return 0;
}