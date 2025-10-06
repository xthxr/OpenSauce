/*
Problem: Subarray Sum Equals K (LeetCode 560)

Given an integer array `arr` and an integer `k`, 
return the total number of continuous subarrays 
whose sum equals to `k`.

Example:
Input: arr = [1, 2, 3], k = 3
Output: 2
Explanation: Subarrays [1,2] and [3] have sum = 3.

Algorithm:
1. Use a variable `prefix` to maintain the running prefix sum.
2. Use a hash map (unordered_map) to store frequencies of prefix sums encountered.
3. For each element:
   - Update prefix sum.
   - If prefix == k → increment answer.
   - If (prefix - k) exists in map → add its frequency to answer.
   - Insert/update current prefix in map.
4. Return the final answer.
Complexity Analysis:
- Time Complexity: O(n), since each element is processed once.
- Space Complexity: O(n), due to the hash map storing prefix sums.

*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    int subarraySum(vector<int>& arr, int k) {
        int n = arr.size(); 
        unordered_map<int,int> mp; 
        int ans = 0, prefix = 0;

        for (int i = 0; i < n; i++) {
            prefix += arr[i];

            // Case 1: subarray starts from index 0
            if (prefix == k) ans++;

            // Case 2: subarray found using previous prefix
            if (mp.find(prefix - k) != mp.end()) {
                ans += mp[prefix - k];
            }

            // Store current prefix in map
            mp[prefix]++;
        }
        return ans;
    }
};

int main() {
    Solution sol;

    // 🔹 Test Cases
    vector<int> arr1 = {1, 2, 3};
    cout << "Output: " << sol.subarraySum(arr1, 3) << " (Expected: 2)" << endl;

    vector<int> arr2 = {1, 1, 1};
    cout << "Output: " << sol.subarraySum(arr2, 2) << " (Expected: 2)" << endl;

    vector<int> arr3 = {3, 4, 7, 2, -3, 1, 4, 2};
    cout << "Output: " << sol.subarraySum(arr3, 7) << " (Expected: 4)" << endl;

    vector<int> arr4 = {-1, -1, 1};
    cout << "Output: " << sol.subarraySum(arr4, 0) << " (Expected: 1)" << endl;

    vector<int> arr5 = {1};
    cout << "Output: " << sol.subarraySum(arr5, 0) << " (Expected: 0)" << endl;
}
