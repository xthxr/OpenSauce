/*Problem: Given an array of integers nums and an integer k. A continuous subarray is called nice if there are k odd numbers on it.
Return the number of nice sub-arrays.

Example 1:
Input: nums = [1,1,2,1,1], k = 3
Output: 2
Explanation: The only sub-arrays with 3 odd numbers are [1,1,2,1] and [1,2,1,1].

Example 2:
Input: nums = [2,4,6], k = 1
Output: 0
Explanation: There are no odd numbers in the array.

Example 3:
Input: nums = [2,2,2,1,2,2,1,2,2,2], k = 2
Output: 16
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    int countAtMost(vector<int>& nums, int k) {
        int left = 0, res = 0;

        for (int right = 0; right < nums.size(); right++) {
            if (nums[right] % 2 != 0)
                k--;

            while (k < 0) {
                if (nums[left] % 2 != 0)
                    k++;
                left++;
            }
            res += (right - left + 1);
        }

        return res;
    }

    int numberOfSubarrays(vector<int>& nums, int k) {
        return countAtMost(nums, k) - countAtMost(nums, k - 1);
    }
};

int main() {
    Solution sol;
    vector<int> nums = {1, 1, 2, 1, 1};
    int k = 3;
    cout << sol.numberOfSubarrays(nums, k) << endl;
    return 0;
}
