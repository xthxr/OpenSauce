/*
Problem: Shortest Subarray with Sum at Least K
Topic: arrays, prefix sums, monotonic deque (deque)
Description:
Given an integer array nums and an integer k, return the length of the shortest,
non-empty, contiguous subarray of nums with sum at least k. If there is no such
subarray, return -1.

Notes:

Elements may be negative, so classic two-pointer sliding window doesn't work.

Use prefix sums + a monotonic deque of indices to achieve O(n) time.

Time Complexity: O(n) // each index is pushed/popped at most once
Space Complexity: O(n) // for prefix sums and deque
*/

/*
Algorithm intuition (short):

Build prefix sums pref where pref[i] is sum of first i elements (pref[0] = 0).

For any j>i, sum(nums[i..j-1]) = pref[j] - pref[i]. We want pref[j] - pref[i] >= k.

Iterate j from 0..n and maintain a deque of indices dq with strictly increasing pref values.

While dq not empty and pref[j] - pref[dq.front()] >= k: we found a valid subarray; update ans with j - dq.front(), then pop_front() because it's the shortest using that front.

While dq not empty and pref[j] <= pref[dq.back()]: pop_back() because a larger prefix is dominated by j (worse candidate).

Push j into deque. Return ans if found else -1.
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
// Return length of shortest subarray with sum >= k, or -1 if none.
static int shortestSubarray(const vector<int>& nums, int k) {
int n = (int)nums.size();
// prefix sums: pref[0] = 0, pref[i] = sum of first i elements
vector<long long> pref(n + 1, 0);
for (int i = 0; i < n; ++i) pref[i + 1] = pref[i] + nums[i];

    deque<int> dq; // will store indices of pref[] with increasing pref values
    int ans = INT_MAX;

    for (int j = 0; j <= n; ++j) {
        // Check if we can form a valid subarray ending at j using smallest pref index
        while (!dq.empty() && pref[j] - pref[dq.front()] >= k) {
            ans = min(ans, j - dq.front());
            dq.pop_front();
        }

        // Maintain monotonicity of prefix sums in deque:
        // If pref[j] <= pref[back], then back is dominated by j and can be removed
        while (!dq.empty() && pref[j] <= pref[dq.back()]) {
            dq.pop_back();
        }

        dq.push_back(j);
    }

    return (ans == INT_MAX) ? -1 : ans;
}


};

/*
Example usage (uncomment to run as standalone program):

int main() {
vector<int> a1 = {2, -1, 2};
cout << "Expected 3, Got: " << Solution::shortestSubarray(a1, 3) << endl;

vector<int> a2 = {1, 2};
cout << "Expected -1, Got: " << Solution::shortestSubarray(a2, 4) << endl;

vector<int> a3 = {84, -37, 32, 40, 95};
cout << "Expected 3, Got: " << Solution::shortestSubarray(a3, 167) << endl;

return 0;


}
*/