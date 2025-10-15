/*
Trapping Rain Water Problem
Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it can trap after raining. 
For example,
Given [0,1,0,2,1,0,1,3,2,1,2,1], return 6.
*/

#include <bits/stdc++.h>
using namespace std;

// Prefix-Suffix (DP) approach: O(n) time, O(n) space
int trap_prefix_suffix(const vector<int>& height) {
    int n = (int)height.size();
    if (n <= 2) return 0;
    vector<int> pre(n), suf(n);
    pre[0] = height[0];
    for (int i = 1; i < n; ++i) pre[i] = max(pre[i-1], height[i]);
    suf[n-1] = height[n-1];
    for (int i = n-2; i >= 0; --i) suf[i] = max(suf[i+1], height[i]);
    int water = 0;
    for (int i = 0; i < n; ++i) {
        water += min(pre[i], suf[i]) - height[i];
    }
    return water;
}

// Brute-force approach: O(n^2) time, O(1) extra space
int trap_bruteforce(const vector<int>& height) {
    int n = (int)height.size();
    int total = 0;
    for (int i = 0; i < n; ++i) {
        int leftMax = 0, rightMax = 0;
        for (int l = 0; l <= i; ++l) leftMax = max(leftMax, height[l]);
        for (int r = i; r < n; ++r) rightMax = max(rightMax, height[r]);
        total += min(leftMax, rightMax) - height[i];
    }
    return total;
}

// Optimized two-pointer approach: O(n) time, O(1) extra space
int trap_two_pointer(const vector<int>& h) {
    int n = (int)h.size();
    if (n <= 2) return 0;
    int left = 0, right = n - 1;
    int leftMax = 0, rightMax = 0;
    int total = 0;
    while (left <= right) {
        if (h[left] <= h[right]) {
            if (h[left] >= leftMax) leftMax = h[left];
            else total += leftMax - h[left];
            ++left;
        } else {
            if (h[right] >= rightMax) rightMax = h[right];
            else total += rightMax - h[right];
            --right;
        }
    }
    return total;
}

// Utility: parse a line of integers
static vector<int> parse_numbers_from_stream(istream& in) {
    vector<int> res;
    string line;
    // Read all remaining integers from the stream
    while (true) {
        long long v;
        if (!(in >> v)) break;
        res.push_back((int)v);
    }
    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cout << "Trapping Rain Water - multiple implementations\n";
    cout << "Provide heights either as: N h1 h2 ... hN  OR as a list of heights (space/newline separated).\n";
    cout << "Example: 6 4 2 0 3 2 5  OR  4 2 0 3 2 5\n";
    cout << "Enter heights (Ctrl+Z then Enter to finish on Windows):\n";

    vector<int> nums = parse_numbers_from_stream(cin);
    if (nums.empty()) {
        cerr << "No input provided. Exiting.\n";
        return 1;
    }

    vector<int> heights;
    // If first number equals count of remaining numbers, treat it as N
    if ((size_t)nums[0] == nums.size() - 1) {
        heights.assign(nums.begin() + 1, nums.end());
    } else {
        heights = nums;
    }

    cout << "Heights read: ";
    for (size_t i = 0; i < heights.size(); ++i) {
        if (i) cout << ' ';
        cout << heights[i];
    }
    cout << '\n';

    int ans_prefix = trap_prefix_suffix(heights);
    int ans_brute = trap_bruteforce(heights);
    int ans_opt = trap_two_pointer(heights);

    cout << "\nResults:\n";
    cout << "  Prefix-Suffix (DP) : " << ans_prefix << " units\n";
    cout << "  Brute-force        : " << ans_brute << " units\n";
    cout << "  Two-pointer (O(n)) : " << ans_opt << " units\n";

    if (!(ans_prefix == ans_brute && ans_brute == ans_opt)) {
        cout << "\nWarning: implementations disagreed. Please verify input.\n";
    }

    return 0;
}