/*
Wildcard Matching - Top-Down DP (Memoization)
LeetCode 44: https://leetcode.com/problems/wildcard-matching/

Time Complexity: O(n * m)
Space Complexity: O(n * m) + recursion stack

Uses recursion with memoization to check if a string matches a pattern with '?' and '*'.
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    bool f(string &s, string &p, int i, int j, vector<vector<int>> &dp) {
        if (i == s.size()) {
            while (j < p.size() && p[j] == '*') j++;
            return j == p.size();
        }
        if (dp[i][j] != -1) return dp[i][j];

        bool ans = false;
        if (s[i] == p[j] || p[j] == '?') {
            ans = f(s, p, i + 1, j + 1, dp);
        }
        if (p[j] == '*') {
            ans = f(s, p, i + 1, j + 1, dp) || f(s, p, i + 1, j, dp) || f(s, p, i, j + 1, dp);
        }

        return dp[i][j] = ans;
    }

    bool isMatch(string s, string p) {
        vector<vector<int>> dp(s.size() + 1, vector<int>(p.size() + 1, -1));
        return f(s, p, 0, 0, dp);
    }
};

// Example usage
int main() {
    Solution sol;
    string s = "adceb", p = "*a*b";
    cout << (sol.isMatch(s, p) ? "Match" : "No Match") << endl;
    return 0;
}
