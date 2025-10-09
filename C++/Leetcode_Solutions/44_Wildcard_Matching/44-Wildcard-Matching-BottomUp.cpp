/*
Wildcard Matching - Bottom-Up DP
LeetCode 44: https://leetcode.com/problems/wildcard-matching/

Time Complexity: O(n * m)
Space Complexity: O(n * m)

Determines if a given string matches a pattern that includes '?' and '*'.
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    bool isMatch(string s, string p) {
        int n = s.size(), m = p.size();
        vector<vector<bool>> dp(n + 1, vector<bool>(m + 1, false));
        dp[0][0] = true; // empty string matches empty pattern

        // Initialize first row (empty string vs pattern)
        for (int j = 1; j <= m; j++) {
            if (j == 1) dp[0][j] = p[j - 1] == '*';
            else dp[0][j] = dp[0][j - 1] && p[j - 1] == '*';
        }

        // Fill DP table
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= m; j++) {
                if (s[i - 1] == p[j - 1] || p[j - 1] == '?') {
                    dp[i][j] = dp[i - 1][j - 1];
                } else if (p[j - 1] == '*') {
                    dp[i][j] = dp[i - 1][j] || dp[i - 1][j - 1] || dp[i][j - 1];
                }
            }
        }

        return dp[n][m];
    }
};

// Example usage
int main() {
    Solution sol;
    string s = "adceb", p = "*a*b";
    cout << (sol.isMatch(s, p) ? "Match" : "No Match") << endl;
    return 0;
}
