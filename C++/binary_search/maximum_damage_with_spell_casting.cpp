/*
🏰 Problem: Maximum Total Spell Damage
--------------------------------------
A magician has various spells.

You are given an array `power`, where each element represents the damage of a spell. 
Multiple spells can have the same damage value.

If a magician decides to cast a spell with damage `power[i]`, they CANNOT cast 
any spell with damage `power[i] - 2`, `power[i] - 1`, `power[i] + 1`, or `power[i] + 2`.

Each spell can be cast only once.

Return the maximum possible total damage that a magician can cast.

--------------------------------------
📘 Example:
--------------------------------------
Input:  power = [1, 1, 3, 4]
Output: 6

Explanation:
The magician can cast spells with damage 1, 1, and 4 → total = 6
(Spells with damage 3 cannot be used as it’s adjacent to 4).

--------------------------------------
💡 Approach:
--------------------------------------
1️⃣ Group all identical spell powers together.
   - For each unique damage value `p`, store its total contribution (`p * frequency`).

2️⃣ Sort the unique powers.

3️⃣ Use Dynamic Programming + Binary Search:
   - At each index `i`, you have two choices:
     🟩 Take: add current power’s contribution and jump to the next valid spell (>= power[i] + 3)
     🟥 Skip: move to the next spell.

4️⃣ Recursively compute the maximum using memoization.

--------------------------------------
⏱️ Time Complexity:  O(n log n)
   (Sorting + Binary Search for each unique spell power)
💾 Space Complexity: O(n)
   (DP array + frequency map)

--------------------------------------
🎯 Author: Ankit Pratap
--------------------------------------
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    // Binary search to find next valid spell index (>= current + 3)
    int findNext(vector<pair<int, long long>>& arr, int start, int target) {
        int low = start, high = arr.size() - 1, ans = arr.size();
        while (low <= high) {
            int mid = low + (high - low) / 2;
            if (arr[mid].first >= target) {
                ans = mid;
                high = mid - 1;
            } else {
                low = mid + 1;
            }
        }
        return ans;
    }

    // Recursive DP to compute maximum total damage
    long long solve(int index, vector<pair<int, long long>>& arr, vector<long long>& dp) {
        if (index >= arr.size()) return 0;
        if (dp[index] != -1) return dp[index];

        // Find next spell that can be used (>= current + 3)
        int nextIdx = findNext(arr, index + 1, arr[index].first + 3);

        // Choice 1: Take current spell’s damage
        long long take = arr[index].second + solve(nextIdx, arr, dp);

        // Choice 2: Skip current spell
        long long skip = solve(index + 1, arr, dp);

        // Return maximum of both choices
        return dp[index] = max(take, skip);
    }

    long long maximumTotalDamage(vector<int>& power) {
        unordered_map<int, long long> freq;

        // Group spells by their damage value
        for (int p : power) freq[p] += p;

        // Create sorted array of (damage, total_contribution)
        vector<pair<int, long long>> arr(freq.begin(), freq.end());
        sort(arr.begin(), arr.end());

        // DP array initialized with -1
        vector<long long> dp(arr.size(), -1);

        // Compute result starting from index 0
        return solve(0, arr, dp);
    }
};

// Example usage
int main() {
    Solution sol;
    vector<int> power = {1, 1, 3, 4};
    cout << sol.maximumTotalDamage(power) << endl; // Output: 6
    return 0;
}
