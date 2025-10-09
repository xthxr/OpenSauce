#include <bits/stdc++.h>
using namespace std;

// Function to check if there is a subset of 'arr' with a sum equal to 'target'
bool subsetSumUtil(int ind, int target, vector<int>& arr, vector<vector<int>>& dp) {
    // If the target sum is 0, we have found a subset
    if (target == 0)
        return true;

    // If we have reached the first element in 'arr'
    if (ind == 0)
        return arr[0] == target;

    // If the result for this subproblem has already been computed, return it
    if (dp[ind][target] != -1)
        return dp[ind][target];

    // Try not taking the current element into the subset
    bool notTaken = subsetSumUtil(ind - 1, target, arr, dp);

    // Try taking the current element into the subset if it doesn't exceed the target
    bool taken = false;
    if (arr[ind] <= target)
        taken = subsetSumUtil(ind - 1, target - arr[ind], arr, dp);

    // Store the result in the dp array to avoid recomputation
    return dp[ind][target] = notTaken || taken;
}

// Function to check if there is a subset of 'arr' with a sum equal to 'k'
bool subsetSumToK(int n, int k, vector<int>& arr) {
    // Initialize a 2D DP array for memoization
    vector<vector<int>> dp(n, vector<int>(k + 1, -1));

    // Call the recursive subsetSumUtil function
    return subsetSumUtil(n - 1, k, arr, dp);
}

int main() {
    vector<int> arr = {1, 2, 3, 4};
    int k = 4;
    int n = arr.size();

    // Call the subsetSumToK function and print the result
    if (subsetSumToK(n, k, arr))
        cout << "Subset with the given target found";
    else
        cout << "Subset with the given target not found";

    return 0;
}