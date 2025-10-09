#include <bits/stdc++.h>
using namespace std;

// Function to solve the problem using dynamic programming
int solveUtil(int ind, vector<int>& arr, vector<int>& dp) {
    // If the result for this index is already computed, return it
    if (dp[ind] != -1)
        return dp[ind];

    // Base cases
    if (ind == 0) 
        return arr[ind];
    if (ind < 0)  
        return 0;

    // Choose the current element or skip it, and take the maximum
    int pick = arr[ind] + solveUtil(ind - 2, arr, dp); // Choosing the current element
    int nonPick = 0 + solveUtil(ind - 1, arr, dp);      // Skipping the current element

    // Store the result in the DP table and return it
    return dp[ind] = max(pick, nonPick);
}

// Function to initiate the solving process
int solve(int n, vector<int>& arr) {
    vector<int> dp(n, -1); // Initialize the DP table with -1
    return solveUtil(n - 1, arr, dp); // Start solving from the last element
}

int main() {
    vector<int> arr{2, 1, 4, 9};
    int n = arr.size();
    
    // Call the solve function and print the result
    cout << solve(n, arr);

    return 0;
}