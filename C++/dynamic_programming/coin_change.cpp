#include <bits/stdc++.h>
using namespace std;

// Function to count the number of ways to make change for a given target sum
long countWaysToMakeChangeUtil(vector<int>& arr, int ind, int T, vector<vector<long>>& dp) {
    // Base case: if we're at the first element
    if (ind == 0) {
        // Check if the target sum is divisible by the first element
        return (T % arr[0] == 0);
    }
    
    // If the result for this index and target sum is already calculated, return it
    if (dp[ind][T] != -1)
        return dp[ind][T];
        
    // Calculate the number of ways without taking the current element
    long notTaken = countWaysToMakeChangeUtil(arr, ind - 1, T, dp);
    
    // Calculate the number of ways by taking the current element
    long taken = 0;
    if (arr[ind] <= T)
        taken = countWaysToMakeChangeUtil(arr, ind, T - arr[ind], dp);
        
    // Store the sum of ways in the DP table and return it
    return dp[ind][T] = notTaken + taken;
}

// Function to count the number of ways to make change for the target sum
long countWaysToMakeChange(vector<int>& arr, int n, int T) {
    vector<vector<long>> dp(n, vector<long>(T + 1, -1)); // Create a DP table
    
    // Call the utility function to calculate the answer
    return countWaysToMakeChangeUtil(arr, n - 1, T, dp);
}

int main() {
    vector<int> arr = {1, 2, 3};
    int target = 4;
    int n = arr.size();
    
    cout << "The total number of ways is " << countWaysToMakeChange(arr, n, target) << endl;

    return 0; // Return 0 to indicate successful program execution
}
