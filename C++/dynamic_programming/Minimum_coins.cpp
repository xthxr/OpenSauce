#include <bits/stdc++.h> // Include necessary libraries

using namespace std;

// Function to calculate the minimum number of elements to form the target sum
int minimumElementsUtil(vector<int>& arr, int ind, int T, vector<vector<int>>& dp){

    // Base case: If we're at the first element
    if(ind == 0){
        // Check if the target sum is divisible by the first element
        if(T % arr[0] == 0)
            return T / arr[0]; // If yes, return the quotient as the answer
        else
            return 1e9; // Otherwise, return a very large value to indicate it's not possible
    }
    
    // If the result for this index and target sum is already calculated, return it
    if(dp[ind][T] != -1)
        return dp[ind][T];
        
    // Calculate the minimum elements needed without taking the current element
    int notTaken = 0 + minimumElementsUtil(arr, ind - 1, T, dp);
    
    // Calculate the minimum elements needed by taking the current element
    int taken = 1e9; // Initialize 'taken' to a very large value
    if(arr[ind] <= T)
        taken = 1 + minimumElementsUtil(arr, ind, T - arr[ind], dp);
        
    // Store the minimum of 'notTaken' and 'taken' in the DP array and return it
    return dp[ind][T] = min(notTaken, taken);
}

// Function to find the minimum number of elements needed to form the target sum
int minimumElements(vector<int>& arr, int T){
    
    int n = arr.size();
    
    // Create a DP (Dynamic Programming) table with n rows and T+1 columns and initialize it with -1
    vector<vector<int>> dp(n, vector<int>(T + 1, -1));
    
    // Call the utility function to calculate the answer
    int ans =  minimumElementsUtil(arr, n - 1, T, dp);
    
    // If 'ans' is still very large, it means it's not possible to form the target sum
    if(ans >= 1e9)
        return -1;
    return ans; // Return the minimum number of elements needed
}

int main() {

    vector<int> arr = {1, 2, 3};
    int T = 7;
                                 
    cout << "The minimum number of coins required to form the target sum is " << minimumElements(arr, T);

    return 0; // Return 0 to indicate successful program execution
}
