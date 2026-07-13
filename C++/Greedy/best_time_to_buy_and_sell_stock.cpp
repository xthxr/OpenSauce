#include <iostream>  
#include <vector>    
#include <algorithm>    
using namespace std;

/**
 * LeetCode 121. Best Time to Buy and Sell Stock
 *
 * You are given an array prices where prices[i] is the price of a given stock on the ith day.
 * You want to maximize your profit by choosing a single day to buy one stock and choosing a
 * different day in the future to sell that stock.
 * Return the maximum profit you can achieve from this transaction. If you cannot achieve any
 * profit, return 0.
 *
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */

int bestTimeToBuyAndSellStock(const vector<int>& prices) {
    
    // Step 1: Handle edge cases (less than 2 days)
    int sizeOfPrices = prices.size();
    if (sizeOfPrices < 2) {
        return 0;
    }

    // Step 2: Initialize min_price and max_profit
    int minPrice = prices[0];
    int maxProfit = 0;

    // Step 3: Iterate through prices starting from the second day
     int potentialProfit;
    for (int i = 1; i < sizeOfPrices; ++i) {
        
        int currentPrice = prices[i];

        // Calculate potential profit if we sell today
        potentialProfit = currentPrice - minPrice;

        // Update max_profit if this is a new high
        maxProfit = max(maxProfit, potentialProfit);
        
        // Update min_price for future calculations
        minPrice = min(minPrice, currentPrice);
    }

    // Step 4: Return the final max profit
    return maxProfit;
}

/**
 * @brief Main function of the program.
 *
 * This function is executed when the program runs.
 * It is used here to test the ' best time to buy and sell stock' function
 * with several example cases.
 */
int main() {
    
    // Example 1: Standard case
    vector<int> prices1 = {7, 1, 5, 3, 6, 4};
    cout << "Test Case 1: {7, 1, 5, 3, 6, 4}" << endl;
    cout << "Max Profit: " << bestTimeToBuyAndSellStock(prices1) << endl; // Expected: 5
    cout << "--------------------------" << endl;

    // Example 2: Decreasing prices
    vector<int> prices2 = {7, 6, 4, 3, 1};
    cout << "Test Case 2: {7, 6, 4, 3, 1}" << endl;
    cout << "Max Profit: " << bestTimeToBuyAndSellStock(prices2) << endl; // Expected: 0
    cout << "--------------------------" << endl;

    // Example 3: Empty vector
    vector<int> prices3 = {};
    cout << "Test Case 3: {}" << endl;
    cout << "Max Profit: " << bestTimeToBuyAndSellStock(prices3) << endl; // Expected: 0
    cout << "--------------------------" << endl;

    // Example 4: Single element
    vector<int> prices4 = {10};
    cout << "Test Case 4: {10}" << endl;
    cout << "Max Profit: " << bestTimeToBuyAndSellStock(prices4) << endl; // Expected: 0
    cout << "--------------------------" << endl;

    // Example 5: All same price
    vector<int> prices5 = {5, 5, 5, 5};
    cout << "Test Case 5: {5, 5, 5, 5}" << endl;
    cout << "Max Profit: " << bestTimeToBuyAndSellStock(prices5) << endl; // Expected: 0
    cout << "--------------------------" << endl;

    // Example 6: Buy on day 1, sell on day 2
    vector<int> prices6 = {2, 10, 1, 3};
    cout << "Test Case 6: {2, 10, 1, 3}" << endl;
    cout << "Max Profit: " << bestTimeToBuyAndSellStock(prices6) << endl; // Expected: 8
    cout << "--------------------------" << endl;

    return 0;
}