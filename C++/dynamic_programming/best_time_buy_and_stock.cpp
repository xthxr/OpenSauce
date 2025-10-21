#include <bits/stdc++.h>
using namespace std;

/**
 * Calculates the maximum profit from buying and selling a stock once.
 *
 * @param prices Vector of stock prices where prices[i] is the price on day i.
 * @return Maximum profit achievable. Returns 0 if no profit is possible.
 *
 * Time Complexity:
 *    O(n) - We iterate through the prices vector once, where n is the number of days.
 * Space Complexity:
 *    O(1) - Only two extra variables (minPrice and maxProfit) are used.
 */
int maxProfit(vector<int>& prices) {
    if (prices.size() < 2) return 0;

    int minPrice = INT_MAX;   // Minimum price seen so far
    int maxProfit = 0;        // Maximum profit achieved

    for (int price : prices) {
        minPrice = min(minPrice, price);          // Update min price
        maxProfit = max(maxProfit, price - minPrice);  // Update max profit
    }

    return maxProfit;
}

// -------------------------------
// Test cases
// -------------------------------
int main() {
    vector<int> prices1 = {7, 1, 5, 3, 6, 4};
    cout << maxProfit(prices1) << endl; // Expected output: 5

    vector<int> prices2 = {7, 6, 4, 3, 1};
    cout << maxProfit(prices2) << endl; // Expected output: 0

    vector<int> prices3 = {};
    cout << maxProfit(prices3) << endl; // Expected output: 0

    vector<int> prices4 = {5};
    cout << maxProfit(prices4) << endl; // Expected output: 0

    return 0;
}
