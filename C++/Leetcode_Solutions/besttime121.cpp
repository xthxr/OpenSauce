#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

class Solution {
public:
    int maxProfit(vector<int>& prices) {
        if (prices.empty()) return 0; 

        int maxprofit = 0;       
        int bestbuy = prices[0]; 

        for (int i = 1; i < prices.size(); i++) {
            maxprofit = max(maxprofit, prices[i] - bestbuy);
            bestbuy = min(bestbuy, prices[i]);
        }

        return maxprofit;
    }
};

int main() {
    Solution sol;
    vector<int> prices;

    int n;
    cout << "Enter number of days: ";
    cin >> n;

    cout << "Enter stock prices: ";
    for (int i = 0; i < n; i++) {
        int price;
        cin >> price;
        prices.push_back(price);
    }

    int result = sol.maxProfit(prices);
    cout << "Maximum profit: " << result << endl;

    return 0;
}
