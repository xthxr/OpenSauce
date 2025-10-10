#include <iostream>
using namespace std;

int main() {
    int n;
    cin >> n;     // Input n-th Fibonacci number
    int dp[100]; // Array to store Fibonacci numbers

    dp[0] = 0;
    dp[1] = 1;

    for(int i = 2; i <= n; i++) {
        dp[i] = dp[i-1] + dp[i-2]; // Sum of previous two numbers
    }

    cout << dp[n] << endl;
    return 0;
}
