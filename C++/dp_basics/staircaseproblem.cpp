#include <iostream>
using namespace std;

int main() {
    int n;
    cin >> n; // Number of stairs
    int dp[100];

    dp[0] = 1; // 1 way to stay on ground
    dp[1] = 1; // 1 way to reach first stair

    for(int i = 2; i <= n; i++) {
        dp[i] = dp[i-1] + dp[i-2]; // 1 step or 2 steps
    }

    cout << dp[n] << endl; // Total ways to reach n-th stair
    return 0;
}
