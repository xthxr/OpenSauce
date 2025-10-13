// Problem: Count number of ways to make change for a value
// Concept: Core recursion used in DP coin change problem

#include <stdio.h>

int countWays(int coins[], int n, int sum) {
    if (sum == 0) return 1;
    if (sum < 0) return 0;
    if (n <= 0 && sum >= 1) return 0;

    return countWays(coins, n - 1, sum) + countWays(coins, n, sum - coins[n - 1]);
}

int main() {
    int n, sum;
    printf("Enter number of coins: ");
    scanf("%d", &n);
    int coins[n];
    printf("Enter coin values: ");
    for (int i = 0; i < n; i++) scanf("%d", &coins[i]);
    printf("Enter total sum: ");
    scanf("%d", &sum);
    printf("Number of ways to make change: %d\n", countWays(coins, n, sum));
    return 0;
}
