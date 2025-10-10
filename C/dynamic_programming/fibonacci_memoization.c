// Fibonacci using recursion + memoization (Dynamic Programming base)

#include <stdio.h>

#define MAX 100
int dp[MAX];

int fib(int n) {
    if (n <= 1) return n;
    if (dp[n] != -1) return dp[n];
    dp[n] = fib(n - 1) + fib(n - 2);
    return dp[n];
}

int main() {
    int n;
    printf("Enter n: ");
    scanf("%d", &n);
    for (int i = 0; i < MAX; i++) dp[i] = -1;
    printf("Fibonacci(%d) = %d\n", n, fib(n));
    return 0;
}
