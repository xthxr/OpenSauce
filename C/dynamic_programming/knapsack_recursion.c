// Problem: 0/1 Knapsack using recursion
// Concept: Fundamental DP pattern — choose or skip items for maximum value

#include <stdio.h>

int knapSack(int W, int wt[], int val[], int n) {
    if (n == 0 || W == 0)
        return 0;

    if (wt[n - 1] > W)
        return knapSack(W, wt, val, n - 1);

    int include = val[n - 1] + knapSack(W - wt[n - 1], wt, val, n - 1);
    int exclude = knapSack(W, wt, val, n - 1);

    return include > exclude ? include : exclude;
}

int main() {
    int n, W;
    printf("Enter number of items: ");
    scanf("%d", &n);
    int val[n], wt[n];
    printf("Enter values of items: ");
    for (int i = 0; i < n; i++) scanf("%d", &val[i]);
    printf("Enter weights of items: ");
    for (int i = 0; i < n; i++) scanf("%d", &wt[i]);
    printf("Enter capacity of knapsack: ");
    scanf("%d", &W);

    printf("Maximum value = %d\n", knapSack(W, wt, val, n));
    return 0;
}
