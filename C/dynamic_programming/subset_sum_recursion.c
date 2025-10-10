// Problem: Check if there's a subset with given sum
// Concept: Binary recursion → include/exclude decision (core of DP thinking)

#include <stdio.h>

int subsetSum(int arr[], int n, int sum) {
    if (sum == 0) return 1;
    if (n == 0) return 0;

    if (arr[n - 1] > sum)
        return subsetSum(arr, n - 1, sum);
    
    return subsetSum(arr, n - 1, sum) || subsetSum(arr, n - 1, sum - arr[n - 1]);
}

int main() {
    int n, sum;
    printf("Enter number of elements: ");
    scanf("%d", &n);
    int arr[n];
    printf("Enter elements: ");
    for (int i = 0; i < n; i++) scanf("%d", &arr[i]);
    printf("Enter target sum: ");
    scanf("%d", &sum);

    if (subsetSum(arr, n, sum))
        printf("Subset with sum %d exists.\n", sum);
    else
        printf("No subset with sum %d found.\n", sum);
    return 0;
}
