// Problem: Find LCS length of two strings using recursion
// Concept: One of the key recursive → DP transitions in algorithmic learning

#include <stdio.h>
#include <string.h>

int LCS(char X[], char Y[], int m, int n) {
    if (m == 0 || n == 0)
        return 0;
    if (X[m - 1] == Y[n - 1])
        return 1 + LCS(X, Y, m - 1, n - 1);
    else
        return (LCS(X, Y, m, n - 1) > LCS(X, Y, m - 1, n)) ? 
                LCS(X, Y, m, n - 1) : LCS(X, Y, m - 1, n);
}

int main() {
    char X[100], Y[100];
    printf("Enter first string: ");
    scanf("%s", X);
    printf("Enter second string: ");
    scanf("%s", Y);
    printf("Length of LCS = %d\n", LCS(X, Y, strlen(X), strlen(Y)));
    return 0;
}
