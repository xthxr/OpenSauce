// Problem: Count number of paths in an m x n grid (from top-left to bottom-right)
// Concept: Classic recursion leading to 2D DP (overlapping subproblems)

#include <stdio.h>

int countPaths(int m, int n) {
    if (m == 1 || n == 1)
        return 1;
    return countPaths(m - 1, n) + countPaths(m, n - 1);
}

int main() {
    int m, n;
    printf("Enter grid size (m n): ");
    scanf("%d %d", &m, &n);
    printf("Number of paths in %dx%d grid = %d\n", m, n, countPaths(m, n));
    return 0;
}
