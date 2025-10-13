// Problem: Find factorial of a number using recursion
// Concept: Basic recursion — the foundation of DP (overlapping subproblems begin here)

#include <stdio.h>

int factorial(int n) {
    if (n == 0 || n == 1)
        return 1;
    return n * factorial(n - 1);
}

int main() {
    int n;
    printf("Enter a number: ");
    scanf("%d", &n);
    printf("Factorial of %d = %d\n", n, factorial(n));
    return 0;
}
