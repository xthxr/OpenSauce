// Problem: Count number of digits in a number using recursion
// Concept: Base case and reduction — core recursion pattern

#include <stdio.h>

int countDigits(int n) {
    if (n == 0) return 0;
    return 1 + countDigits(n / 10);
}

int main() {
    int n;
    printf("Enter a number: ");
    scanf("%d", &n);
    printf("Number of digits in %d = %d\n", n, countDigits(n));
    return 0;
}
