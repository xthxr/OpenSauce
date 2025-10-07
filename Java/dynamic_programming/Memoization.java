/*
 * Memoization is a technique used in dynamic programming.
 * 
 * It’s a top-down approach where we solve the main problem recursively
 * and store results of subproblems to avoid redundant computations.
 * 
 * Example: Fibonacci sequence using memoization (array version)
 * Time Complexity: O(n)
 * Space Complexity: O(n)
 */

package Java.dynamic_programming;

public class Memoization {

    // Array to store computed Fibonacci values
    private static int[] memo;

    public static int fib(int n) {
        // Base case
        if (n <= 1) {
            return n;
        }

        // If result already computed, return it
        if (memo[n] != -1) {
            return memo[n];
        }

        // Otherwise, compute and store it
        memo[n] = fib(n - 1) + fib(n - 2);
        return memo[n];
    }

    public static void main(String[] args) {
        int n = 55;

        // Initialize memo array with -1 (indicates "not computed")
        memo = new int[n + 1];
        for (int i = 0; i <= n; i++) {
            memo[i] = -1;
        }

        System.out.println("Fibonacci sequence using the memoization algorithm:")
        System.out.println("fib(" + n + ") = " + fib(n));
    }
}
