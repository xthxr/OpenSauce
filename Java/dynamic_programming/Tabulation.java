/*
 * Tabulation is a technique used in dynamic programming
 * 
 * Tabulation is a bottom-up approach where we solve smaller subproblems first
 * and store their results in a table (usually an array) to avoid redundant
 * computations
 * This technique is particularly useful for problems with overlapping
 * subproblems and optimal substructure properties.
 *
 * Example: Fibonacci sequence using tabulation
 * Time Complexity: O(n)
 * Space Complexity: O(n)
 *
 */
package Java.dynamic_programming;

public class Tabulation {
    public static int Fibonacci(int n) {
        if (n <= 1) {
            return n;
        }

        // Base cases; smallest subproblems
        int[] table = new int[n + 1];
        table[0] = 0;
        table[1] = 1;

        // Fill the table using tabulation
        // table[i] = table[i - 1] + table[i - 2]
        // We are filling in the array from smaller subproblems
        for (int i = 2; i <= n; i++) {
            table[i] = table[i - 1] + table[i - 2];
        }

        // return the nth Fibonacci number
        return table[n];
    }

    public static void main(String[] args) {
        int n = 10; // Example input
        System.out.println("Fibonacci of " + n + " is: " + Fibonacci(n));

        n = 45; // Example input
        System.out.println("Fibonacci of " + n + " is: " + Fibonacci(n));
    }
}
