import java.util.Scanner;

public class NonSumOfTwoPrimes {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int T = sc.nextInt(); // Number of test cases
        
        // Process each test case
        for (int t = 0; t < T; t++) {
            int N = sc.nextInt(); // Input N for current test case
            System.out.println(countNonSumOfTwoPrimes(N));
        }
        
        sc.close();
    }
    
    public static long countNonSumOfTwoPrimes(int N) {
        if (N < 1) return 0; // No positive numbers if N < 1
        
        // Step 1: Use Sieve of Eratosthenes to mark prime numbers
        boolean[] isPrime = new boolean[N + 1];
        for (int i = 2; i <= N; i++) {
            isPrime[i] = true; // Initially assume all numbers >= 2 are prime
        }
        
        // Mark non-prime numbers
        for (int i = 2; i * i <= N; i++) {
            if (isPrime[i]) {
                for (int j = i * i; j <= N; j += i) {
                    isPrime[j] = false; // Mark multiples as non-prime
                }
            }
        }
        
        // Step 2: Count numbers that cannot be expressed as sum of two primes
        long count = 0;
        for (int i = 1; i <= N; i++) {
            if (!canBeSumOfTwoPrimes(i, isPrime)) {
                count++;
            }
        }
        
        return count;
    }
    
    public static boolean canBeSumOfTwoPrimes(int num, boolean[] isPrime) {
        // Numbers less than 2 cannot be sum of two primes
        if (num < 2) return false;
        
        // Check if num can be expressed as sum of two primes
        for (int i = 2; i <= num / 2; i++) {
            if (isPrime[i] && isPrime[num - i]) {
                return true; // Found a pair of primes summing to num
            }
        }
        
        return false;
    }
}