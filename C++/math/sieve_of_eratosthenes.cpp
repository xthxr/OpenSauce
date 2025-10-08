#include <iostream>
#include <vector>
using namespace std;

/**
Function: sieveOfEratosthenes
Returns a vector of all prime numbers <= n using the Sieve of Eratosthenes algorithm.
*/
vector<int> sieveOfEratosthenes(int n) {
    // Step 1: Create a boolean array "isPrime[0..n]" and initialize all entries as true.
    // A value in isPrime[i] will be false if i is not a prime, true otherwise.
    vector<bool> isPrime(n + 1, true);
    isPrime[0] = isPrime[1] = false; // 0 and 1 are not primes.

    // Step 2: Start marking multiples of each prime.
    for (int p = 2; p * p <= n; p++) {
        // If isPrime[p] is true, it is a prime.
        if (isPrime[p]) {
            // Mark all multiples of p as false (not prime).
            // Start from p*p (smaller multiples already marked by smaller primes).
            for (int multiple = p * p; multiple <= n; multiple += p) {
                isPrime[multiple] = false;
            }
        }
    }

    // Step 3: Collect all prime numbers into a result vector.
    vector<int> primes;
    for (int i = 2; i <= n; i++) {
        if (isPrime[i])
            primes.push_back(i);
    }

    return primes;
}

int main() {
    int n;
    cout << "Enter the upper limit (n): ";
    cin >> n;
    
    vector<int> primes = sieveOfEratosthenes(n);
    cout << "Prime numbers up to " << n << " are:\n";
    for (int prime : primes) {
        cout << prime << " ";
    }
    cout << endl;
    return 0;
}
