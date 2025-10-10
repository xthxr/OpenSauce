/*
Problem: Count Good Numbers (Leetcode 1922)
Topic: combinatorics, modular arithmetic, fast exponentiation
Description:
A number is "good" if digits at even indices are even and digits at odd indices are prime digits (2, 3, 5, 7). 
Given an integer n, return the number of good numbers of length n modulo 10^9 + 7.

Notes:

- Positions are 0-indexed: index 0, 2, 4,... should have even digits (0,2,4,6,8)
- Odd indices: 1,3,5,... should have prime digits (2,3,5,7)
- Use combinatorics and fast exponentiation to compute large powers efficiently.

Time Complexity: O(log n) // using fast exponentiation
Space Complexity: O(1)
*/

/*
Algorithm intuition (short):

1. For positions at even indices: 5 choices (0,2,4,6,8)
2. For positions at odd indices: 4 choices (2,3,5,7)
3. Total good numbers = (5^(n_even) * 4^(n_odd)) % MOD
4. Use fast exponentiation (modular) to compute large powers efficiently
*/


#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    static const int MOD = 1e9 + 7;

    static long long countGoodNumbers(long long n) {
        long long evenPositions = (n + 1) / 2; // 0,2,4,... (ceil(n/2))
        long long oddPositions = n / 2;        // 1,3,5,... (floor(n/2))
        
        long long result = modPow(5, evenPositions) * modPow(4, oddPositions) % MOD;
        return result;
    }

private:
    // Fast modular exponentiation
    static long long modPow(long long base, long long exp) {
        long long res = 1;
        base %= MOD;
        while (exp > 0) {
            if (exp % 2 == 1)
                res = res * base % MOD;
            base = base * base % MOD;
            exp /= 2;
        }
        return res;
    }
};

/*
Example usage:

int main() {
    cout << "n=1 -> " << Solution::countGoodNumbers(1) << endl; // Expected 5
    cout << "n=2 -> " << Solution::countGoodNumbers(2) << endl; // Expected 20
    cout << "n=4 -> " << Solution::countGoodNumbers(4) << endl; // Expected 400
    return 0;
}
*/
