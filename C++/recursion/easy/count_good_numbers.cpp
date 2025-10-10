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
