/*
Problem: Pow(x, n) - Implement Power Function
Topic: recursion, divide-and-conquer, math
Description:
Given a double x and an integer n, implement a function to calculate x raised 
to the power n (i.e., x^n). Return the result as a double.

Notes:

- n may be negative, zero, or positive.
- Use recursion with divide-and-conquer (exponentiation by squaring) to achieve O(log n) time.
- Avoid integer overflow when n = INT_MIN.

Time Complexity: O(log n) // recursive calls halve the exponent each time
Space Complexity: O(log n) // recursion stack
*/

/*
Algorithm intuition (short):

1. Base case: if n == 0, return 1.
2. If n < 0, compute 1 / (x^-n) using long long to handle INT_MIN.
3. Recursively compute half = pow(x, n/2).
4. If n is even, return half * half.
5. If n is odd, return half * half * x.
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    // Public interface to compute x^n
    static double myPow(double x, int n) {
        if (n == 0) return 1.0; // base case
        
        if (n < 0) {
            x = 1 / x;
            // Use long long to prevent overflow when n = INT_MIN
            return fastPow(x, -(long long)n);
        }
        
        return fastPow(x, n);
    }

private:
    // Helper recursive function for positive powers
    static double fastPow(double x, long long n) {
        if (n == 0) return 1.0;
        
        double half = fastPow(x, n / 2);
        if (n % 2 == 0) 
            return half * half;
        else 
            return half * half * x;
    }
};

/*
Example usage:

int main() {
    cout << "2^10 = " << Solution::myPow(2, 10) << endl; // Expected 1024
    cout << "2^-2 = " << Solution::myPow(2, -2) << endl; // Expected 0.25
    cout << "3^5 = " << Solution::myPow(3, 5) << endl;   // Expected 243
    cout << "2^0 = " << Solution::myPow(2, 0) << endl;   // Expected 1
    return 0;
}
*/
