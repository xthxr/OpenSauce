/*
Problem: Palindrome String
------------------------------------
Given a string s, check if it reads the same backward as forward.

Example:
Input: s = "madam"
Output: true

Input: s = "hello"
Output: false

Time Complexity: O(n)
Space Complexity: O(1)
*/

#include <bits/stdc++.h>
using namespace std;

bool isPalindrome(string s) {
    int left = 0, right = s.size() - 1;
    while (left < right) {
        if (s[left] != s[right])
            return false;
        left++;
        right--;
    }
    return true;
}

int main() {
    string s1 = "madam";
    string s2 = "hello";

    cout << (isPalindrome(s1) ? "true" : "false") << endl;  // Output: true
    cout << (isPalindrome(s2) ? "true" : "false") << endl;  // Output: false

    return 0;
}
