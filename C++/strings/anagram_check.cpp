/*
Problem: Check if Two Strings are Anagrams
---------------------------------------------
Two strings are called anagrams if one string can be formed by rearranging 
the characters of the other, using all the original letters exactly once.

Example:
Input: s1 = "listen", s2 = "silent"
Output: true

Input: s1 = "hello", s2 = "world"
Output: false

Approach:
- Sort both strings and compare them.
OR
- Use frequency counting with a hash map or fixed array (O(n)).

Time Complexity: O(n)       // using frequency count
Space Complexity: O(1)      // constant space (26 letters)
*/

#include <bits/stdc++.h>
using namespace std;

bool isAnagram(string s, string t) {
    if (s.size() != t.size())
        return false;

    vector<int> count(26, 0);
    for (int i = 0; i < s.size(); i++) {
        count[s[i] - 'a']++;
        count[t[i] - 'a']--;
    }

    for (int val : count) {
        if (val != 0)
            return false;
    }
    return true;
}

int main() {
    string s1 = "listen";
    string s2 = "silent";
    string s3 = "hello";
    string s4 = "world";

    cout << (isAnagram(s1, s2) ? "true" : "false") << endl;  // Output: true
    cout << (isAnagram(s3, s4) ? "true" : "false") << endl;  // Output: false

    return 0;
}
