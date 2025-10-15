/*
Problem: Valid Parentheses
------------------------------------
Given a string s containing just the characters '(', ')', '{', '}', '[' and ']',
determine if the input string is valid.

A string is valid if:
1. Open brackets are closed by the same type of brackets.
2. Open brackets are closed in the correct order.

Example:
Input: s = "()[{}]"
Output: true

Input: s = "([)]"
Output: false

Time Complexity: O(n)
Space Complexity: O(n)
*/

#include <bits/stdc++.h>
using namespace std;

bool isValid(string s) {
    stack<char> st;
    unordered_map<char, char> match = {{')','('}, {']','['}, {'}','{'}};

    for (char c : s) {
        if (c == '(' || c == '[' || c == '{') {
            st.push(c);
        } else {
            if (st.empty() || st.top() != match[c])
                return false;
            st.pop();
        }
    }
    return st.empty();
}

int main() {
    string s1 = "()[{}]";
    string s2 = "([)]";

    cout << (isValid(s1) ? "true" : "false") << endl;  // Output: true
    cout << (isValid(s2) ? "true" : "false") << endl;  // Output: false

    return 0;
}
