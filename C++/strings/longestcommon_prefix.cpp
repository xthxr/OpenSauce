/*
Problem: Longest Common Prefix
---------------------------------------------
Given an array of strings, find the longest common prefix 
among all strings. If there is no common prefix, return an empty string.

Example:
Input: ["flower", "flow", "flight"]
Output: "fl"

Input: ["dog", "racecar", "car"]
Output: ""  // No common prefix

Approach:
1️⃣ Sort the array of strings.
2️⃣ Compare the first and last strings only — since sorting arranges them lexicographically,
    their common prefix will be the common prefix of the entire array.

Time Complexity: O(n * m)
  where n = number of strings, m = length of the shortest string
Space Complexity: O(1)
*/

#include <bits/stdc++.h>
using namespace std;

string longestCommonPrefix(vector<string>& strs) {
    if (strs.empty()) return "";

    sort(strs.begin(), strs.end());
    string first = strs[0];
    string last = strs[strs.size() - 1];
    int i = 0;
    
    while (i < first.size() && i < last.size() && first[i] == last[i])
        i++;
        
    return first.substr(0, i);
}

int main() {
    vector<string> strs1 = {"flower", "flow", "flight"};
    vector<string> strs2 = {"dog", "racecar", "car"};

    cout << longestCommonPrefix(strs1) << endl;  // Output: fl
    cout << longestCommonPrefix(strs2) << endl;  // Output: (empty)

    return 0;
}
