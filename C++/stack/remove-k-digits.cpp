/*Problem: Lettcode 402. Remove K Digits

Given string num representing a non-negative integer num, and an integer k, return the smallest possible integer after removing k digits from num.

Example 1:
Input: num = "1432219", k = 3
Output: "1219"
Explanation: Remove the three digits 4, 3, and 2 to form the new number 1219 which is the smallest.

Example 2:
Input: num = "10200", k = 1
Output: "200"
Explanation: Remove the leading 1 and the number is 200. Note that the output must not contain leading zeroes.

Example 3:
Input: num = "10", k = 2
Output: "0"
Explanation: Remove all the digits from the number and it is left with nothing which is 0.

*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    string removeKdigits(string nums, int k) {
        
        stack <char> st;
        
        for(int i=0; i < nums.size(); i++) {
        
            char digit = nums[i];
            while(!st.empty() && k > 0
                  && st.top() > digit) {

                st.pop();
                k--;
            }
            st.push(digit);
        }
        
        while(!st.empty() && k > 0) {
            
            st.pop();
            k--;
        }
        
        if(st.empty()) return "0";
        
        string res = "";
        while(!st.empty()) {
            res.push_back(st.top());
            st.pop();
        }
        while(res.size() > 0 && 
              res.back() == '0') {

            res.pop_back();
        }
        
        reverse(res.begin(), res.end());
        if(res.empty()) return "0";
        
        return res;
    }
};
