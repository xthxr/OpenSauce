#include <bits/stdc++.h>
using namespace std;
class Solution
{
public:
    string removeKdigits(string num, int k)
    {
        stack<char> st;
        for (char c : num)
        {
            while (!st.empty() && k > 0 && st.top() > c)
            {

                st.pop();
                k--;
            }
            st.push(c);
        }
        while (k > 0 && !st.empty())
        {
            st.pop();
            k--;
        }
        string result;
        while (!st.empty())
        {
            result.push_back(st.top());
            st.pop();
        }
        while (result.size() != 0 && result.back() == '0')
            result.pop_back();
        reverse(result.begin(), result.end());
        if (result.empty())
            return "0";
        else
            return result;
    }
};