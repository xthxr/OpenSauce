#include <bits/stdc++.h>
using namespace std;
class Solution
{
public:
    vector<int> dailyTemperatures(vector<int> &temperatures)
    {
        int n = temperatures.size(); // Number of days
        stack<int> st;               // Stack to store indices of unresolved days
        vector<int> ans(n, 0);       // Result array initialized to 0

        for (int i = n - 1; i >= 0; i--)
        {
            // Pop all days that are not warmer than the current day
            while (!st.empty() && temperatures[i] >= temperatures[st.top()])
                st.pop();

            // If stack not empty, top holds index of next warmer day
            if (!st.empty())
                ans[i] = st.top() - i;

            // Push current day's index to stack
            st.push(i);
        }
        return ans;
    }
};
