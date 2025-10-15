#include <bits/stdc++.h>
using namespace std;
class Solution
{
public:
    vector<int> nextGreaterElements(vector<int> &nums)
    {
        stack<int> st; // Stack to store candidate "next greater" value
        vector<int> ans(nums.size(), -1); // Initialize result with -1 for all elements
        for (int i = 2 * nums.size() - 1; i >= 0; i--)
        {
            //pop all elements in stack that are <= current element
            while (!st.empty() && nums[i % nums.size()] >= st.top())
                st.pop();
            //if stack is not empty, top element is the next greater
            if (!st.empty())
                ans[i % nums.size()] = st.top();
            else
                ans[i % nums.size()] = -1;
            st.push(nums[i % nums.size()]);
        }
        return ans;
    }
};