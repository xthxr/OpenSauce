#include <bits/stdc++.h>
using namespace std;
class Solution
{
public:
    vector<int> maxSlidingWindow(vector<int> &nums, int k)
    {
        deque<int> dq; //useful indices
        vector<int> ans; //stores max element for each window
        for (int i = 0; i < nums.size(); i++)
        {
            if (!dq.empty() && dq.front() <= i - k)
            {
                dq.pop_front();
            }//remove indices that are out of the current window
            while (!dq.empty() && nums[dq.back()] <= nums[i])
            {
                dq.pop_back();
            }
            // Remove elements smaller than current because they can't be future max
            dq.push_back(i); //add current index
            if (i >= k - 1)
            {
                ans.push_back(nums[dq.front()]);
            }//record max
        }
        return ans;
    }
};