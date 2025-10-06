/*Problem: Find Right Interval(LeetCode 436)
You are given an array of intervals, where intervals[i] = [starti, endi] and each starti is unique.

The right interval for an interval i is an interval j such that startj >= endi and startj is minimized. Note that i may equal j.

Return an array of right interval indices for each interval i. If no right interval exists for interval i, then put -1 at index i.

Example 1:
Input: intervals = [[1,2]]
Output: [-1]
Explanation: There is only one interval in the collection, so it outputs -1.

Example 2:
Input: intervals = [[3,4],[2,3],[1,2]]
Output: [-1,0,1]
Explanation: There is no right interval for [3,4].
The right interval for [2,3] is [3,4] since start0 = 3 is the smallest start that is >= end1 = 3.
The right interval for [1,2] is [2,3] since start1 = 2 is the smallest start that is >= end2 = 2.

Example 3:
Input: intervals = [[1,4],[2,3],[3,4]]
Output: [-1,2,-1]
Explanation: There is no right interval for [1,4] and [3,4].
The right interval for [2,3] is [3,4] since start2 = 3 is the smallest start that is >= end1 = 3.
 

Constraints:

1 <= intervals.length <= 2 * 10^4
intervals[i].length == 2
-106 <= starti <= endi <= 1^6
The start point of each interval is unique.
*/
#include <iostream>
using namespace std;
class Solution {
public:
   vector<int> findRightInterval(vector<vector<int>> &intervals)
{
    vector<int> ans;
    if (intervals.size() == 1)
    {
        return {-1};
    }
    for (int i = 0; i < intervals.size() - 1; i++)
    {
    }
    return ans;
}

};

int main()
{
    Solution s;
    vector<vector<int>> intervals = {{1, 4}, {2, 3}, {3, 4}};
    cout<<"Find Right Interval : "<<endl;
    vector<int> ans = s.findRightInterval(intervals);
    for (auto num : ans)
    {
        cout << num << " ";
    }
    cout << endl;
    return 0;
}