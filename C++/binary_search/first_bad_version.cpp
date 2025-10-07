// Problem:
// First Bad Version (LeetCode 278)
// You are a product manager and currently leading a team to develop a new product. Unfortunately, the latest version of your product fails the quality check. Since each version is developed based on the previous version, all the versions after a bad version are also bad.

// Suppose you have n versions [1, 2, ..., n] and you want to find out the first bad one, which causes all the following ones to be bad.

// You are given an API bool isBadVersion(version) which returns whether version is bad. Implement a function to find the first bad version. You should minimize the number of calls to the API.

// Example 1:
// Input: n = 5, bad = 4
// Output: 4
// Explanation:
// call isBadVersion(3) -> false
// call isBadVersion(5) -> true
// call isBadVersion(4) -> true
// Then 4 is the first bad version.

// Example 2:
// Input: n = 1, bad = 1
// Output: 1

// Constraints:
// 1 <= bad <= n <= 2^31 - 1

#include <bits/stdc++.h>
using namespace std;

class Solution
{
public:
    // The API isBadVersion is defined for you.
    // bool isBadVersion(int version);
    bool isBadVersion(int num)
    {
        if (num >= 4) return true;
        return false;
    }
    // Brute force solution
    // Time Complexity: O(n))
    int firstBadVersionBrute(int n)
    {
        int low = 1, high = n;
        for (int i = low; i <= high; i++)
        {
            if (isBadVersion(i))return i;
        }
        return -1;
    }
    // Optimal solution
    // Time Complexity: O(log((n))
    int firstBadVersionOptimal(int n)
    {
        int low = 1, high = n;

        while (low <= high)
        {
            int mid = low + (high - low) / 2;

            if (isBadVersion(mid)) high = mid - 1;
            else low = mid + 1;
        }
        return low;
    }
};
int main()
{
    Solution s;
    cout << "first Bad Version : " << s.firstBadVersionBrute(5) << endl;
    cout << "first Bad Version : " << s.firstBadVersionOptimal(5) << endl;
    return 0;
}
