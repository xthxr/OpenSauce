// Problem:
// Koko Eating Bananas (LeetCode 875)

// Koko loves to eat bananas. There are n piles of bananas, the ith pile has piles[i] bananas. The guards have gone and will come back in h hours.

// Koko can decide her bananas-per-hour eating speed of k. Each hour, she chooses some pile of bananas and eats k bananas from that pile. If the pile has less than k bananas, she eats all of them instead and will not eat any more bananas during this hour.

// Koko likes to eat slowly but still wants to finish eating all the bananas before the guards return.

// Return the minimum integer k such that she can eat all the bananas within h hours.

// Example 1:
// Input: piles = [3,6,7,11], h = 8
// Output: 4

// Example 2:
// Input: piles = [30,11,23,4,20], h = 5
// Output: 30

// Example 3:
// Input: piles = [30,11,23,4,20], h = 6
// Output: 23

// Constraints:
// 1 <= piles.length <= 10^4
// piles.length <= h <= 10^9
// 1 <= piles[i] <= 10^9

#include <bits/stdc++.h>
using namespace std;

class Solution
{
public:
    // Optimal solution
    // Time Complexity (TC)
    // Let:
    // n = number of piles
    // m = maximum number of bananas in any pile (*max_element(piles.begin(), piles.end()))

    // Then:
    // Binary search runs for log₂(m) iterations.
    // Each iteration calls EatingSpeed, which is O(n).
    // So the total time complexity is:
    // O(n · log m)
    long long EatingSpeed(vector<int> &piles, int hourly)
    {
        long long totalHour = 0;

        for (auto pile : piles)
        {
            totalHour += ceil((double)pile / (double)hourly);
        }

        return totalHour;
    }
    int minEatingSpeed(vector<int> &piles, int h)
    {
        int low = 1;
        int high = *max_element(piles.begin(), piles.end());

        while (low <= high)
        {
            int mid = (low + high) / 2;

            if ((EatingSpeed(piles, mid)) <= h)
            {

                high = mid - 1;
            }
            else
            {
                low = mid + 1;
            }
        }

        return low;
    }
};
int main()
{
    Solution s;
    vector<int> piles = {805306368, 805306368, 805306368};
    int h = 1000000000;

    cout << "Koko Eating Bananas : " << s.minEatingSpeed(piles, h) << endl;
    return 0;
}