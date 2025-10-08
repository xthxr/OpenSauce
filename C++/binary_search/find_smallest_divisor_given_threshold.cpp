// Problem:
// Find the Smallest Divisor Given a Threshold (LeetCode 1283)
// Given an array of integers nums and an integer threshold, we will choose a positive integer divisor, divide all the array by it, and sum the division's result. Find the smallest divisor such that the result mentioned above is less than or equal to threshold.
// Each result of the division is rounded to the nearest integer greater than or equal to that element. (For example: 7/3 = 3 and 10/2 = 5).
// The test cases are generated so that there will be an answer.

// Example 1:
// Input: nums = [1,2,5,9], threshold = 6
// Output: 5
// Explanation: We can get a sum to 17 (1+2+5+9) if the divisor is 1.
// If the divisor is 4 we can get a sum of 7 (1+1+2+3) and if the divisor is 5 the sum will be 5 (1+1+1+2).

// Example 2:
// Input: nums = [44,22,33,11,1], threshold = 5
// Output: 44

// Constraints:
// 1 <= nums.length <= 5 * 10^4
// 1 <= nums[i] <= 106
// nums.length <= threshold <= 10^6

#include <iostream>
#include <vector>
#include <cmath>        
#include <algorithm>    
using namespace std;

class Solution
{
public:
    bool Divisor(vector<int> &nums, int threshold, int div)
    {
        int sum = 0;

        for (auto num : nums)
        {
            sum += ceil((double)num / (double)div);
        }
        return sum <= threshold; 
    }
    /*
    Time Complexity: O(n * max(nums))
        - Outer loop runs from 1 to max(nums): O(max(nums))
        - For each i, we call Divisor(), which loops through nums: O(n)
        - So total time = O(n * max(nums))

    Limitations:
        - This approach becomes very slow when max(nums) is large (e.g., > 10^5)
        - Not suitable for large input sizes due to performance issues
        - Use only for small datasets or debugging purposes
    */
    int smallestDivisorBrute(vector<int> &nums, int threshold)
    {
        int low = 1;
        int high = *max_element(nums.begin(), nums.end());

        for (int i = low; i <= high; i++)
        {
            if (Divisor(nums, threshold, i))
            {
                return i;
            }
        }
        return -1;
    }
    // Time Complexity: O(n × log(max(nums)))
    int smallestDivisorOptimal(vector<int> &nums, int threshold)
    {
        int low = 1;
        int high = *max_element(nums.begin(), nums.end());
        int ans = -1;

        while (low <= high)
        {
            int mid = (low + high) / 2;

            if (Divisor(nums, threshold, mid))
            {
                ans = mid;
                high = mid - 1;
            }
            else
            {
                low = mid + 1;
            }
        }
        return ans;
    }
};

int main()
{
    Solution s;
    vector<int> nums = {1, 2, 5, 9};
    int threshold = 6;

    cout << "smallest Divisor Brute : " << s.smallestDivisorBrute(nums, threshold) << endl;
    cout << "smallest Divisor Optimal : " << s.smallestDivisorOptimal(nums, threshold) << endl;
    return 0;
}
