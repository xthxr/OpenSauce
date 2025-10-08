// Problem:
//  Find Smallest Letter Greater Than Target (LeetCode 744)
// You are given an array of characters letters that is sorted in non-decreasing order, and a character target. There are at least two different characters in letters.

// Return the smallest character in letters that is lexicographically greater than target. If such a character does not exist, return the first character in letters.

// Example 1:
// Input: letters = ["c","f","j"], target = "a"
// Output: "c"
// Explanation: The smallest character that is lexicographically greater than 'a' in letters is 'c'.

// Example 2:
// Input: letters = ["c","f","j"], target = "c"
// Output: "f"
// Explanation: The smallest character that is lexicographically greater than 'c' in letters is 'f'.

// Example 3:
// Input: letters = ["x","x","y","y"], target = "z"
// Output: "x"
// Explanation: There are no characters in letters that is lexicographically greater than 'z' so we return letters[0].

// Constraints:
// 2 <= letters.length <= 10^4
// letters[i] is a lowercase English letter.
// letters is sorted in non-decreasing order.
// letters contains at least two different characters.
// target is a lowercase English letter.

#include <bits/stdc++.h>
using namespace std;
class Solution
{
public:
    // Brute force solution
    // Time Complexity: O(letters.size())
    char nextGreatestLetterBrute(vector<char> &letters, char target)
    {
        int low = 0;
        int high = letters.size() - 1;
        int ans = letters[0];
        for (int i = low; i <= high; i++)
        {
            if (letters[i] > target)
            {
                return letters[i];
            }
        }
        return ans;
    }

    // Optimal solution
    // Time Complexity: O(log((letters.size()))
    char nextGreatestLetterOptimal(vector<char> &letters, char target)
    {
        int low = 0;
        int high = letters.size() - 1;
        int ans = letters[0];
        while (low <= high)
        {
            int mid = (low + high) / 2;
            if (letters[mid] > target)
            {
                ans = letters[mid];
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
    vector<char> letters = {'c', 'f', 'j'};
    char target = 'c';
    cout << "nextGreatestLetter : " << s.nextGreatestLetterBrute(letters, target) << endl;
    cout << "nextGreatestLetter : " << s.nextGreatestLetterOptimal(letters, target) << endl;
    return 0;
}