/*
Problem: Trapping Rain Water
Topic: Arrays, Two-Pointer
Description:
You are given an array `height` representing the elevation map where the width of each bar is 1.
Calculate how much water it is able to trap after raining.

Example:
Input: height = [0,1,0,2,1,0,1,3,2,1,2,1]
Output: 6
Explanation: 6 units of water can be trapped between the bars.

Constraints:
- 1 <= height.length <= 10^5
- 0 <= height[i] <= 10^5

Time Complexity: O(n)
Space Complexity: O(1)
*/

/*
Algorithm intuition (short):
- Water trapped at each bar depends on the maximum height to its left and right.
- Use two pointers: left and right, with leftMax and rightMax to track the highest walls.
- Move the pointer with smaller height:
    - If current height < leftMax/rightMax, add trapped water: leftMax/rightMax - height
    - Otherwise, update leftMax/rightMax
- Continue until pointers meet. Accumulate water along the way.
*/

#include <bits/stdc++.h>
using namespace std;

class TrappingRainWater {
public:
    int trap(vector<int>& height) {
        if (height.empty()) return 0;// if no bars the answer is zero

        int left = 0;
        int right = height.size() - 1;
        int leftMax = height[left];
        int rightMax = height[right];
        int water = 0;

        // Always move the pointer with the smaller max height
        // Water trapped at a bar = min(leftMax, rightMax) - currentHeight
        // By moving the smaller side, we ensure correct calculation and O(n) time

        while (left < right) {
            if (leftMax < rightMax) {
                left++;
                leftMax = max(leftMax, height[left]);
                water += leftMax - height[left];
            } else {
                right--;
                rightMax = max(rightMax, height[right]);
                water += rightMax - height[right];
            }
        }

        return water;
    }
};

