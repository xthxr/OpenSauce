/*
Problem: Container With Most Water
Topic: arrays
Description: You are given an integer array height of length n. There are n vertical lines drawn such that the two endpoints of the ith line are (i, 0) and (i, height[i]).
Find two lines that together with the x-axis form a container, such that the container contains the most water.
Return the maximum amount of water a container can store.
Time Complexity: O(n)
Space Complexity: O(1)
*/

/*
Algorithm intuition (short):
- Use two pointers starting at both ends. The area is limited by the shorter line, so move the smaller pointer inward to try to find a taller line and potentially increase area.
- This guarantees O(n) time since each pointer moves at most n steps.
*/

#include <bits/stdc++.h>
using namespace std;


class Solution {
public:
    // Computes area between indices l and r in the height vector.
    // Kept as an inline helper for readability.
    static inline int area(int l, int r, const vector<int>& height) {
    return min(height[l], height[r]) * (r - l);
    }

    // We always move the pointer at the smaller height because the current area
    // is limited by the smaller height; moving the taller one cannot increase
    // the min-height boundary for any wider container.

    static int maxArea(const vector<int>& height) {
    int n = (int)height.size();
    int l = 0, r = n - 1;
    int best = 0;
    while (l < r) {
    // Compute area and update best
    best = max(best, area(l, r, height));
    // Move the pointer pointing to the smaller line inward.
    if (height[l] < height[r]) {
    ++l; // we hope to find a taller left line
    } else {
    --r; // or a taller right line
    }
    }
    return best;
    }
};