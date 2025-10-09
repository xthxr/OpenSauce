/*
Trapping Rain Water Problem
Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it can trap after raining. 
For example,
Given [0,1,0,2,1,0,1,3,2,1,2,1], return 6.
*/

#include <bits/stdc++.h>
using namespace std;

int trap(vector<int>& height) {
    int n= height.size();
    vector<int>pre(n);
    vector<int>suf(n);
    pre[0]= height[0];
    for(int i= 1; i<n; i++)
    pre[i]= max(pre[i-1],height[i]);
    suf[n-1]= height[n-1];
    for(int i= n-2; i>=0; i--)
    suf[i]= max(suf[i+1],height[i]);
    int water= 0;
    for(int i = 0; i<n; i++){
        water+= min(pre[i],suf[i])-height[i];
    }
    return water;
}

int main() {
    vector<int> height = {0,1,0,2,1,0,1,3,2,1,2,1};
    cout << "Trapped rainwater: " << trap(height) << endl;
    return 0;
}