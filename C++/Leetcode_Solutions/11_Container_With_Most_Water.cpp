class Solution {
public:
    int maxArea(vector<int>& height) {
        int n = height.size();
        int l = 0, r = n-1;
        int ans = 0;
        while (l <= r) {
            int now = 0;
            
            if (height[l] <= height[r]) {
                now = height[l] * (r-l);
                l++; 
            } else {
                now = height[r] * (r-l);
                r--;
            }
            ans = max(ans, now);
        }
        return ans;
    }
};
