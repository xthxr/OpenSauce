#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

void MaxWaterTrap(vector<int> heights) {
    int n = heights.size();
    if (n == 0) return;

    int left = 0, right = n - 1;
    int leftMax = 0, rightMax = 0;
    int totalWater = 0;

    while (left < right) {
        if (heights[left] <= heights[right]) {
            // Process left side
            if (heights[left] >= leftMax)
                leftMax = heights[left];
            else
                totalWater += leftMax - heights[left];
            left++;
        } else {
            // Process right side
            if (heights[right] >= rightMax)
                rightMax = heights[right];
            else
                totalWater += rightMax - heights[right];
            right--;
        }
    }

    cout << "Total trapped water: " << totalWater << endl;
}

int main() {
    vector<int> heights = {4, 2, 0, 3, 8, 5, 6};
    MaxWaterTrap(heights);
    return 0;
}
