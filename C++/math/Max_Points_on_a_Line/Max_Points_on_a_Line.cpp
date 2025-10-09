#include <vector>
#include <unordered_map>
#include <numeric>
#include <string>
#include <algorithm>
using namespace std;

class Solution {
public:
    int maxPoints(vector<vector<int>>& points) {
        int n = points.size();
        if (n <= 2) return n;

        int maxPointsOnLine = 0;

        for (int i = 0; i < n; i++) {
            unordered_map<string, int> slopeCount;
            int overlap = 0, currMax = 0;

            for (int j = i + 1; j < n; j++) {
                int dx = points[j][0] - points[i][0];
                int dy = points[j][1] - points[i][1];

                if (dx == 0 && dy == 0) {
                    overlap++;
                    continue;
                }

                int g = gcd(dx, dy);
                dx /= g;
                dy /= g;

                // Normalize sign
                if (dx < 0) {
                    dx = -dx;
                    dy = -dy;
                } else if (dx == 0) {
                    dy = 1;
                } else if (dy == 0) {
                    dx = 1;
                }

                string key = to_string(dx) + "/" + to_string(dy);
                slopeCount[key]++;
                currMax = max(currMax, slopeCount[key]);
            }

            maxPointsOnLine = max(maxPointsOnLine, currMax + overlap + 1);
        }

        return maxPointsOnLine;
    }
};
