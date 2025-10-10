# 149. Max Points on a Line

## Problem Description

Given an array of `points` where `points[i] = [xi, yi]` represents a point on the 2D plane,  
return the **maximum number of points that lie on the same straight line**.

---

### Examples

#### Example 1
**Input:**  
`points = [[1,1],[2,2],[3,3]]`  
**Output:**  
`3`  
**Explanation:**  
All three points lie on the same line y = x.

#### Example 2
**Input:**  
`points = [[1,1],[3,2],[5,3],[4,1],[2,3],[1,4]]`  
**Output:**  
`4`  
**Explanation:**  
The line passing through points `(1,1)`, `(2,3)`, `(3,2)`, and `(5,3)` includes 4 points.

---

### Constraints

- `1 <= points.length <= 300`
- `points[i].length == 2`
- `-10^4 <= xi, yi <= 10^4`
- All the points are unique.

---

## Solution Approach & Intuition

This problem is a **geometry + hashing** problem that asks for the maximum number of collinear points.

### 🧠 Intuition
Any two points uniquely define a line.  
Hence, if we fix one point and compute the **slope** with respect to all other points,  
all points that have the same slope relative to this fixed point lie on the same straight line.

We can count how many such points exist for each slope.

---

## Approach: Hashing by Slope

### Key Insight
- For each point `i`, calculate the slope it makes with every other point `j`.
- Points with the **same slope** relative to point `i` lie on the same line.
- Keep track of the maximum number of points that share a slope.
- Repeat this for each point and take the global maximum.

---

### Steps

1. **Initialize maxPoints = 1** (since at least one point exists)
2. For each point `i`:
   - Create a hashmap `slopeCount` to store slope frequencies.
   - Initialize `overlap = 0` (for identical points) and `currMax = 0`.
3. For every other point `j`:
   - If `points[i] == points[j]`, increment `overlap`.
   - Else compute the **reduced slope** as a rational fraction:
     ```
     dx = xj - xi
     dy = yj - yi
     gcd = __gcd(dx, dy)
     dx /= gcd
     dy /= gcd
     ```
     Use a pair `(dx, dy)` as the slope key.
   - Increment `slopeCount[{dx, dy}]`
   - Update `currMax = max(currMax, slopeCount[{dx, dy}])`
4. Update `maxPoints = max(maxPoints, currMax + overlap + 1)`
5. Return `maxPoints`.

---

## Handling Edge Cases

- **Duplicate points:** count separately with `overlap`.
- **Vertical lines:** handled naturally since `dx = 0`.
- **Floating-point precision:** avoid using double; use integer pairs `(dy/gcd, dx/gcd)` to store slopes accurately.

---

## Time & Space Complexity

- **Time Complexity:** `O(N²)`  
  Each pair of points is considered once.

- **Space Complexity:** `O(N)`  
  The hashmap stores slopes for each iteration.

---

## Implementation (C++)

```cpp
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