/*Problem: Lettcode 735. Asteroid Collision

We are given an array asteroids of integers representing asteroids in a row. The indices of the asteriod in the array represent their relative position in space.
For each asteroid, the absolute value represents its size, and the sign represents its direction (positive meaning right, negative meaning left). Each asteroid moves at the same speed.
Find out the state of the asteroids after all collisions. If two asteroids meet, the smaller one will explode. If both are the same size, both will explode. Two asteroids moving in the same direction will never meet.

*/

#include <bits/stdc++.h>
using namespace std;
class Solution {
public:
    vector<int> asteroidCollision(vector<int>& asteroids) {
        std::stack<int> stack;

        for (int a : asteroids) {
            if (a > 0) {
                stack.push(a);
            } else {
                while (!stack.empty() && stack.top() > 0 && stack.top() < -a) {
                    stack.pop();
                }

                if (stack.empty() || stack.top() < 0) {
                    stack.push(a);
                }

                if (!stack.empty() && stack.top() == -a) {
                    stack.pop();
                }
            }
        }

        std::vector<int> res(stack.size());
        int i = stack.size() - 1;

        while (!stack.empty()) {
            res[i--] = stack.top();
            stack.pop();
        }

        return res;        
    }
};