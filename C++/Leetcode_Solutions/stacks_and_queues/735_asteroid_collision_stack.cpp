#include <bits/stdc++.h>
using namespace std;

class Solution
{
public:
    vector<int> asteroidCollision(vector<int> &asteroids)
    {
        stack<int> st; //asteroids in motion

        for (int ast : asteroids)
        {
            bool explode = false; //track whether current asteroid destroyed
            // Collision happens only when:
            // top of stack is moving right (positive)
            // AND current asteroid is moving left (negative)
            while (!st.empty() && st.top() > 0 && ast < 0)
            {
                int x = abs(ast);
                // If current asteroid is bigger, the top one is destroyed
                if (abs(st.top()) < x)
                {
                    st.pop();
                }
                // If both are equal in size, both get destroyed
                else if (abs(st.top()) == x)
                {
                    st.pop();
                    explode = true;
                    break;
                }
                // If the stack's top is bigger, current asteroid is destroyed
                else
                {
                    explode = true;
                    break;
                }
            }
            // Only push the asteroid if it survives
            if (!explode)
            {
                st.push(ast);
            }
        }

        vector<int> result(st.size());
        for (int i = st.size() - 1; i >= 0; i--)
        {
            result[i] = st.top();
            st.pop();
        }
        return result;
    }
};
