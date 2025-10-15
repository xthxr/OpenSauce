#include <bits/stdc++.h>
using namespace std;
//Queue is FIFO - first in first out
class MyQueue
{
    stack<int> que;
    stack<int> que1;

public:
    MyQueue() {}
    void push(int x)
    {
        que.push(x);
    } //add element to the front
    int pop()
    {
        if (que1.empty())
        {
            while (!que.empty())
            {
                que1.push(que.top());
                que.pop();
            }
        }

        int popped = que1.top();
        que1.pop();
        return popped;
    } //remove element from the back
    int peek()
    {
        if (que1.empty())
        {
            while (!que.empty())
            {
                que1.push(que.top());
                que.pop();
            }
        }
        return que1.top();
    } //return top i.e. front element
    bool empty()
    {
        return que.empty() && que1.empty();
    } //return if "queue" empty
};