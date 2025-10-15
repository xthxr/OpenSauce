#include <bits/stdc++.h>
using namespace std;
//Queue is FIFO - first in first out
class MyStack
{
    queue<int> stac;

public:
    MyStack()
    { //initialize 
    }
    void push(int x)
    {
        queue<int> temp, temp2;
        temp2 = stac;
        temp.push(x);
        while (!temp2.empty())
        {
            temp.push(temp2.front());
            temp2.pop();
        }
        stac = temp;
    } //add element at back (instead of first as it would be done in a queue)
    int pop()
    {
        int popped = stac.front();
        stac.pop();
        return popped;
    } //remove first element (common to both)
    int top()
    {
        return stac.front();
    } //return top i.e. first element
    bool empty()
    {
        return stac.empty();
    } //return if empty
};