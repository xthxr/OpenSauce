#include <bits/stdc++.h>
using namespace std;
//stack is LIFO - last in first out
class MinStack
{
    vector<int> stac;

public:
    MinStack() {} //initializes the stack
    void push(int val)
    {
        stac.push_back(val); 
    } //adds the last element
    void pop()
    {
        if (!stac.empty())
            stac.pop_back();
    } //removes the last element
    int top()
    {
        if (!stac.empty())
            return stac.back();
        throw std::runtime_error("Stack is empty");
    } //returns last element to be added i.e. top of stack
    int getMin()
    {
        if (stac.empty())
            throw std::runtime_error("Stack is empty");
        int min = stac[0];
        for (int i = 1; i < stac.size(); i++)
        {
            if (stac[i] < min)
                min = stac[i];
        } //gives minimum element in stack
        return min;
    }
};