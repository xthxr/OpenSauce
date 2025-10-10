/*
Problem: Reverse a Stack using Recursion
Topic: recursion, stack
Description:
Given a stack of integers, reverse it using only recursion. You cannot use 
any loop or extra stack data structure; you can only use the recursion stack.

Notes:

- Use recursion to remove elements one by one, then insert them at the bottom 
  of the stack to reverse the order.
- Base case: empty stack or single element stack.

Time Complexity: O(n^2) // due to inserting elements at the bottom
Space Complexity: O(n)  // recursion stack
*/

/*
Algorithm intuition (short):

1. Base case: if stack is empty, return.
2. Pop the top element and store it.
3. Recursively reverse the remaining stack.
4. Insert the popped element at the bottom of the stack using a helper recursive function.
*/


#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    // Reverse the stack using recursion
    static void reverseStack(stack<int>& st) {
        if (st.empty()) return;
        
        int top = st.top();
        st.pop();
        
        // Recursively reverse the remaining stack
        reverseStack(st);
        
        // Insert the popped element at the bottom
        insertAtBottom(st, top);
    }

private:
    // Helper function to insert element at the bottom of the stack
    static void insertAtBottom(stack<int>& st, int x) {
        if (st.empty()) {
            st.push(x);
            return;
        }
        
        int top = st.top();
        st.pop();
        insertAtBottom(st, x);
        st.push(top);
    }
};

/*
Example usage:

int main() {
    stack<int> st;
    st.push(1);
    st.push(2);
    st.push(3);
    st.push(4);

    cout << "Original stack (top to bottom): ";
    stack<int> temp = st;
    while (!temp.empty()) {
        cout << temp.top() << " ";
        temp.pop();
    }
    cout << endl;

    Solution::reverseStack(st);

    cout << "Reversed stack (top to bottom): ";
    while (!st.empty()) {
        cout << st.top() << " ";
        st.pop();
    }
    cout << endl;

    return 0;
}
*/
