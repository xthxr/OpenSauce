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
