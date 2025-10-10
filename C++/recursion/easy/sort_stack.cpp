/*
Problem 1: Reverse a Stack using Recursion
Problem 2: Sort a Stack using Recursion
Topic: recursion, stack
Description:
1. Reverse a stack using only recursion.
2. Sort a stack in ascending order using only recursion.
No loops or extra data structures are allowed; only recursion is used.

Time Complexity:
- Reverse: O(n)
- Sort: O(n^2)
Space Complexity:
- Both: O(n) for recursion stack
*/

#include <bits/stdc++.h>
using namespace std;

class StackUtils {
public:
    // ---------------------- Reverse Stack ----------------------
    static void reverseStack(stack<int>& st) {
        if (st.empty()) return;
        int top = st.top();
        st.pop();
        reverseStack(st);
        insertAtBottom(st, top);
    }

private:
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

    // ---------------------- Sort Stack ----------------------
public:
    static void sortStack(stack<int>& st) {
        if (st.empty()) return;
        int top = st.top();
        st.pop();
        sortStack(st);
        insertSorted(st, top);
    }

private:
    static void insertSorted(stack<int>& st, int x) {
        if (st.empty() || x > st.top()) {
            st.push(x);
            return;
        }
        int top = st.top();
        st.pop();
        insertSorted(st, x);
        st.push(top);
    }
};

// ---------------------- Example Usage ----------------------
int main() {
    stack<int> st1;
    st1.push(1);
    st1.push(2);
    st1.push(3);
    st1.push(4);

    cout << "Original Stack 1 (top to bottom): ";
    stack<int> temp1 = st1;
    while (!temp1.empty()) {
        cout << temp1.top() << " ";
        temp1.pop();
    }
    cout << endl;

    StackUtils::reverseStack(st1);
    cout << "Reversed Stack 1 (top to bottom): ";
    while (!st1.empty()) {
        cout << st1.top() << " ";
        st1.pop();
    }
    cout << endl;

    stack<int> st2;
    st2.push(3);
    st2.push(1);
    st2.push(4);
    st2.push(2);

    cout << "\nOriginal Stack 2 (top to bottom): ";
    stack<int> temp2 = st2;
    while (!temp2.empty()) {
        cout << temp2.top() << " ";
        temp2.pop();
    }
    cout << endl;

    StackUtils::sortStack(st2);
    cout << "Sorted Stack 2 (top to bottom): ";
    while (!st2.empty()) {
        cout << st2.top() << " ";
        st2.pop();
    }
    cout << endl;

    return 0;
}
