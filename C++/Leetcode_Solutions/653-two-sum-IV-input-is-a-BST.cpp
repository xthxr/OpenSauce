/*
    Problem Statement :- Given a Binary Search Tree (BST) and a target integer `k`, 
    determine if there exist **two elements in the BST** such that their sum equals `k`.

    --------------
    
    Approach :- (Using Two-Pointer Technique with BST Iterators)
    --------------
    1. Use two iterators on the BST:
        a. `left` iterator for the **smallest elements** (in-order traversal).
        b. `right` iterator for the **largest elements** (reverse in-order traversal).
    2. Each iterator uses a stack to traverse the BST lazily:
        - For `left` iterator, push nodes going left.
        - For `right` iterator, push nodes going right.
    3. Initialize `leftVal = left->next()` and `rightVal = right->next()`.
    4. While `leftVal < rightVal`:
        a. If `leftVal + rightVal == k`, return `true`.
        b. If `leftVal + rightVal < k`, move the `left` iterator forward (`leftVal = left->next()`).
        c. If `leftVal + rightVal > k`, move the `right` iterator backward (`rightVal = right->next()`).
    5. If no pair is found, return `false`.

    --------------
    
    Time Complexity :-
    -----------------
    - Each iterator performs O(h) work per `next()` call, where `h` is the height of the tree.
    - Overall, each node is visited at most once → O(n) time.

    --------------
    
    Space Complexity :-
    ------------------
    - Each iterator uses a stack of size O(h) → O(h) space.

    --------------
    
    Example :-
    ----------
    Input :-
    --------
        BST:       5
                  / \
                 3   6
                / \   \
               2   4   7
        k = 9

    Output :-
    --------
    true

    Explanation :-
    --------------
    Nodes 2 and 7 sum to 9, so the function returns true.
*/


#include <bits/stdc++.h>
using namespace std;

struct TreeNode {
    int val;
    TreeNode *left, *right;
    TreeNode(int v) : val(v), left(nullptr), right(nullptr) {}
};

class BSTIterator {
    private:
        stack<TreeNode*> st;
        bool reverse;

        void insertNodes(TreeNode* node){
            while(node != nullptr){
                st.push(node);
                node = reverse ? node->right : node->left;
            }
        }

    public:
        BSTIterator(TreeNode* root, bool reverse) {
            this->reverse = reverse;
            insertNodes(root);
        }

        int next() {
            TreeNode* node = st.top();
            st.pop();
            if(!reverse) {
                if(node->right)
                    insertNodes(node->right);
            } else {
                if(node->left)
                    insertNodes(node->left);
            }
            return node->val;
        }

        bool hasNext() {
            return !st.empty();
        }
};

class Solution {
public:
    bool findTarget(TreeNode* root, int k) {
        BSTIterator* left = new BSTIterator(root, false);
        BSTIterator* right = new BSTIterator(root, true);
        int leftVal = left->next();
        int rightVal = right->next();

        while(leftVal < rightVal){
            if(leftVal + rightVal == k)
                return true;
            else if(leftVal + rightVal < k)
                leftVal = left->next();
            else
                rightVal = right->next();
        }

        return false;
    }
};
