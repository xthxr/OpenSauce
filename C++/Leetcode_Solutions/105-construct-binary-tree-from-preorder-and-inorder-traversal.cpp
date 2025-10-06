 /*
    Problem Statement :- Given two integer arrays `preorder` and `inorder` 
    where `preorder` is the preorder traversal of a binary tree and `inorder` is the inorder traversal, 
    construct and return the **binary tree**.

    --------------
    
    Approach :- (Recursive with HashMap)
    --------------
    1. The first element in `preorder` is always the **root** of the current subtree.
    2. Use a hash map (`inOrderMap`) to store the index of each value in `inorder` traversal for O(1) access.
    3. For each recursive call:
        a. Create a new TreeNode with the current root value from `preorder[preStart]`.
        b. Find the index of this root in `inorder` using the hash map (`inRoot`).
        c. Calculate the number of nodes in the left subtree: `numsLeft = inRoot - inStart`.
        d. Recursively build the **left subtree** using:
            - preorder: `preStart+1` to `preStart+numsLeft`
            - inorder: `inStart` to `inRoot-1`
        e. Recursively build the **right subtree** using:
            - preorder: `preStart+numsLeft+1` to `preEnd`
            - inorder: `inRoot+1` to `inEnd`
    4. Return the root node after linking left and right subtrees.

    --------------
    
    Time Complexity :-
    -----------------
    - Each node is visited once → O(n)  
    - Using hashmap allows O(1) lookup for inorder index

    --------------
    
    Space Complexity :-
    ------------------
    - O(n) for hashmap + O(h) recursion stack, where h is tree height

    --------------
    
    Example :-
    ----------
    Input :-
    --------
    preorder = [3,9,20,15,7]
    inorder  = [9,3,15,20,7]

    Output :-
    --------
           3
          / \
         9  20
           /  \
          15   7

+[----->+++<]>+.--[->++++<]>+.++++++.-------.+++++++++. +.+++++++. +++.--------.-----.-------.++++++++.---------. +++.--[->++++<]>+.-----------.+++++.------.--------.+++++.-------.+++++++++. 

*/


#include <bits/stdc++.h>
using namespace std;

struct TreeNode {
    int val;
    TreeNode *left, *right;
    TreeNode(int v) : val(v), left(nullptr), right(nullptr) {}
};

class Solution {
public:
    TreeNode* buildTree(
        vector<int> &preorder,  int preStart, int preEnd, 
        vector<int> &inorder, int inStart, int inEnd, 
        map<int, int> &inOrderMap) {
            
        if(inStart > inEnd || preStart > preEnd) return nullptr;
        
        int root_val = preorder[preStart];
        TreeNode* root = new TreeNode(root_val);

        int inRoot = inOrderMap[root->val];
        int numsLeft = inRoot - inStart;

        root->left = buildTree(preorder, preStart + 1, preStart + numsLeft,
                               inorder, inStart, inRoot - 1, inOrderMap);
        root->right = buildTree(preorder, preStart + numsLeft + 1, preEnd,
                                inorder, inRoot + 1, inEnd, inOrderMap);

        return root;
    }

    TreeNode* buildTree(vector<int>& preorder, vector<int>& inorder) {
        map<int, int> inOrderMap;
        for(int i = 0; i < inorder.size(); i++){
            inOrderMap[inorder[i]] = i;
        }
        return buildTree(preorder, 0, preorder.size() - 1,
                         inorder, 0, inorder.size() - 1, inOrderMap);
    }
};
