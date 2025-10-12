// Given a binary search tree (BST), find the lowest common ancestor (LCA) node of two given nodes in the BST.
// Ac cording to the definition of LCA on Wikipedia: "The lowest common ancestor is defined between two nodes p and q as the lowest node in T that has both p and q as descendants (where we allow a node to be a descendant of itself)."
/*Input: LCAof 10 and 14
Output:  12
Explanation: 12 is the closest node to both 10 and 14, which is also an ancestor of both the nodes.

Input: LCA of 8 and 14
Output:  8
Explanation: 8 is the closest node to both 8 and 14, which is also an ancestor of both the nodes.

*/
// using bst properties(recursive approach)
// Time Complexity: O(h) where h is the height of the tree
// Space Complexity: O(h) for the recursion stack

#include <iostream>
using namespace std;

class Node
{
public:
    int data;
    Node *left;
    Node *right;
    Node(int val)
    {
        data = val;
        left = right = nullptr;
    }
};

// Function to find LCA of nodes n1 and n2, assuming
// both are present in the BST
Node *LCA(Node *root, Node *n1, Node *n2)
{

    if (root == nullptr)
        return nullptr;

    // If both n1 and n2 are smaller than
    // root, go to left subtree
    if (root->data > n1->data && root->data > n2->data)
        return LCA(root->left, n1, n2);

    // If both n1 and n2 are greater than
    // root, go to right subtree
    if (root->data < n1->data && root->data < n2->data)
        return LCA(root->right, n1, n2);

    // If nodes n1 and n2 are on the opposite sides,
    // then root is the LCA
    return root;
}

int main()
{

    // Representation of input BST:
    //            20
    //           /  \
    //          8    22
    //        /   \     
    //       4    12
    //           /   \   
    //         10    14
    Node *root = new Node(20);
    root->left = new Node(8);
    root->right = new Node(22);
    root->left->left = new Node(4);
    root->left->right = new Node(12);
    root->left->right->left = new Node(10);
    root->left->right->right = new Node(14);

    Node *n1 = root->left->left;         // Node 4
    Node *n2 = root->left->right->right; // Node 14

    Node *res = LCA(root, n1, n2);
    cout << res->data << endl;

    return 0;
}
