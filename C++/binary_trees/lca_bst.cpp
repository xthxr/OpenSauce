/*Given the root of a Binary Search Tree and two node n1 and n2, find the Lowest Common Ancestor (LCA). LCA is the deepest node that has both n1 and n2 as descendants.

Note: Both node are always present in the Binary Search Tree.
Examples:

Input:  n1.data = 4, n2.data = 14

lowest_common_ancestor_in_a_binary_tree2

Output: 8
Explanation: 8 is the lowest common ancestor (LCA) of nodes 4 and 14, as it is the deepest node that is an ancestor of both.*/
// Naive approach
#include <iostream>
#include <vector>

using namespace std;

// Node Structure
class Node
{
public:
    int data;
    Node *left, *right;
    Node(int x)
    {
        data = x;
        left = nullptr;
        right = nullptr;
    }
};

// Function to find path from root to given node.
bool findPath(Node *root, vector<Node *> &path, int n)
{

    if (root == nullptr)
        return false;

    // Store current node value in the path.
    path.push_back(root);

    if (root->data == n ||
        findPath(root->left, path, n) ||
        findPath(root->right, path, n))
        return true;

    // else remove root from path and return false
    path.pop_back();
    return false;
}

Node *lca(Node *root, int n1, int n2)
{

    vector<Node *> path1, path2;

    // Find paths from root to n1
    // and root to n2.
    if (!findPath(root, path1, n1) ||
        !findPath(root, path2, n2))
        return nullptr;

    // Compare the paths to get the first
    // different value
    int i;
    for (i = 0; i < path1.size() && i < path2.size(); i++)
    {
        if (path1[i] != path2[i])
            return path1[i - 1];
    }

    return path1[i - 1];
}

int main()
{

    // construct the binary tree
    //			   1
    //           /   \
    //          2     3
    //         / \   / \
    //        4  5  6   7
    //             /
    //            8

    Node *root = new Node(1);
    root->left = new Node(2);
    root->right = new Node(3);
    root->left->left = new Node(4);
    root->left->right = new Node(5);
    root->right->left = new Node(6);
    root->right->right = new Node(7);
    root->right->left->left = new Node(8);

    Node *ans = lca(root, 7, 8);

    cout << ans->data;

    return 0;
}

// better approach : usin binary search tree properties
#include <iostream>
using namespace std;

// Node structure
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

    // Node 4
    Node *n1 = root->left->left;

    // Node 14
    Node *n2 = root->left->right->right;

    Node *res = LCA(root, n1, n2);
    cout << res->data << endl;

    return 0;
}
