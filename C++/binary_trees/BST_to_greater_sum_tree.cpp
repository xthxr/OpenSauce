/*
    Problem:
    Convert a Binary Search Tree (BST) into a Greater Sum Tree (GST),
    where each node contains the sum of all nodes greater than that node's value.

    Example:
        Input BST:             Output GST:
             5                       18
            / \                     / \
           2   13                 20   0
*/

#include <iostream>
using namespace std;

/* -------------------- Node Definition -------------------- */
class Node {
public:
    int data;
    Node* left;
    Node* right;

    Node(int value) {
        data = value;
        left = nullptr;
        right = nullptr;
    }
};

/* -------------------- Solution Class -------------------- */
class Solution {
public:
    int sum;  // Used in recursive approach

    /* ---------------------------------------------------------
       Approach 1: Recursive (Reverse Inorder Traversal)
       ---------------------------------------------------------
       Logic:
       - Traverse the tree in reverse inorder (Right → Root → Left).
       - Maintain a running sum of all visited nodes.
       - Update each node's value to the running sum before adding its original value.

       Time Complexity  : O(N)
       Space Complexity : O(H)  [Recursive stack, H = height of tree]
    --------------------------------------------------------- */
    void recursiveHelper(Node* root) {
        if (!root) return;

        // 1. Traverse right subtree (greater values)
        recursiveHelper(root->right);

        // 2. Update current node
        int val = root->data;
        root->data = sum;
        sum += val;

        // 3. Traverse left subtree (smaller values)
        recursiveHelper(root->left);
    }

    void transformTreeRecursive(Node* root) {
        sum = 0;
        recursiveHelper(root);
    }

    /* ---------------------------------------------------------
       Approach 2: Iterative (Morris Traversal)
       ---------------------------------------------------------
       Logic:
       - Perform reverse inorder traversal without recursion or stack.
       - Use temporary links (threads) to backtrack to parent nodes.
       - Update each node's value using running sum.

       Time Complexity  : O(N)
       Space Complexity : O(1)  [No recursion/stack used]
    --------------------------------------------------------- */
    void transformTreeIterative(Node* root) {
        Node* curr = root;
        int sum = 0;

        while (curr) {
            if (curr->right == nullptr) {
                // Process current node
                int val = curr->data;
                curr->data = sum;
                sum += val;

                // Move to left child
                curr = curr->left;
            } else {
                // Find inorder predecessor in the right subtree
                Node* right = curr->right;
                while (right->left && right->left != curr)
                    right = right->left;

                if (right->left == nullptr) {
                    // Establish thread (temporary link)
                    right->left = curr;
                    curr = curr->right;
                } else {
                    // Thread exists — remove it and process node
                    right->left = nullptr;
                    int val = curr->data;
                    curr->data = sum;
                    sum += val;

                    curr = curr->left;
                }
            }
        }
    }
};

/* -------------------- Helper: Inorder Traversal -------------------- */
void inorder(Node* root) {
    if (!root) return;
    inorder(root->left);
    cout << root->data << " ";
    inorder(root->right);
}

/* -------------------- Example Usage -------------------- */
int main() {
    /*
        Example Tree:
               5
              / \
             2   13
    */
    Node* root1 = new Node(5);
    root1->left = new Node(2);
    root1->right = new Node(13);

    Solution sol;

    cout << "Using Recursive Approach:\n";
    sol.transformTreeRecursive(root1);
    inorder(root1); // Expected output: 20 18 0
    cout << endl;

    // Create another tree for iterative approach
    Node* root2 = new Node(5);
    root2->left = new Node(2);
    root2->right = new Node(13);

    cout << "Using Iterative (Morris Traversal) Approach:\n";
    sol.transformTreeIterative(root2);
    inorder(root2); // Expected output: 20 18 0
    cout << endl;

    return 0;
}
