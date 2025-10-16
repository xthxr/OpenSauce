/*
Definition for a binary tree node:
class Node {
public:
    int data;
    Node* left;
    Node* right;

    Node(int val) {
        data = val;
        left = NULL;
        right = NULL;
    }
};
*/

class Solution {
public:
    /**
     * @brief Removes all nodes from the Binary Search Tree (BST) whose values
     *        lie outside the given range [low, high].
     * 
     * @param root Pointer to the root node of the BST
     * @param low  Lower bound of the range
     * @param high Upper bound of the range
     * @return Node* Pointer to the new root of the modified BST
     */
    Node* removeKeys(Node* root, int low, int high) {
        // Base case: if tree is empty
        if (!root) 
            return nullptr;

        // If current node's value is within range,
        // recursively fix its left and right subtrees.
        if (root->data >= low && root->data <= high) {
            root->left = removeKeys(root->left, low, high);
            root->right = removeKeys(root->right, low, high);
        } 
        // If current node's value is less than 'low',
        // then all nodes in its left subtree are also smaller (BST property),
        // so we can skip the left subtree.
        else if (root->data < low) {
            return removeKeys(root->right, low, high);
        } 
        // If current node's value is greater than 'high',
        // then all nodes in its right subtree are also larger (BST property),
        // so we can skip the right subtree.
        else {
            return removeKeys(root->left, low, high);
        }

        return root;
    }
};

/**
 * @brief Example usage / test case
 */
int main() {
    /*
        Original BST:
                10
               /  \
              5    50
             /    /  \
            1    40   100

        Range: [5, 45]
        After removal, BST becomes:
                10
               /  \
              5    40
    */

    Node* root = new Node(10);
    root->left = new Node(5);
    root->left->left = new Node(1);
    root->right = new Node(50);
    root->right->left = new Node(40);
    root->right->right = new Node(100);

    Solution sol;
    Node* updatedRoot = sol.removeKeys(root, 5, 45);

    // You can add your own traversal (like inorder) to verify results
    return 0;
}

/**
 * Time Complexity:  O(N)
 *    - Each node is visited exactly once.
 *
 * Space Complexity: O(H)
 *    - Due to recursion stack, where H is the height of the BST.
 *    - Worst case (skewed tree): O(N)
 *    - Best/Average case (balanced tree): O(log N)
 */
