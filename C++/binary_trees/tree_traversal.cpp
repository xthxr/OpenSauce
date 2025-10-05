#include <iostream>
using namespace std;

// Define a structure for tree nodes
struct Node {
    int data;       // Value stored in the node
    Node* left;     // Pointer to the left child
    Node* right;    // Pointer to the right child

    // Constructor to create a new node
    Node(int value) {
        data = value;
        left = right = nullptr;
    }
};

// Function for Preorder Traversal (Root -> Left -> Right)
void preorder(Node* root) {
    if (root == nullptr) return;    // Base condition: empty tree

    cout << root->data << " ";      // Step 1: Visit root
    preorder(root->left);           // Step 2: Traverse left subtree
    preorder(root->right);          // Step 3: Traverse right subtree
}

// Function for Inorder Traversal (Left -> Root -> Right)
void inorder(Node* root) {
    if (root == nullptr) return;    // Base condition

    inorder(root->left);            // Step 1: Traverse left subtree
    cout << root->data << " ";      // Step 2: Visit root
    inorder(root->right);           // Step 3: Traverse right subtree
}

// Function for Postorder Traversal (Left -> Right -> Root)
void postorder(Node* root) {
    if (root == nullptr) return;    // Base condition

    postorder(root->left);          // Step 1: Traverse left subtree
    postorder(root->right);         // Step 2: Traverse right subtree
    cout << root->data << " ";      // Step 3: Visit root
}

int main() {
    // Create a sample binary tree manually
    /*
            1
           / \
          2   3
         / \   \
        4   5   6
    */

    Node* root = new Node(1);
    root->left = new Node(2);
    root->right = new Node(3);
    root->left->left = new Node(4);
    root->left->right = new Node(5);
    root->right->right = new Node(6);

    // Display tree traversals
    cout << "Preorder Traversal: ";
    preorder(root);
    cout << endl;

    cout << "Inorder Traversal: ";
    inorder(root);
    cout << endl;

    cout << "Postorder Traversal: ";
    postorder(root);
    cout << endl;

    return 0;
}
