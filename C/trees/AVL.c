#include <stdio.h>
#include <stdlib.h>


struct AVLNode {
    int data;
    struct AVLNode* left;
    struct AVLNode* right;
    int height; 
};


struct AVLNode* createNode(int data) {
    struct AVLNode* node = (struct AVLNode*)malloc(sizeof(struct AVLNode));
    node->data = data;
    node->left = node->right = NULL;
    node->height = 1; 
    return node;
}


int height(struct AVLNode* node) {
    if (node == NULL) return 0;
    return node->height;
}


int max(int a, int b) {
    return (a > b) ? a : b;
}


struct AVLNode* rightRotate(struct AVLNode* y) {
    struct AVLNode* x = y->left;
    struct AVLNode* T2 = x->right;

   
    x->right = y;
    y->left = T2;

    
    y->height = max(height(y->left), height(y->right)) + 1;
    x->height = max(height(x->left), height(x->right)) + 1;

    return x; 
}


struct AVLNode* leftRotate(struct AVLNode* x) {
    struct AVLNode* y = x->right;
    struct AVLNode* T2 = y->left;

    
    y->left = x;
    x->right = T2;

   
    x->height = max(height(x->left), height(x->right)) + 1;
    y->height = max(height(y->left), height(y->right)) + 1;

    return y; 
}


int getBalance(struct AVLNode* node) {
    if (node == NULL) return 0;
    return height(node->left) - height(node->right);
}


struct AVLNode* insert(struct AVLNode* node, int data) {
    
    if (node == NULL)
        return createNode(data);

    if (data < node->data)
        node->left = insert(node->left, data);
    else if (data > node->data)
        node->right = insert(node->right, data);
    else 
        return node;

    
    node->height = 1 + max(height(node->left), height(node->right));

    
    int balance = getBalance(node);

    

    
    if (balance > 1 && data < node->left->data)
        return rightRotate(node);

    
    if (balance < -1 && data > node->right->data)
        return leftRotate(node);

   
    if (balance > 1 && data > node->left->data) {
        node->left = leftRotate(node->left);
        return rightRotate(node);
    }

   
    if (balance < -1 && data < node->right->data) {
        node->right = rightRotate(node->right);
        return leftRotate(node);
    }

    return node; 
}


void inorder(struct AVLNode* root) {
    if (root != NULL) {
        inorder(root->left);
        printf("%d ", root->data);
        inorder(root->right);
    }
}


void preorder(struct AVLNode* root) {
    if (root != NULL) {
        printf("%d ", root->data);
        preorder(root->left);
        preorder(root->right);
    }
}


void postorder(struct AVLNode* root) {
    if (root != NULL) {
        postorder(root->left);
        postorder(root->right);
        printf("%d ", root->data);
    }
}


int main() {
    struct AVLNode* root = NULL;

    
    int values[] = {10, 20, 30, 40, 50, 25};
    int n = sizeof(values)/sizeof(values[0]);
    for (int i = 0; i < n; i++) {
        root = insert(root, values[i]);
    }

   
    printf("Inorder Traversal: ");
    inorder(root);
    printf("\n");

    printf("Preorder Traversal: ");
    preorder(root);
    printf("\n");

    printf("Postorder Traversal: ");
    postorder(root);
    printf("\n");

    return 0;
}
