#include <stdio.h>
#include <stdlib.h>


struct Node {
    int data;
    struct Node* left;
    struct Node* right;
};


struct Node* createNode(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}


struct BSTNode {
    int data;
    struct BSTNode* left;
    struct BSTNode* right;
};


struct BSTNode* createBSTNode(int data) {
    struct BSTNode* newNode = (struct BSTNode*)malloc(sizeof(struct BSTNode));
    newNode->data = data;
    newNode->left = newNode->right = NULL;
    return newNode;
}


struct BSTNode* insertBST(struct BSTNode* root, int data) {
    if (root == NULL) return createBSTNode(data);

    if (data < root->data)
        root->left = insertBST(root->left, data);
    else if (data > root->data)
        root->right = insertBST(root->right, data);

    return root;
}


struct BSTNode* searchBST(struct BSTNode* root, int key) {
    if (root == NULL || root->data == key)
        return root;

    if (key < root->data)
        return searchBST(root->left, key);
    else
        return searchBST(root->right, key);
}


void inorder(struct Node* root) {
    if (root != NULL) {
        inorder(root->left);
        printf("%d ", root->data);
        inorder(root->right);
    }
}

void preorder(struct Node* root) {
    if (root != NULL) {
        printf("%d ", root->data);
        preorder(root->left);
        preorder(root->right);
    }
}

void postorder(struct Node* root) {
    if (root != NULL) {
        postorder(root->left);
        postorder(root->right);
        printf("%d ", root->data);
    }
}


struct Queue {
    int front, rear, size;
    unsigned capacity;
    struct Node** array;
};

struct Queue* createQueue(unsigned capacity) {
    struct Queue* queue = (struct Queue*) malloc(sizeof(struct Queue));
    queue->capacity = capacity;
    queue->front = queue->size = 0;
    queue->rear = capacity - 1;
    queue->array = (struct Node**) malloc(queue->capacity * sizeof(struct Node*));
    return queue;
}

int isFull(struct Queue* queue) { return (queue->size == queue->capacity); }
int isEmpty(struct Queue* queue) { return (queue->size == 0); }

void enqueue(struct Queue* queue, struct Node* item) {
    if (isFull(queue)) return;
    queue->rear = (queue->rear + 1) % queue->capacity;
    queue->array[queue->rear] = item;
    queue->size += 1;
}

struct Node* dequeue(struct Queue* queue) {
    if (isEmpty(queue)) return NULL;
    struct Node* item = queue->array[queue->front];
    queue->front = (queue->front + 1) % queue->capacity;
    queue->size -= 1;
    return item;
}

void levelOrder(struct Node* root) {
    if (root == NULL) return;

    struct Queue* queue = createQueue(100);
    enqueue(queue, root);

    while (!isEmpty(queue)) {
        struct Node* temp = dequeue(queue);
        printf("%d ", temp->data);
        if (temp->left) enqueue(queue, temp->left);
        if (temp->right) enqueue(queue, temp->right);
    }
}


int main() {
    
    struct Node* root = createNode(1);
    root->left = createNode(2);
    root->right = createNode(3);
    root->left->left = createNode(4);
    root->left->right = createNode(5);

    printf("Binary Tree Inorder Traversal: ");
    inorder(root);
    printf("\nBinary Tree Preorder Traversal: ");
    preorder(root);
    printf("\nBinary Tree Postorder Traversal: ");
    postorder(root);
    printf("\nBinary Tree Level Order Traversal: ");
    levelOrder(root);
    printf("\n\n");

    
    struct BSTNode* bstRoot = NULL;
    bstRoot = insertBST(bstRoot, 50);
    insertBST(bstRoot, 30);
    insertBST(bstRoot, 70);
    insertBST(bstRoot, 20);
    insertBST(bstRoot, 40);
    insertBST(bstRoot, 60);
    insertBST(bstRoot, 80);

    printf("BST Inorder Traversal (Sorted): ");
    inorder((struct Node*)bstRoot);
    printf("\n");

    int key = 40;
    struct BSTNode* found = searchBST(bstRoot, key);
    if (found != NULL)
        printf("BST Node %d found\n", key);
    else
        printf("BST Node %d not found\n", key);

    return 0;
}
