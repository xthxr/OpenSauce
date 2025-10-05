// File: linked_list_basics.c
// Author: Gaayatri K
// Description: Beginner-friendly Linked List implementations in C
// Includes Singly and Doubly Linked List with common operations
// Time & Space Complexity provided

#include <stdio.h>
#include <stdlib.h>

// Singly Linked List 

typedef struct Node {
    int data;
    struct Node* next;
} Node;

// Insert at end - O(n) time, O(1) space
void insertEnd(Node** head, int val) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->data = val;
    newNode->next = NULL;

    if (*head == NULL) {
        *head = newNode;
        return;
    }

    Node* temp = *head;
    while (temp->next)
        temp = temp->next;
    temp->next = newNode;
}

// Insert at beginning - O(1) time, O(1) space
void insertBeginning(Node** head, int val) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->data = val;
    newNode->next = *head;
    *head = newNode;
}

// Delete node by value - O(n) time, O(1) space
void deleteNode(Node** head, int val) {
    if (*head == NULL) return;

    Node* temp = *head;

    if (temp->data == val) {
        *head = temp->next;
        free(temp);
        return;
    }

    Node* prev = temp;
    temp = temp->next;
    while (temp && temp->data != val) {
        prev = temp;
        temp = temp->next;
    }

    if (temp) {
        prev->next = temp->next;
        free(temp);
    }
}

// Search a value - O(n) time, O(1) space
int search(Node* head, int val) {
    Node* temp = head;
    while (temp) {
        if (temp->data == val) return 1;
        temp = temp->next;
    }
    return 0;
}

// Reverse list - O(n) time, O(1) space
void reverse(Node** head) {
    Node* prev = NULL;
    Node* curr = *head;
    Node* next = NULL;

    while (curr) {
        next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }
    *head = prev;
}

// Display list - O(n) time, O(1) space
void display(Node* head) {
    Node* temp = head;
    while (temp) {
        printf("%d -> ", temp->data);
        temp = temp->next;
    }
    printf("NULL\n");
}

// Doubly Linked List 

typedef struct DNode {
    int data;
    struct DNode* prev;
    struct DNode* next;
} DNode;

// Insert at end - O(n) time, O(1) space
void insertEndDLL(DNode** head, int val) {
    DNode* newNode = (DNode*)malloc(sizeof(DNode));
    newNode->data = val;
    newNode->next = NULL;
    newNode->prev = NULL;

    if (*head == NULL) {
        *head = newNode;
        return;
    }

    DNode* temp = *head;
    while (temp->next)
        temp = temp->next;

    temp->next = newNode;
    newNode->prev = temp;
}

// Insert at beginning - O(1) time, O(1) space
void insertBeginningDLL(DNode** head, int val) {
    DNode* newNode = (DNode*)malloc(sizeof(DNode));
    newNode->data = val;
    newNode->next = *head;
    newNode->prev = NULL;

    if (*head)
        (*head)->prev = newNode;

    *head = newNode;
}

// Delete node by value - O(n) time, O(1) space
void deleteNodeDLL(DNode** head, int val) {
    DNode* temp = *head;

    while (temp && temp->data != val)
        temp = temp->next;

    if (!temp) return;

    if (temp->prev)
        temp->prev->next = temp->next;
    else
        *head = temp->next;

    if (temp->next)
        temp->next->prev = temp->prev;

    free(temp);
}

// Display forward - O(n) time, O(1) space
void displayForward(DNode* head) {
    DNode* temp = head;
    while (temp) {
        printf("%d <-> ", temp->data);
        temp = temp->next;
    }
    printf("NULL\n");
}

// Display backward - O(n) time, O(1) space
void displayBackward(DNode* head) {
    if (!head) return;
    DNode* temp = head;
    while (temp->next)
        temp = temp->next;
    while (temp) {
        printf("%d <-> ", temp->data);
        temp = temp->prev;
    }
    printf("NULL\n");
}

//  Main Function
int main() {
    printf("Singly Linked List\n");
    Node* sll = NULL;
    insertEnd(&sll, 10);
    insertEnd(&sll, 20);
    insertBeginning(&sll, 5);
    display(sll);
    deleteNode(&sll, 20);
    display(sll);
    reverse(&sll);
    display(sll);
    printf("Search 10: %s\n", search(sll, 10) ? "Found" : "Not Found");
    printf("\n--- Doubly Linked List ---\n");
    DNode* dll = NULL;
    insertEndDLL(&dll, 1);
    insertEndDLL(&dll, 2);
    insertBeginningDLL(&dll, 0);
    displayForward(dll);
    displayBackward(dll);
    deleteNodeDLL(&dll, 1);
    displayForward(dll);
    return 0;
}
