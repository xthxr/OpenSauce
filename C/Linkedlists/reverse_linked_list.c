/*
 * Program: Reverse a Singly Linked List
 * 
 * Description:
 * This program creates a singly linked list and reverses it.
 * The reversal is done by changing the next pointers of nodes.
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */

#include <stdio.h>
#include <stdlib.h>

// Define structure for a linked list node
struct Node {
    int data;
    struct Node* next;
};

// Function to create a new node
struct Node* createNode(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

// Function to insert a node at the end of the list
void insertEnd(struct Node** head, int data) {
    struct Node* newNode = createNode(data);
    if (*head == NULL) {
        *head = newNode;
        return;
    }
    struct Node* temp = *head;
    while (temp->next != NULL)
        temp = temp->next;
    temp->next = newNode;
}

// Function to reverse the linked list
struct Node* reverseList(struct Node* head) {
    struct Node* prev = NULL;    // To store previous node
    struct Node* current = head; // To traverse the list
    struct Node* next = NULL;    // To store next node temporarily

    while (current != NULL) {
        next = current->next;    // Store next node
        current->next = prev;    // Reverse the link
        prev = current;          // Move prev forward
        current = next;          // Move current forward
    }
    return prev; // New head of the reversed list
}

// Function to print the linked list
void printList(struct Node* head) {
    struct Node* temp = head;
    while (temp != NULL) {
        printf("%d", temp->data);
        if (temp->next != NULL)
            printf(" -> ");
        temp = temp->next;
    }
    printf("\n");
}


// Main function
int main() {
    struct Node* head = NULL;

    // Create linked list: 10 -> 20 -> 30 -> 40 ->
    insertEnd(&head, 10);
    insertEnd(&head, 20);
    insertEnd(&head, 30);
    insertEnd(&head, 40);

    printf("Original Linked List:\n");
    printList(head);

    head = reverseList(head);

    printf("\nReversed Linked List:\n");
    printList(head);

    return 0;
}
