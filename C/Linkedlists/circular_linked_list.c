/*
 * File: circular_linked_list.c
 * Description: Implementation of Circular Singly Linked List in C
 * Author: Gompa Rani Prasanna
 *
 * Algorithm:
 *   - A circular linked list is a variation of a linked list where
 *     the last node points back to the head, forming a circle.
 *   - Supports insertion at the beginning/end, deletion, and traversal.
 *
 * Operations Implemented:
 *   1. Insert at end
 *   2. Insert at beginning
 *   3. Delete a node
 *   4. Display the list
 *
 * Time Complexity:
 *   - Insertion at beginning: O(1)
 *   - Insertion at end: O(n)
 *   - Deletion: O(n)
 *   - Traversal/Display: O(n)
 *
 * Space Complexity: O(n) (for n nodes)
 *
 * Example Usage:
 *   Input:
 *     Insert 10, 20, 30 at end
 *     Display list
 *     Insert 5 at beginning
 *     Delete 20
 *   Output:
 *     10 -> 20 -> 30 -> (back to head)
 *     5 -> 10 -> 30 -> (back to head)
 */

#include <stdio.h>
#include <stdlib.h>

// Node structure definition
struct Node {
    int data;
    struct Node* next;
};

// Global pointer to the last node (useful for circular list)
struct Node* last = NULL;

// Function to insert at the end
void insertEnd(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;

    if (last == NULL) {
        // First node creation
        last = newNode;
        last->next = last;
    } else {
        newNode->next = last->next;
        last->next = newNode;
        last = newNode;
    }
}

// Function to insert at the beginning
void insertBegin(int data) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;

    if (last == NULL) {
        last = newNode;
        last->next = last;
    } else {
        newNode->next = last->next;
        last->next = newNode;
    }
}

// Function to delete a node by value
void deleteNode(int key) {
    if (last == NULL) {
        printf("List is empty.\n");
        return;
    }

    struct Node *curr = last->next, *prev = last;

    // If the list has only one node
    if (curr == last && curr->data == key) {
        last = NULL;
        free(curr);
        return;
    }

    // If the node to be deleted is the first node
    if (curr->data == key) {
        prev->next = curr->next;
        free(curr);
        return;
    }

    // Search for the node to delete
    while (curr != last && curr->data != key) {
        prev = curr;
        curr = curr->next;
    }

    // If node found
    if (curr->data == key) {
        prev->next = curr->next;
        // If deleting last node, update `last`
        if (curr == last)
            last = prev;
        free(curr);
    } else {
        printf("Node with value %d not found.\n", key);
    }
}

// Function to display the circular linked list
void display() {
    if (last == NULL) {
        printf("List is empty.\n");
        return;
    }

    struct Node* temp = last->next; // start from head
    do {
        printf("%d -> ", temp->data);
        temp = temp->next;
    } while (temp != last->next);
    printf("(back to head)\n");
}

// Example usage
int main() {
    printf("Circular Linked List Operations\n\n");

    insertEnd(10);
    insertEnd(20);
    insertEnd(30);
    printf("List after inserting 10, 20, 30 at end:\n");
    display();

    insertBegin(5);
    printf("\nList after inserting 5 at beginning:\n");
    display();

    deleteNode(20);
    printf("\nList after deleting 20:\n");
    display();

    deleteNode(50); // testing non-existent element

    return 0;
}
