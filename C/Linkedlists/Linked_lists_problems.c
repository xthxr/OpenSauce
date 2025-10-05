// File: linked_list_problems.c
// Author: Gaayatri K
// Description: 5 Linked List problems (2 Easy + 3 Interview-Level)
// Each includes explanation, time & space complexity
// Uses the Node structure from linked_list_basics.c

#include <stdio.h>
#include <stdlib.h>

// Linked List Node Definition
typedef struct Node {
    int data;
    struct Node* next;
} Node;

// Helper: Create new node
Node* createNode(int val) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->data = val;
    newNode->next = NULL;
    return newNode;
}

// Helper: Insert at end (used to build lists)
void insertEnd(Node** head, int val) {
    Node* newNode = createNode(val);
    if (*head == NULL) {
        *head = newNode;
        return;
    }
    Node* temp = *head;
    while (temp->next)
        temp = temp->next;
    temp->next = newNode;
}

// Helper: Display the list
void display(Node* head) {
    while (head) {
        printf("%d -> ", head->data);
        head = head->next;
    }
    printf("NULL\n");
}

/* ==========================================================
   EASY PROBLEMS
   ========================================================== */

// 1️⃣ Count Nodes in a Linked List
// Time: O(n), Space: O(1)
int countNodes(Node* head) {
    int count = 0;
    while (head) {
        count++;
        head = head->next;
    }
    return count;
}

// 2️⃣ Find Maximum Element in a Linked List
// Time: O(n), Space: O(1)
int findMax(Node* head) {
    if (!head) return -1;  // empty list
    int max = head->data;
    while (head) {
        if (head->data > max)
            max = head->data;
        head = head->next;
    }
    return max;
}

/* ==========================================================
   INTERVIEW-LEVEL PROBLEMS
   ========================================================== */

// 3️⃣ Detect Loop in Linked List (Floyd’s Cycle Detection)
// Time: O(n), Space: O(1)
int detectLoop(Node* head) {
    Node *slow = head, *fast = head;
    while (slow && fast && fast->next) {
        slow = slow->next;
        fast = fast->next->next;
        if (slow == fast)
            return 1; // Loop found
    }
    return 0; // No loop
}

// 4️⃣ Find Middle Element of Linked List
// Time: O(n), Space: O(1)
int findMiddle(Node* head) {
    if (!head) return -1;
    Node *slow = head, *fast = head;
    while (fast && fast->next) {
        slow = slow->next;
        fast = fast->next->next;
    }
    return slow->data;
}

// 5️⃣ Reverse a Linked List (Iterative)
// Time: O(n), Space: O(1)
Node* reverseList(Node* head) {
    Node *prev = NULL, *curr = head, *next = NULL;
    while (curr) {
        next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }
    return prev;
}

/* ==========================================================
   MAIN FUNCTION FOR TESTING
   ========================================================== */
int main() {
    Node* head = NULL;
    insertEnd(&head, 10);
    insertEnd(&head, 20);
    insertEnd(&head, 30);
    insertEnd(&head, 40);
    insertEnd(&head, 50);

    printf("Original List: ");
    display(head);

    printf("\n[Easy Problems]\n");
    printf("1️⃣ Count of Nodes: %d\n", countNodes(head));
    printf("2️⃣ Maximum Element: %d\n", findMax(head));

    printf("\n[Interview-Level Problems]\n");
    printf("4️⃣ Middle Element: %d\n", findMiddle(head));

    printf("5️⃣ Reversed List: ");
    head = reverseList(head);
    display(head);

    // Loop detection demo
    printf("\n3️⃣ Detect Loop Test:\n");
    head->next->next->next = head; // manually creating loop for test
    printf("Loop Present? %s\n", detectLoop(head) ? "Yes" : "No");

    return 0;
}
