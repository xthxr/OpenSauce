#include <stdio.h>
#include <stdlib.h>

// Node structure for stack
typedef struct Node {
    int data;
    struct Node* next;
} Node;

// Stack structure
typedef struct Stack {
    Node* top;
} Stack;

// Initialize the stack
// Time Complexity: O(1)
void init(Stack* s) {
    s->top = NULL;
}

// Check if stack is empty
// Time Complexity: O(1)
int isEmpty(Stack* s) {
    return s->top == NULL;
}

// Push an element onto the stack
// Time Complexity: O(1)
void push(Stack* s, int value) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    if (!newNode) {
        printf("Memory allocation failed!\n");
        return;
    }
    newNode->data = value;
    newNode->next = s->top;
    s->top = newNode;
    printf("%d pushed to stack\n", value);
}

// Pop an element from the stack
// Time Complexity: O(1)
int pop(Stack* s) {
    if (isEmpty(s)) {
        printf("Stack Underflow!\n");
        return -1;
    }
    Node* temp = s->top;
    int value = temp->data;
    s->top = s->top->next;
    free(temp);
    return value;
}

// Peek at the top element
// Time Complexity: O(1)
int peek(Stack* s) {
    if (isEmpty(s)) {
        printf("Stack is empty!\n");
        return -1;
    }
    return s->top->data;
}

// Display stack elements
// Time Complexity: O(n), n = number of elements in stack
void display(Stack* s) {
    if (isEmpty(s)) {
        printf("Stack is empty!\n");
        return;
    }
    Node* temp = s->top;
    printf("Stack elements (top to bottom): ");
    while (temp) {
        printf("%d ", temp->data);
        temp = temp->next;
    }
    printf("\n");
}

// Free all nodes in the stack
// Time Complexity: O(n)
void freeStack(Stack* s) {
    while (!isEmpty(s)) {
        pop(s);
    }
}

int main() {
    Stack s;
    init(&s);

    push(&s, 10);
    push(&s, 20);
    push(&s, 30);

    display(&s);

    printf("Top element is %d\n", peek(&s));

    printf("Popped element is %d\n", pop(&s));
    display(&s);

    freeStack(&s); // Clean up memory

    return 0;
}
