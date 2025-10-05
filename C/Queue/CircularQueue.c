#include <stdio.h>
#include <stdlib.h>

#define SIZE 100  // Define the maximum size of the circular queue

int CQ[SIZE];
int front = -1, rear = -1;

// Function to insert an element in the Circular Queue
void CENQUE(int item) {
    if ((front == 0 && rear == SIZE - 1) || (front == rear + 1)) {
        printf("\nQueue Overflow! Cannot insert %d\n", item);
        return;
    }

    if (front == -1) {
        front = 0;
        rear = 0;
    } else if (rear == SIZE - 1)
        rear = 0;
    else
        rear++;

    CQ[rear] = item;
    printf("\nInserted: %d\n", item);
}

// Function to delete an element from the Circular Queue
void CDELQUE() {
    if (front == -1) {
        printf("\nQueue Underflow! Nothing to delete.\n");
        return;
    }

    int item = CQ[front];
    printf("\nDeleted: %d\n", item);

    if (front == rear) { // Queue has only one element
        front = -1;
        rear = -1;
    } else if (front == SIZE - 1)
        front = 0;
    else
        front++;
}

// Function to display elements of the Circular Queue
void CDISPLAY() {
    if (front == -1) {
        printf("\nQueue is Empty.\n");
        return;
    }

    printf("\nCircular Queue Elements:\n");

    if (rear >= front) {
        for (int i = front; i <= rear; i++)
            printf("%d ", CQ[i]);
    } else {
        for (int i = front; i < SIZE; i++)
            printf("%d ", CQ[i]);
        for (int i = 0; i <= rear; i++)
            printf("%d ", CQ[i]);
    }
    printf("\n");
}

// Main Function with Menu
int main() {
    int choice, item;

    while (1) {
        printf("1. Insert (CENQUE)\n");
        printf("2. Delete (CDELQUE)\n");
        printf("3. Display (CDISPLAY)\n");
        printf("4. Exit\n");
        printf("Enter your choice: ");
        scanf("%d", &choice);

        switch (choice) {
            case 1:
                printf("Enter the element to insert: ");
                scanf("%d", &item);
                CENQUE(item);
                break;
            case 2:
                CDELQUE();
                break;
            case 3:
                CDISPLAY();
                break;
            case 4:
                printf("\nExiting Program...\n");
                exit(0);
            default:
                printf("\nInvalid Choice! Try again.\n");
        }
    }

    return 0;
}
