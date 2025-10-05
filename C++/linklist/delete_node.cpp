/*
Delete Node in a Linked List

Problem: Write a function to delete a node in a singly-linked list. 
You will not be given access to the head of the list, instead you will be given access to the node to be deleted directly.

Time Complexity: O(1) - constant time operation
Space Complexity: O(1) - only using constant extra space

Example:
Input: head = [4,5,1,9], node = 5
Output: [4,1,9]
Explanation: You are given the second node with value 5, the linked list should become 4 -> 1 -> 9 after calling your function.
*/

#include <iostream>
using namespace std;

// Definition for singly-linked list
struct ListNode {
    int val;
    ListNode *next;
    ListNode() : val(0), next(nullptr) {}
    ListNode(int x) : val(x), next(nullptr) {}
    ListNode(int x, ListNode *next) : val(x), next(next) {}
};

class Solution {
public:
    /**
     * Deletes a node from linked list given only access to that node
     * 
     * @param node: pointer to the node to be deleted (guaranteed not to be tail)
     */
    void deleteNode(ListNode* node) {
        // Copy the value from next node
        node->val = node->next->val;
        
        // Store the node to be deleted
        ListNode* nodeToDelete = node->next;
        
        // Skip the next node
        node->next = node->next->next;
        
        // Free memory
        delete nodeToDelete;
    }
};

// Helper function to create a linked list from array
ListNode* createList(int arr[], int size) {
    if (size == 0) return nullptr;
    
    ListNode* head = new ListNode(arr[0]);
    ListNode* current = head;
    
    for (int i = 1; i < size; i++) {
        current->next = new ListNode(arr[i]);
        current = current->next;
    }
    
    return head;
}

// Helper function to find node with given value
ListNode* findNode(ListNode* head, int val) {
    while (head != nullptr) {
        if (head->val == val) {
            return head;
        }
        head = head->next;
    }
    return nullptr;
}

// Helper function to print linked list
void printList(ListNode* head) {
    if (head == nullptr) {
        cout << "NULL";
        return;
    }
    
    ListNode* current = head;
    while (current != nullptr) {
        cout << current->val;
        if (current->next != nullptr) {
            cout << " -> ";
        }
        current = current->next;
    }
    cout << " -> NULL" << endl;
}

// Helper function to free memory
void deleteList(ListNode* head) {
    while (head != nullptr) {
        ListNode* temp = head;
        head = head->next;
        delete temp;
    }
}

int main() {
    Solution solution;
    
    // Test case 1: [4,5,1,9], delete node with value 5
    int arr1[] = {4, 5, 1, 9};
    ListNode* head1 = createList(arr1, 4);
    
    cout << "Original list: ";
    printList(head1);
    
    ListNode* nodeToDelete1 = findNode(head1, 5);
    if (nodeToDelete1 != nullptr && nodeToDelete1->next != nullptr) {
        solution.deleteNode(nodeToDelete1);
        cout << "After deleting node with value 5: ";
        printList(head1);
    }
    
    deleteList(head1);
    
    // Test case 2: [4,5,1,9], delete node with value 1
    int arr2[] = {4, 5, 1, 9};
    ListNode* head2 = createList(arr2, 4);
    
    cout << "\nOriginal list: ";
    printList(head2);
    
    ListNode* nodeToDelete2 = findNode(head2, 1);
    if (nodeToDelete2 != nullptr && nodeToDelete2->next != nullptr) {
        solution.deleteNode(nodeToDelete2);
        cout << "After deleting node with value 1: ";
        printList(head2);
    }
    
    deleteList(head2);
    
    return 0;
}