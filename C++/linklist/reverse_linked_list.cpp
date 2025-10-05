/*
Reverse Linked List

Problem: Given the head of a singly linked list, reverse the list and return the reversed list.

Time Complexity: O(n) - where n is the number of nodes
Space Complexity: O(1) - only using constant extra space

Example:
Input: 1->2->3->4->5->NULL
Output: 5->4->3->2->1->NULL
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
     * Reverses a singly linked list iteratively
     * 
     * @param head: pointer to the head of the linked list
     * @return: pointer to the new head of the reversed list
     */
    ListNode* reverseList(ListNode* head) {
        ListNode* prev = nullptr;
        ListNode* current = head;
        
        while (current != nullptr) {
            ListNode* nextTemp = current->next;  // Store next node
            current->next = prev;                // Reverse the link
            prev = current;                      // Move prev forward
            current = nextTemp;                  // Move current forward
        }
        
        return prev;  // prev is now the new head
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

// Helper function to print linked list
void printList(ListNode* head) {
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
    
    // Test case 1: [1,2,3,4,5]
    int arr1[] = {1, 2, 3, 4, 5};
    ListNode* head1 = createList(arr1, 5);
    
    cout << "Original list: ";
    printList(head1);
    
    ListNode* reversed1 = solution.reverseList(head1);
    cout << "Reversed list: ";
    printList(reversed1);
    
    deleteList(reversed1);
    
    // Test case 2: [1,2]
    int arr2[] = {1, 2};
    ListNode* head2 = createList(arr2, 2);
    
    cout << "\nOriginal list: ";
    printList(head2);
    
    ListNode* reversed2 = solution.reverseList(head2);
    cout << "Reversed list: ";
    printList(reversed2);
    
    deleteList(reversed2);
    
    // Test case 3: Empty list
    ListNode* head3 = nullptr;
    cout << "\nOriginal list: ";
    printList(head3);
    
    ListNode* reversed3 = solution.reverseList(head3);
    cout << "Reversed list: ";
    printList(reversed3);
    
    return 0;
}