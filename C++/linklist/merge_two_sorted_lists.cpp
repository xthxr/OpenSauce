/*
Merge Two Sorted Lists

Problem: Merge two sorted linked lists and return it as a sorted list.
The list should be made by splicing together the nodes of the first two lists.

Time Complexity: O(m + n) - where m and n are lengths of the two lists
Space Complexity: O(1) - only using constant extra space

Example:
Input: list1 = [1,2,4], list2 = [1,3,4]
Output: [1,1,2,3,4,4]
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
     * Merges two sorted linked lists
     * 
     * @param list1: pointer to the head of first sorted list
     * @param list2: pointer to the head of second sorted list
     * @return: pointer to the head of merged sorted list
     */
    ListNode* mergeTwoLists(ListNode* list1, ListNode* list2) {
        // Create a dummy node to simplify the logic
        ListNode dummy;
        ListNode* current = &dummy;
        
        // Compare nodes and link the smaller one
        while (list1 != nullptr && list2 != nullptr) {
            if (list1->val <= list2->val) {
                current->next = list1;
                list1 = list1->next;
            } else {
                current->next = list2;
                list2 = list2->next;
            }
            current = current->next;
        }
        
        // Attach remaining nodes
        if (list1 != nullptr) {
            current->next = list1;
        } else {
            current->next = list2;
        }
        
        return dummy.next;
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
    
    // Test case 1: [1,2,4] and [1,3,4]
    int arr1[] = {1, 2, 4};
    int arr2[] = {1, 3, 4};
    ListNode* list1 = createList(arr1, 3);
    ListNode* list2 = createList(arr2, 3);
    
    cout << "List 1: ";
    printList(list1);
    cout << "List 2: ";
    printList(list2);
    
    ListNode* merged1 = solution.mergeTwoLists(list1, list2);
    cout << "Merged: ";
    printList(merged1);
    
    deleteList(merged1);
    
    // Test case 2: [] and [0]
    ListNode* list3 = nullptr;
    int arr4[] = {0};
    ListNode* list4 = createList(arr4, 1);
    
    cout << "\nList 1: ";
    printList(list3);
    cout << "List 2: ";
    printList(list4);
    
    ListNode* merged2 = solution.mergeTwoLists(list3, list4);
    cout << "Merged: ";
    printList(merged2);
    
    deleteList(merged2);
    
    // Test case 3: [] and []
    ListNode* list5 = nullptr;
    ListNode* list6 = nullptr;
    
    cout << "\nList 1: ";
    printList(list5);
    cout << "List 2: ";
    printList(list6);
    
    ListNode* merged3 = solution.mergeTwoLists(list5, list6);
    cout << "Merged: ";
    printList(merged3);
    
    return 0;
}