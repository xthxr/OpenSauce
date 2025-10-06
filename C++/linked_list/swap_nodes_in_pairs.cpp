/*
Problem: Swap Nodes in Pairs
Topic: Linked List
Description: Given a linked list, swap every two adjacent nodes and return its head. You must solve the problem without modifying the values in the list's nodes (i.e., only nodes themselves may be changed.)
Time Complexity: O(n)
Space Complexity: O(1)
*/

class Solution {
public:
    ListNode* swapPairs(ListNode* head) {
        ListNode dummy(0, head);
        ListNode *prev = &dummy, *cur = head;

        while (cur && cur->next) {
            ListNode *npn = cur->next->next; // next pair's node
            ListNode *second = cur->next;

            // Swapping nodes
            second->next = cur;
            cur->next = npn;
            prev->next = second;

            // Move pointers for next swap
            prev = cur;
            cur = npn;
        }

        return dummy.next;        
    }
};