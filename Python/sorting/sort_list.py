"""
148. Sort List

Given the head of a linked list, return the list after sorting it in ascending order.

We must do it in O(n log n) time and O(1) space (excluding recursion stack).

Approach:
- Use Merge Sort for linked lists.
- Find the middle using slow/fast pointers.
- Recursively divide and merge.

Example 1:
    Input: head = [4,2,1,3]
    Output: [1,2,3,4]

Example 2:
    Input: head = [-1,5,3,4,0]
    Output: [-1,0,3,4,5]

Example 3:
    Input: head = []
    Output: []

Constraints:
- The number of nodes in the list is in the range [0, 5 * 10^4].
- -10^5 <= Node.val <= 10^5
"""

# Definition for singly-linked list.
class ListNode(object):
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

class Solution(object):
    def sortList(self, head):
        """
        Sorts the linked list using Merge Sort.
        Time Complexity: O(n log n)
        Space Complexity: O(1) (excluding recursion)
        """
        # Base case: if list is empty or has one node
        if not head or not head.next:
            return head

        # Step 1: Split the list into two halves
        mid = self.getMid(head)
        left = head
        right = mid.next
        mid.next = None  # break the list

        # Step 2: Recursively sort both halves
        left = self.sortList(left)
        right = self.sortList(right)

        # Step 3: Merge two sorted halves
        return self.merge(left, right)

    def getMid(self, head):
        # Finds the middle node using slow/fast pointer
        slow, fast = head, head.next
        while fast and fast.next:
            slow = slow.next
            fast = fast.next.next
        return slow

    def merge(self, list1, list2):
        # Merges two sorted linked lists
        dummy = ListNode(0)
        tail = dummy

        while list1 and list2:
            if list1.val < list2.val:
                tail.next = list1
                list1 = list1.next
            else:
                tail.next = list2
                list2 = list2.next
            tail = tail.next

        tail.next = list1 if list1 else list2
        return dummy.next


# Example Usage
if __name__ == "__main__":
    def build_list(values):
        head = ListNode(values[0]) if values else None
        current = head
        for val in values[1:]:
            current.next = ListNode(val)
            current = current.next
        return head

    def print_list(head):
        res = []
        while head:
            res.append(head.val)
            head = head.next
        return res

    obj = Solution()

    # Test Cases
    test_cases = [
        [4, 2, 1, 3],
        [-1, 5, 3, 4, 0],
        []
    ]

    for case in test_cases:
        head = build_list(case)
        sorted_head = obj.sortList(head)
        print("Input List:", case)
        print("Sorted List:", print_list(sorted_head))
        print("-" * 30)
