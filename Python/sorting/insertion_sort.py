"""
147. Insertion Sort List

Given the head of a singly linked list, sort the list using insertion sort,
and return the sorted list's head.

Algorithm steps:
1. Insertion sort iterates, consuming one element each time and growing a sorted list.
2. Each element is placed in its correct position relative to the sorted part.

Example 1:
    Input: head = [4,2,1,3]
    Output: [1,2,3,4]

Example 2:
    Input: head = [-1,5,3,4,0]
    Output: [-1,0,3,4,5]

Constraints:
- The number of nodes in the list is in the range [1, 5000].
- -5000 <= Node.val <= 5000
"""

# Definition for singly-linked list.
class ListNode(object):
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

class Solution(object):
    def insertionSortList(self, head):
        """
        Sorts a linked list using insertion sort.
        Time Complexity: O(n^2)
        Space Complexity: O(1)
        """
        # Dummy node to simplify insertion at the beginning
        dummy = ListNode(0)
        current = head

        while current:
            # At each iteration, we insert current into the sorted list.
            prev = dummy
            # Find the correct insertion position
            while prev.next and prev.next.val < current.val:
                prev = prev.next

            # Store next node to process
            next_temp = current.next
            # Insert current node in sorted part
            current.next = prev.next
            prev.next = current
            # Move to the next node
            current = next_temp

        return dummy.next


# Example Usage
if __name__ == "__main__":
    def build_list(values):
        head = ListNode(values[0])
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
        [-1, 5, 3, 4, 0]
    ]

    for case in test_cases:
        head = build_list(case)
        sorted_head = obj.insertionSortList(head)
        print("Input List:", case)
        print("Sorted List:", print_list(sorted_head))
        print("-" * 30)
