import java.util.*;

/**
 * Problem: Merge k Sorted Linked Lists
 *
 * LeetCode: 23
 *
 * Approach:
 * - Use Min-Heap to keep track of smallest head node among lists
 * - Repeatedly extract min, add to result list, push next node from same list
 *
 * Time Complexity: O(N log k) 
 *   -> N = total nodes, k = number of lists
 * Space Complexity: O(k) for heap
 */
public class MergeKSortedLists {

    // Definition for singly-linked list
    static class ListNode {
        int val;
        ListNode next;
        ListNode(int x) { val = x; }
    }

    public static ListNode mergeKLists(ListNode[] lists) {
        // Step 1: Min-Heap (compare by node value)
        PriorityQueue<ListNode> heap = new PriorityQueue<>(Comparator.comparingInt(a -> a.val));

        // Step 2: Add all list heads
        for (ListNode node : lists) {
            if (node != null) heap.add(node);
        }

        // Dummy head for result
        ListNode dummy = new ListNode(0);
        ListNode tail = dummy;

        // Step 3: Extract smallest, add to result, push next node
        while (!heap.isEmpty()) {
            ListNode min = heap.poll();
            tail.next = min;
            tail = tail.next;

            if (min.next != null) {
                heap.add(min.next);
            }
        }
        return dummy.next;
    }

    // Utility to build a linked list from array
    public static ListNode buildList(int[] arr) {
        ListNode dummy = new ListNode(0);
        ListNode cur = dummy;
        for (int num : arr) {
            cur.next = new ListNode(num);
            cur = cur.next;
        }
        return dummy.next;
    }

    // Utility to print linked list
    public static void printList(ListNode head) {
        while (head != null) {
            System.out.print(head.val + " -> ");
            head = head.next;
        }
        System.out.println("null");
    }

    public static void main(String[] args) {
        System.out.println("=== Merge K Sorted Lists (LC 23) ===");

        ListNode[] lists = new ListNode[3];
        lists[0] = buildList(new int[]{1,4,5});
        lists[1] = buildList(new int[]{1,3,4});
        lists[2] = buildList(new int[]{2,6});

        ListNode merged = mergeKLists(lists);
        System.out.print("Merged List: ");
        printList(merged);
    }
}
