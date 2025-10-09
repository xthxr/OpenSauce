/**
 * Definition for a singly-linked list node.
 */
class ListNode {
    int val;
    ListNode next;
    
    ListNode() {}
    
    ListNode(int val) { 
        this.val = val; 
    }
    
    ListNode(int val, ListNode next) { 
        this.val = val; 
        this.next = next; 
    }
}

/**
 * This class provides a solution for partitioning a linked list around a value x.
 * All nodes less than x come before nodes greater than or equal to x while
 * maintaining the relative order of nodes in each partition.
 * 
 * Example:
 * Input: head = [1,4,3,2,5,2], x = 3
 * Output: [1,2,2,4,3,5]
 * 
 * Time Complexity: O(n) where n is the number of nodes
 * Space Complexity: O(1) as we only use constant extra space
 */
public class PartitionList {
    
    /**
     * Partitions a linked list around a value x such that all nodes less than x come
     * before nodes greater than or equal to x while maintaining relative order.
     * 
     * @param head the head of the linked list to partition
     * @param x the value to partition around
     * @return the head of the partitioned linked list
     */
    public ListNode partition(ListNode head, int x) {
        // Handle empty list or single node
        if (head == null || head.next == null) {
            return head;
        }
        
        // Create dummy nodes for two partitions
        ListNode dummyLess = new ListNode(0);    // For nodes < x
        ListNode dummyGreater = new ListNode(0); // For nodes >= x
        
        // Pointers to build the two partitions
        ListNode currentLess = dummyLess;
        ListNode currentGreater = dummyGreater;
        ListNode current = head;
        
        // Traverse the original list and partition nodes
        while (current != null) {
            if (current.val < x) {
                currentLess.next = current;
                currentLess = currentLess.next;
            } else {
                currentGreater.next = current;
                currentGreater = currentGreater.next;
            }
            current = current.next;
        }
        
        // Terminate the greater partition
        currentGreater.next = null;
        
        // Connect the two partitions
        currentLess.next = dummyGreater.next;
        
        return dummyLess.next;
    }

    /**
     * Helper method to create a linked list from an array of integers.
     */
    private static ListNode createLinkedList(int[] arr) {
        if (arr == null || arr.length == 0) {
            return null;
        }
        
        ListNode dummy = new ListNode(0);
        ListNode current = dummy;
        for (int val : arr) {
            current.next = new ListNode(val);
            current = current.next;
        }
        return dummy.next;
    }

    /**
     * Helper method to convert a linked list to string for printing.
     */
    private static String linkedListToString(ListNode head) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        ListNode current = head;
        while (current != null) {
            sb.append(current.val);
            if (current.next != null) {
                sb.append(",");
            }
            current = current.next;
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * Main method to test the partition functionality with example cases.
     * 
     * @param args command line arguments (not used)
     */
    public static void main(String[] args) {
        PartitionList solution = new PartitionList();
        
        // Test case 1: Standard case
        int[] arr1 = {1, 4, 3, 2, 5, 2};
        ListNode head1 = createLinkedList(arr1);
        int x1 = 3;
        System.out.println("Test case 1:");
        System.out.println("Original list: " + linkedListToString(head1));
        System.out.println("Partition value: " + x1);
        ListNode result1 = solution.partition(head1, x1);
        System.out.println("Result: " + linkedListToString(result1));
        System.out.println();
        
        // Test case 2: All elements less than x
        int[] arr2 = {1, 2, 2, 1};
        ListNode head2 = createLinkedList(arr2);
        int x2 = 3;
        System.out.println("Test case 2:");
        System.out.println("Original list: " + linkedListToString(head2));
        System.out.println("Partition value: " + x2);
        ListNode result2 = solution.partition(head2, x2);
        System.out.println("Result: " + linkedListToString(result2));
        System.out.println();
        
        // Test case 3: All elements greater than or equal to x
        int[] arr3 = {3, 4, 3, 5};
        ListNode head3 = createLinkedList(arr3);
        int x3 = 3;
        System.out.println("Test case 3:");
        System.out.println("Original list: " + linkedListToString(head3));
        System.out.println("Partition value: " + x3);
        ListNode result3 = solution.partition(head3, x3);
        System.out.println("Result: " + linkedListToString(result3));
        System.out.println();
        
        // Test case 4: Single element
        int[] arr4 = {1};
        ListNode head4 = createLinkedList(arr4);
        int x4 = 2;
        System.out.println("Test case 4:");
        System.out.println("Original list: " + linkedListToString(head4));
        System.out.println("Partition value: " + x4);
        ListNode result4 = solution.partition(head4, x4);
        System.out.println("Result: " + linkedListToString(result4));
    }
}