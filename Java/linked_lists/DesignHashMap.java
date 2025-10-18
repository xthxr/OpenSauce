/**
 * LeetCode 706 - Design HashMap
 *
 * Time Complexity: O(1) average, O(n) worst case for collisions
 * Space Complexity: O(n)
 *
 * Implementation using separate chaining with linked lists for collision resolution.
 */

public class DesignHashMap {
    private static final int SIZE = 1000;
    private ListNode[] buckets;

    // ListNode for separate chaining
    private class ListNode {
        int key;
        int value;
        ListNode next;

        ListNode(int key, int value) {
            this.key = key;
            this.value = value;
        }

        ListNode() {
            // Dummy head node constructor
        }
    }

    public DesignHashMap() {
        buckets = new ListNode[SIZE];
        // Initialize with dummy head nodes
        for (int i = 0; i < SIZE; i++) {
            buckets[i] = new ListNode();
        }
    }

    private int hash(int key) {
        return key % SIZE;
    }

    public void put(int key, int value) {
        int index = hash(key);
        ListNode current = buckets[index];

        // Search for existing key
        while (current.next != null) {
            if (current.next.key == key) {
                current.next.value = value; // Update existing
                return;
            }
            current = current.next;
        }

        // Add new node at the end
        current.next = new ListNode(key, value);
    }

    public int get(int key) {
        int index = hash(key);
        ListNode current = buckets[index].next; // Skip dummy head

        while (current != null) {
            if (current.key == key) {
                return current.value;
            }
            current = current.next;
        }
        return -1; // Key not found
    }

    public void remove(int key) {
        int index = hash(key);
        ListNode current = buckets[index];

        // Look for the node to remove
        while (current.next != null) {
            if (current.next.key == key) {
                current.next = current.next.next; // Remove from chain
                return;
            }
            current = current.next;
        }
    }

    // Test cases
    public static void main(String[] args) {
        DesignHashMap hashMap = new DesignHashMap();

        // Test basic operations
        hashMap.put(1, 1);
        hashMap.put(2, 2);
        System.out.println("Get 1: " + hashMap.get(1));    // Expected: 1
        System.out.println("Get 3: " + hashMap.get(3));    // Expected: -1

        hashMap.put(2, 1); // Update existing key
        System.out.println("Get 2 after update: " + hashMap.get(2)); // Expected: 1

        hashMap.remove(2); // Remove key
        System.out.println("Get 2 after remove: " + hashMap.get(2)); // Expected: -1

        // Test collision handling
        hashMap.put(1, 1000);  // Update key 1
        hashMap.put(1001, 42); // Same bucket as 1 (1001 % 1000 = 1)
        System.out.println("Get 1001: " + hashMap.get(1001)); // Expected: 42
        System.out.println("Get 1 after collision: " + hashMap.get(1)); // Expected: 1000
    }
}