/*
 * 1. Brief description of the algorithm
 * This algorithm detects if a singly linked list has a cycle in it. A cycle
 * exists if any node can be reached again by continuously following the `next`
 * pointer. The problem is solved using Floyd's Tortoise and Hare algorithm,
 * which uses two pointers moving at different speeds.
 *
 * 2. Time complexity: O(n)
 * In the worst-case scenario (a non-cyclical list), the fast pointer traverses
 * the entire list. In a cyclical list, the pointers are guaranteed to meet
 * within a finite number of steps proportional to the list's length.
 *
 * 3. Space complexity: O(1)
 * The algorithm uses only two pointers (slow and fast), so the memory usage
 * is constant regardless of the size of the linked list.
 */

// 4. Implementation

/**
 * Helper class to represent a node in the linked list.
 */
class ListNode {
  constructor(val, next = null) {
    this.val = val;
    this.next = next;
  }
}

/**
 * Determines if a linked list contains a cycle using Floyd's Tortoise and Hare algorithm.
 * @param {ListNode} head The head node of the linked list.
 * @returns {boolean} True if a cycle exists, false otherwise.
 */
const hasCycle = (head) => {
  // If the list is empty or has only one node, it can't have a cycle.
  if (!head || !head.next) {
    return false;
  }

  // Initialize two pointers, 'slow' (tortoise) and 'fast' (hare).
  let slow = head;
  let fast = head;

  // Traverse the list as long as the fast pointer and its next node are not null.
  while (fast && fast.next) {
    // The slow pointer moves one step.
    slow = slow.next;
    // The fast pointer moves two steps.
    fast = fast.next.next;

    // If the pointers meet, a cycle has been detected.
    if (slow === fast) {
      return true;
    }
  }

  // If the loop completes, the fast pointer reached the end, so there is no cycle.
  return false;
};

// 5. Example usage/test cases

console.log("--- Linked List Cycle Detection Problem ---");

// Example 1: Create a linked list WITH a cycle
const node1 = new ListNode(3);
const node2 = new ListNode(2);
const node3 = new ListNode(0);
const node4 = new ListNode(-4);

node1.next = node2;
node2.next = node3;
node3.next = node4;
node4.next = node2; // Cycle! Last node points back to the second node.

console.log("\nTesting list with a cycle...");
const hasCycleResult1 = hasCycle(node1);
console.log(`Does the list have a cycle? ${hasCycleResult1}`); // Expected: true

// Example 2: Create a linked list WITHOUT a cycle
const nodeA = new ListNode(1);
const nodeB = new ListNode(2);
nodeA.next = nodeB;

console.log("\nTesting list without a cycle...");
const hasCycleResult2 = hasCycle(nodeA);
console.log(`Does the list have a cycle? ${hasCycleResult2}`); // Expected: false

// Example 3: Single node list
const nodeSingle = new ListNode(1);

console.log("\nTesting a single-node list...");
const hasCycleResult3 = hasCycle(nodeSingle);
console.log(`Does the list have a cycle? ${hasCycleResult3}`); // Expected: false
