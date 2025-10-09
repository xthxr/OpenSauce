/**
 * Problem: Linked List Cycle (Part 1 - Detection)
 *
 * Given the head of a singly linked list, determine if the linked list has a cycle in it.
 * There is a cycle if some node in the list can be reached again by continuously following the `next` pointer.
 *
 * This is famously solved using Floyd's Tortoise and Hare algorithm.
 */

// Helper class to represent a node in the linked list.
class ListNode {
  constructor(val, next = null) {
    this.val = val;
    this.next = next;
  }
}

/**
 * Determines if a linked list contains a cycle.
 * @param {ListNode} head The head node of the linked list.
 * @returns {boolean} True if a cycle exists, false otherwise.
 */
const hasCycle = (head) => {
  // If the list is empty or has only one node, it can't have a cycle.
  if (!head || !head.next) {
    return false;
  }

  // Initialize two pointers, 'slow' and 'fast'.
  // 'slow' moves one step at a time.
  let slow = head;
  // 'fast' moves two steps at a time.
  let fast = head;

  // Traverse the list. The loop continues as long as 'fast' and the node
  // after it are not null. This prevents errors when 'fast' is at the end.
  while (fast && fast.next) {
    // Move slow pointer one step.
    slow = slow.next;
    // Move fast pointer two steps.
    fast = fast.next.next;

    // If at any point the slow and fast pointers meet (point to the same node),
    // it means there is a cycle in the list.
    if (slow === fast) {
      return true;
    }
  }

  // If the loop finishes, it means the 'fast' pointer reached the end of the list.
  // Therefore, no cycle was found.
  return false;
};

// --- Example Usage ---

console.log("--- Linked List Cycle Detection Problem ---");

// Example 1: Create a linked list WITH a cycle
const node1 = new ListNode(3);
const node2 = new ListNode(2);
const node3 = new ListNode(0);
const node4 = new ListNode(-4);

node1.next = node2;
node2.next = node3;
node3.next = node4;
node4.next = node2; // Cycle! The last node points back to the second node.

const hasCycleResult1 = hasCycle(node1);
console.log("Testing list with a cycle...");
console.log(`Does the list have a cycle? ${hasCycleResult1}`); // Expected output: true
console.log("-" * 20);

// Example 2: Create a linked list WITHOUT a cycle
const nodeA = new ListNode(1);
const nodeB = new ListNode(2);
nodeA.next = nodeB;

const hasCycleResult2 = hasCycle(nodeA);
console.log("Testing list without a cycle...");
console.log(`Does the list have a cycle? ${hasCycleResult2}`); // Expected output: false
console.log("-" * 20);

// Example 3: Single node list
const nodeSingle = new ListNode(1);
const hasCycleResult3 = hasCycle(nodeSingle);
console.log("Testing a single-node list...");
console.log(`Does the list have a cycle? ${hasCycleResult3}`); // Expected output: false
