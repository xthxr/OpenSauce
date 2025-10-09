/**
 * Problem: Linked List Cycle (Part 2 - Find Start)
 *
 * Given the head of a linked list, return the node where the cycle begins.
 * If there is no cycle, return null.
 *
 * This solution builds on Floyd's Tortoise and Hare algorithm.
 * 1. First, find the intersection point of the slow and fast pointers.
 * 2. If they don't meet, there's no cycle.
 * 3. If they do meet, move one pointer back to the head of the list.
 * 4. Move both pointers one step at a time. The node where they meet again
 * is the start of the cycle.
 */

// Helper class to represent a node in the linked list.
class ListNode {
  constructor(val, next = null) {
    this.val = val;
    this.next = next;
  }
}

/**
 * Finds the starting node of a cycle in a linked list.
 * @param {ListNode} head The head node of the linked list.
 * @returns {ListNode | null} The starting node of the cycle, or null if no cycle exists.
 */
const detectCycleStart = (head) => {
  // If the list is empty or has only one node, no cycle can exist.
  if (!head || !head.next) {
    return null;
  }

  let slow = head;
  let fast = head;
  let intersection = null;

  // Step 1: Find the intersection point.
  while (fast && fast.next) {
    slow = slow.next;
    fast = fast.next.next;
    if (slow === fast) {
      intersection = slow;
      break; // Cycle detected, intersection found.
    }
  }

  // Step 2: If there was no intersection, there is no cycle.
  if (!intersection) {
    return null;
  }

  // Step 3: Move one pointer back to the head.
  let p1 = head;
  let p2 = intersection;

  // Step 4: Move both pointers one step at a time until they meet.
  // The meeting point is the start of the cycle.
  while (p1 !== p2) {
    p1 = p1.next;
    p2 = p2.next;
  }

  return p1; // or p2, as they are the same node.
};


// --- Example Usage ---

console.log("--- Linked List Cycle Start Node Problem ---");

// Example 1: Create a list where the cycle starts at node with value 2.
const node1 = new ListNode(3);
const node2 = new ListNode(2);
const node3 = new ListNode(0);
const node4 = new ListNode(-4);

node1.next = node2;
node2.next = node3;
node3.next = node4;
node4.next = node2; // Cycle starts at node2 (value 2).

const cycleStartNode1 = detectCycleStart(node1);
console.log("Testing list with a cycle...");
if (cycleStartNode1) {
  console.log(`Cycle starts at node with value: ${cycleStartNode1.val}`); // Expected output: 2
} else {
  console.log("No cycle found.");
}
console.log("-" * 20);

// Example 2: Create a list with no cycle.
const nodeA = new ListNode(1);
const nodeB = new ListNode(2);
nodeA.next = nodeB;

const cycleStartNode2 = detectCycleStart(nodeA);
console.log("Testing list without a cycle...");
if (cycleStartNode2) {
  console.log(`Cycle starts at node with value: ${cycleStartNode2.val}`);
} else {
  console.log("No cycle found."); // Expected output: No cycle found.
}
