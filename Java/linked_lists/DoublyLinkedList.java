/**
 * DoublyLinkedList.java
 *
 * This program demonstrates the implementation of a Doubly Linked List in Java.
 * Each node contains three parts: data, a reference to the next node, and a reference to the previous node.
 *
 * Operations implemented:
 *  1. Insertion at beginning, end, and at a given position
 *  2. Deletion from beginning, end, and at a given position
 *  3. Traversal (Forward & Backward)
 *  4. Searching for an element
 *
 * Time and Space Complexities:
 *  ➤ Insertion: O(1) at head/tail, O(n) at position
 *  ➤ Deletion: O(1) at head/tail, O(n) at position
 *  ➤ Traversal: O(n)
 *  ➤ Searching: O(n)
 *  ➤ Space Complexity: O(n) for n nodes
 *
 * Author: Pradyumn Pratap Singh (Strange)
 * For: Hacktoberfest / Open Source Contribution
 */

public class DoublyLinkedList {

    // Node definition for Doubly Linked List
    static class Node {
        int data;
        Node prev, next;

        Node(int data) {
            this.data = data;
        }
    }

    private Node head;

    /**
     * Inserts a new node at the beginning.
     */
    public void insertAtBeginning(int data) {
        Node newNode = new Node(data);
        if (head != null) {
            newNode.next = head;
            head.prev = newNode;
        }
        head = newNode;
    }

    /**
     * Inserts a new node at the end.
     */
    public void insertAtEnd(int data) {
        Node newNode = new Node(data);
        if (head == null) {
            head = newNode;
            return;
        }

        Node temp = head;
        while (temp.next != null)
            temp = temp.next;

        temp.next = newNode;
        newNode.prev = temp;
    }

    /**
     * Inserts a new node at a given position (0-based index).
     */
    public void insertAtPosition(int data, int position) {
        if (position == 0) {
            insertAtBeginning(data);
            return;
        }

        Node newNode = new Node(data);
        Node temp = head;
        for (int i = 0; i < position - 1 && temp != null; i++)
            temp = temp.next;

        if (temp == null) {
            System.out.println("Position out of bounds!");
            return;
        }

        newNode.next = temp.next;
        newNode.prev = temp;
        if (temp.next != null)
            temp.next.prev = newNode;
        temp.next = newNode;
    }

    /**
     * Deletes the first node.
     */
    public void deleteAtBeginning() {
        if (head == null)
            return;

        head = head.next;
        if (head != null)
            head.prev = null;
    }

    /**
     * Deletes the last node.
     */
    public void deleteAtEnd() {
        if (head == null)
            return;

        if (head.next == null) {
            head = null;
            return;
        }

        Node temp = head;
        while (temp.next != null)
            temp = temp.next;

        temp.prev.next = null;
    }

    /**
     * Deletes node at a given position.
     */
    public void deleteAtPosition(int position) {
        if (head == null)
            return;

        if (position == 0) {
            deleteAtBeginning();
            return;
        }

        Node temp = head;
        for (int i = 0; i < position && temp != null; i++)
            temp = temp.next;

        if (temp == null) {
            System.out.println("Position out of bounds!");
            return;
        }

        if (temp.prev != null)
            temp.prev.next = temp.next;

        if (temp.next != null)
            temp.next.prev = temp.prev;
    }

    /**
     * Searches for an element in the list.
     * @return true if found, false otherwise.
     */
    public boolean search(int key) {
        Node temp = head;
        while (temp != null) {
            if (temp.data == key)
                return true;
            temp = temp.next;
        }
        return false;
    }

    /**
     * Traverses and prints the list in forward direction.
     */
    public void traverseForward() {
        Node temp = head;
        System.out.print("Forward Traversal: ");
        while (temp != null) {
            System.out.print(temp.data + " ");
            temp = temp.next;
        }
        System.out.println();
    }

    /**
     * Traverses and prints the list in backward direction.
     */
    public void traverseBackward() {
        if (head == null)
            return;

        Node temp = head;
        while (temp.next != null)
            temp = temp.next;

        System.out.print("Backward Traversal: ");
        while (temp != null) {
            System.out.print(temp.data + " ");
            temp = temp.prev;
        }
        System.out.println();
    }

    // ---------------- TEST CASES ----------------
    public static void main(String[] args) {
        DoublyLinkedList list = new DoublyLinkedList();

        list.insertAtEnd(10);
        list.insertAtEnd(20);
        list.insertAtEnd(30);
        list.insertAtEnd(40);

        list.traverseForward();
        list.traverseBackward();

        System.out.println("\nInsert 5 at beginning:");
        list.insertAtBeginning(5);
        list.traverseForward();

        System.out.println("\nInsert 25 at position 3:");
        list.insertAtPosition(25, 3);
        list.traverseForward();

        System.out.println("\nAfter deleting first element:");
        list.deleteAtBeginning();
        list.traverseForward();

        System.out.println("\nAfter deleting element at position 2:");
        list.deleteAtPosition(2);
        list.traverseForward();

        System.out.println("\nAfter deleting last element:");
        list.deleteAtEnd();
        list.traverseForward();

        int key = 25;
        System.out.println("\nSearching for " + key + ": " + (list.search(key) ? "Found" : "Not Found"));
    }
}
