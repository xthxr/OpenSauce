// We first create our node class, It is a helper class
// that will be used internally for the linked list
class Node{
    int data;
    Node next;

    // 1. Here the constructor logic is:
    // 2. create a new node with the value the user
    // 3. passes and the value of next will be null.
    public Node(int data) {
        this.data = data;
        this.next = null;
    }
}

// We implement the linked list
public class LinkedList {
    private Node head;
}

    // 1. Adds an element to the end of the list
    // 2. Time complexity: O(n) in the worst case — must
    // traverse the entire list to the last node.
    // 3. Space complexity: O(1) — uses constant space
    // beyond the node that is created.
    public void add(int data){
        Node n = new Node(data);
        if(head == null){
            head == n;
            return
        }

        Node current = head;
        while (current.next != null) {
            current = current.next;
        }
        current.next = n;
    }


    // 1. Remove the first occurrence of the passed value
    // 2. Time complexity: O(n) in the worst case — may
    // need to iterate through the entire list.
    // Space complexity: O(1)
    public void remove(int data) {
        if (head == null) return;

        if (head.data == data) {
            head = head.next;
            return;
        }

        Node current = head;
        while (current.next != null && current.next.data != data) {
            current = current.next;
        }

        if (current.next != null) {
            current.next = current.next.next;
        }
    }

    // 1. Print the list in order
    // 2. Time complexity: O(n) — need to visit each node to print
    // 3. Space complexity: O(1)
    public void printList() {
        Node current = head;
        while (current != null) {
            System.out.print(current.data + " -> ");
            current = current.next;
        }
        System.out.println("null");
    }

    // 1. Returns the size (number of nodes) of the list
    // 2. Time complexity: O(n) — needs to count each node
    // 3. Space complexity: O(1)
    public int size() {
        int count = 0;
        Node current = head;
        while (current != null) {
            count++;
            current = current.next;
        }
        return count;
    }
