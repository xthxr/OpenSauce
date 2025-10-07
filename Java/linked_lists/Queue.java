// Queue for storing any type of data. Data can be of any type LinkedList, Integer, String etc. In the form of LinkedList.
// T represents the "Type" parameter, a placeholder for any data type.
class Queue<T> {

    // This represents a node in the linked list.
    // It's a static inner class because it doesn't need to access instance members of the outer Queue class.
    static class qNode<T> {
        private T data; // Data part of the node.
        private qNode<T> next; // Reference to the next node in the list.

        // Constructor for the Node class.
        qNode(T data) {
            this.data = data;
            this.next = null; // By default, a new node points to null.
        }
    }

    private qNode<T> front; // Points to the front of the queue (the first element).
    private qNode<T> rear;  // Points to the rear of the queue (the last element).

    /**
     * Checks if the queue is empty.
     * The queue is empty if the 'front' pointer is null.
     * * Time Complexity: O(1) - It's a single comparison operation.
     * Space Complexity: O(1) - No extra space is used.
     * * @return true if the queue is empty, false otherwise.
     */
    public boolean isEmpty() {
        if (front == null) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Adds an element to the rear of the queue.
     * This operation involves creating a new node and updating the 'rear' pointer.
     * * Time Complexity: O(1) - All operations (node creation, pointer assignments) take constant time.
     * Space Complexity: O(1) - We only use constant extra space for the new node.
     * * @param data The data to be added to the queue.
     */
    public void enqueue(T data) {
        // Create a new node with the given data.
        qNode<T> node = new qNode<>(data);

        // If the queue is not empty, link the current rear node to the new node.
        if (rear != null) {
            rear.next = node;
        }
        
        // Update the rear pointer to be the new node.
        rear = node;

        // If the queue was initially empty, the new node is both the front and the rear.
        if (front == null) {
            front = rear;
        }
    }

    /**
     * Removes and returns the element from the front of the queue.
     * This follows the First-In, First-Out (FIFO) principle.
     * * Time Complexity: O(1) - It involves a few pointer manipulations which are constant time operations.
     * Space Complexity: O(1) - No extra space is used.
     * * @return The data from the front of the queue, or null if the queue is empty.
     */
    public T dequeu() {
        // Check if the queue is empty before trying to remove an element.
        if (isEmpty()) {
            System.out.println("Queue is null.");
            return null;
        }

        // Store the data from the front node to return it later.
        T data = front.data;
        
        // Move the front pointer to the next node in the queue.
        front = front.next;

        // If moving the front pointer makes it null, it means the queue is now empty.
        // In this case, we must also set the rear pointer to null.
        if (front == null) {
            rear = null;
        }
        
        // Return the removed data.
        return data;
    }
}

// This Kind of Queue is useful for levelorder traversal of trees.