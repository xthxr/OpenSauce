
class Node:
    """Represents a single node in a Doubly Linked List."""

    def __init__(self, data):
        self.data = data
        self.prev = None
        self.next = None

class DoublyLinkedList:
    """Implementation of a Doubly Linked List in Python."""

    def __init__(self):
        self.head = None

    def insert_at_beginning(self, data):
        """Insert a new node at the beginning of the list."""
        new_node = Node(data)
        if self.head is None:
            self.head = new_node
            return
        new_node.next = self.head
        self.head.prev = new_node
        self.head = new_node

    def insert_at_end(self, data):
        """Insert a new node at the end of the list."""
        new_node = Node(data)
        if self.head is None:
            self.head = new_node
            return
        itr = self.head
        while itr.next:
            itr = itr.next
        itr.next = new_node
        new_node.prev = itr

    def insert_after_value(self, target_value, data):
        """Insert a new node after a given target value."""
        itr = self.head
        while itr:
            if itr.data == target_value:
                new_node = Node(data)
                new_node.next = itr.next
                new_node.prev = itr
                if itr.next:
                    itr.next.prev = new_node
                itr.next = new_node
                return
            itr = itr.next
        print(f"Value {target_value} not found in the list.")

    def remove_by_value(self, data):
        """Remove the first occurrence of a node with the given data."""
        if self.head is None:
            print("List is empty.")
            return

        itr = self.head

        # Case 1: Removing head node
        if itr.data == data:
            if itr.next:
                itr.next.prev = None
            self.head = itr.next
            itr = None
            return

        # Case 2: Removing a non-head node
        while itr:
            if itr.data == data:
                if itr.prev:
                    itr.prev.next = itr.next
                if itr.next:
                    itr.next.prev = itr.prev
                itr = None
                return
            itr = itr.next
        print(f"Value {data} not found in the list.")

    def print_forward(self):
        """Display the list from head to tail."""
        if self.head is None:
            print("List is empty.")
            return
        itr = self.head
        dll_str = ''
        while itr:
            dll_str += str(itr.data) + ' <-> '
            itr = itr.next
        print(dll_str.rstrip(' <-> '))

    def print_backward(self):
        """Display the list from tail to head."""
        if self.head is None:
            print("List is empty.")
            return
        itr = self.head
        while itr.next:
            itr = itr.next  # Move to the tail

        dll_str = ''
        while itr:
            dll_str += str(itr.data) + ' <-> '
            itr = itr.prev
        print(dll_str.rstrip(' <-> '))

    def get_length(self):
        """Return the total number of nodes in the list."""
        count = 0
        itr = self.head
        while itr:
            count += 1
            itr = itr.next
        return count


if __name__ == "__main__":
    # Example usage
    dll = DoublyLinkedList()
    dll.insert_at_end(10)
    dll.insert_at_end(20)
    dll.insert_at_beginning(5)
    dll.insert_after_value(10, 15)

    print("Forward traversal:")
    dll.print_forward()    # Output: 5 <-> 10 <-> 15 <-> 20

    print("Backward traversal:")
    dll.print_backward()   # Output: 20 <-> 15 <-> 10 <-> 5

    print(f"Length: {dll.get_length()}")

    dll.remove_by_value(15)
    dll.print_forward()    # Output: 5 <-> 10 <-> 20
