class Node:
    def __init__(self, data):
        self.data = data
        self.next = None


class LinkedList:
    def __init__(self):
        self.head = None

    # Insert at beginning
    def insert_at_beginning(self, data):
        new_node = Node(data)
        new_node.next = self.head
        self.head = new_node

    # Insert at end
    def insert_at_end(self, data):
        new_node = Node(data)
        if not self.head:
            self.head = new_node
            return
        last = self.head
        while last.next:
            last = last.next
        last.next = new_node

    # Delete a node by value
    def delete_node(self, key):
        temp = self.head

        # If head node itself holds the key
        if temp and temp.data == key:
            self.head = temp.next
            temp = None
            return

        # Search for the key
        prev = None
        while temp and temp.data != key:
            prev = temp
            temp = temp.next

        if temp is None:
            print("Key not found")
            return

        prev.next = temp.next
        temp = None

    # Search for a node
    def search(self, key):
        current = self.head
        while current:
            if current.data == key:
                return True
            current = current.next
        return False

    # Display the list
    def display(self):
        current = self.head
        while current:
            print(current.data, end=" -> ")
            current = current.next
        print("None")

    # Find length
    def length(self):
        count = 0
        current = self.head
        while current:
            count += 1
            current = current.next
        return count

if __name__ == "__main__":
    ll = LinkedList()

    ll.insert_at_end(10)
    ll.insert_at_end(20)
    ll.insert_at_beginning(5)
    ll.insert_at_end(30)

    ll.display()  # Output: 5 -> 10 -> 20 -> 30 -> None

    print("Length:", ll.length())  # Output: 4

    ll.delete_node(20)
    ll.display()  # Output: 5 -> 10 -> 30 -> None

    print("Search 10:", ll.search(10))  # True
    print("Search 50:", ll.search(50))  # False
import math