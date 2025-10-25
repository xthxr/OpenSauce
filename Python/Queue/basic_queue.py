"""
Basic Queue Implementation using Array (List)
Time Complexity: 
    - Enqueue: O(1)
    - Dequeue: O(n) due to list.pop(0)
    - Peek: O(1)
    - Size: O(1)
Space Complexity: O(n)
"""

class Queue:
    """Array-based Queue implementation"""
    
    def __init__(self):
        """Initialize an empty queue"""
        self.items = []
    
    def is_empty(self):
        """
        Check if queue is empty
        Returns: True if queue is empty, False otherwise
        """
        return len(self.items) == 0
    
    def enqueue(self, item):
        """
        Add an item to the rear of the queue
        Args:
            item: Element to be added
        """
        self.items.append(item)
    
    def dequeue(self):
        """
        Remove and return the front item from queue
        Returns: Front element of the queue
        Raises: IndexError if queue is empty
        """
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        return self.items.pop(0)
    
    def peek(self):
        """
        Return the front item without removing it
        Returns: Front element of the queue
        Raises: IndexError if queue is empty
        """
        if self.is_empty():
            raise IndexError("Peek from empty queue")
        return self.items[0]
    
    def size(self):
        """
        Return the number of items in queue
        Returns: Size of the queue
        """
        return len(self.items)
    
    def __str__(self):
        """String representation of queue"""
        return f"Queue({self.items})"
    
    def __repr__(self):
        """Official string representation"""
        return self.__str__()


class OptimizedQueue:
    """
    Optimized Queue using collections.deque
    Time Complexity: O(1) for all operations
    Space Complexity: O(n)
    """
    
    def __init__(self):
        """Initialize an empty queue using deque"""
        from collections import deque
        self.items = deque()
    
    def is_empty(self):
        """Check if queue is empty"""
        return len(self.items) == 0
    
    def enqueue(self, item):
        """Add an item to the rear of the queue"""
        self.items.append(item)
    
    def dequeue(self):
        """Remove and return the front item from queue"""
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        return self.items.popleft()
    
    def peek(self):
        """Return the front item without removing it"""
        if self.is_empty():
            raise IndexError("Peek from empty queue")
        return self.items[0]
    
    def size(self):
        """Return the number of items in queue"""
        return len(self.items)
    
    def __str__(self):
        """String representation of queue"""
        return f"Queue({list(self.items)})"
    
    def __repr__(self):
        """Official string representation"""
        return self.__str__()


# Example usage and test cases
if __name__ == "__main__":
    print("=" * 50)
    print("Basic Queue Implementation")
    print("=" * 50)
    
    # Test basic queue
    q = Queue()
    print(f"Is empty: {q.is_empty()}")  # True
    
    # Enqueue elements
    print("\nEnqueuing: 10, 20, 30, 40, 50")
    for i in [10, 20, 30, 40, 50]:
        q.enqueue(i)
    
    print(f"Queue: {q}")
    print(f"Size: {q.size()}")
    print(f"Front element (peek): {q.peek()}")
    
    # Dequeue elements
    print(f"\nDequeued: {q.dequeue()}")  # 10
    print(f"Dequeued: {q.dequeue()}")  # 20
    print(f"Queue after dequeuing: {q}")
    print(f"Size: {q.size()}")
    
    # Add more elements
    print("\nEnqueuing: 60, 70")
    q.enqueue(60)
    q.enqueue(70)
    print(f"Queue: {q}")
    
    print("\n" + "=" * 50)
    print("Optimized Queue (using deque)")
    print("=" * 50)
    
    # Test optimized queue
    opt_q = OptimizedQueue()
    print(f"Is empty: {opt_q.is_empty()}")
    
    print("\nEnqueuing: A, B, C, D, E")
    for char in ['A', 'B', 'C', 'D', 'E']:
        opt_q.enqueue(char)
    
    print(f"Queue: {opt_q}")
    print(f"Size: {opt_q.size()}")
    print(f"Front element (peek): {opt_q.peek()}")
    
    print(f"\nDequeued: {opt_q.dequeue()}")  # A
    print(f"Dequeued: {opt_q.dequeue()}")  # B
    print(f"Queue after dequeuing: {opt_q}")
    
    # Test empty queue error handling
    print("\n" + "=" * 50)
    print("Error Handling Test")
    print("=" * 50)
    
    empty_q = Queue()
    try:
        empty_q.dequeue()
    except IndexError as e:
        print(f"Caught expected error: {e}")
    
    try:
        empty_q.peek()
    except IndexError as e:
        print(f"Caught expected error: {e}")
