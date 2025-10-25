"""
Circular Queue Implementation
A circular queue is a linear data structure that uses a fixed-size array
where the last position is connected back to the first position.

Time Complexity: O(1) for all operations
Space Complexity: O(n) where n is the capacity
"""

class CircularQueue:
    """Fixed-size Circular Queue implementation"""
    
    def __init__(self, capacity):
        """
        Initialize circular queue with given capacity
        Args:
            capacity: Maximum number of elements in queue
        """
        self.capacity = capacity
        self.queue = [None] * capacity
        self.front = -1
        self.rear = -1
        self.count = 0
    
    def is_empty(self):
        """
        Check if queue is empty
        Returns: True if queue is empty, False otherwise
        """
        return self.count == 0
    
    def is_full(self):
        """
        Check if queue is full
        Returns: True if queue is full, False otherwise
        """
        return self.count == self.capacity
    
    def enqueue(self, item):
        """
        Add an item to the rear of the queue
        Args:
            item: Element to be added
        Raises: OverflowError if queue is full
        """
        if self.is_full():
            raise OverflowError("Queue is full")
        
        if self.front == -1:  # First element
            self.front = 0
        
        self.rear = (self.rear + 1) % self.capacity
        self.queue[self.rear] = item
        self.count += 1
    
    def dequeue(self):
        """
        Remove and return the front item from queue
        Returns: Front element of the queue
        Raises: IndexError if queue is empty
        """
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        
        item = self.queue[self.front]
        self.queue[self.front] = None  # Clear the slot
        
        if self.front == self.rear:  # Last element
            self.front = -1
            self.rear = -1
        else:
            self.front = (self.front + 1) % self.capacity
        
        self.count -= 1
        return item
    
    def peek(self):
        """
        Return the front item without removing it
        Returns: Front element of the queue
        Raises: IndexError if queue is empty
        """
        if self.is_empty():
            raise IndexError("Peek from empty queue")
        return self.queue[self.front]
    
    def size(self):
        """
        Return the number of items in queue
        Returns: Current size of the queue
        """
        return self.count
    
    def get_capacity(self):
        """
        Return the maximum capacity of queue
        Returns: Capacity of the queue
        """
        return self.capacity
    
    def display(self):
        """
        Display the queue elements
        Returns: List of current elements in order
        """
        if self.is_empty():
            return []
        
        elements = []
        if self.rear >= self.front:
            elements = self.queue[self.front:self.rear + 1]
        else:
            elements = self.queue[self.front:] + self.queue[:self.rear + 1]
        
        return elements
    
    def __str__(self):
        """String representation of circular queue"""
        return f"CircularQueue(capacity={self.capacity}, size={self.count}, elements={self.display()})"
    
    def __repr__(self):
        """Official string representation"""
        return self.__str__()


class DynamicCircularQueue:
    """
    Dynamic Circular Queue that resizes automatically
    Doubles capacity when full, halves when 1/4 full
    """
    
    def __init__(self, initial_capacity=8):
        """Initialize with initial capacity"""
        self.capacity = initial_capacity
        self.queue = [None] * initial_capacity
        self.front = 0
        self.rear = 0
        self.count = 0
    
    def is_empty(self):
        """Check if queue is empty"""
        return self.count == 0
    
    def is_full(self):
        """Check if queue is full"""
        return self.count == self.capacity
    
    def _resize(self, new_capacity):
        """Resize the queue to new capacity"""
        new_queue = [None] * new_capacity
        
        # Copy elements to new queue
        for i in range(self.count):
            new_queue[i] = self.queue[(self.front + i) % self.capacity]
        
        self.queue = new_queue
        self.capacity = new_capacity
        self.front = 0
        self.rear = self.count
    
    def enqueue(self, item):
        """Add item to queue, resize if needed"""
        if self.is_full():
            self._resize(self.capacity * 2)
        
        self.queue[self.rear] = item
        self.rear = (self.rear + 1) % self.capacity
        self.count += 1
    
    def dequeue(self):
        """Remove and return front item, resize if needed"""
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        
        item = self.queue[self.front]
        self.queue[self.front] = None
        self.front = (self.front + 1) % self.capacity
        self.count -= 1
        
        # Shrink if queue is 1/4 full and capacity > 8
        if self.count > 0 and self.count == self.capacity // 4 and self.capacity > 8:
            self._resize(self.capacity // 2)
        
        return item
    
    def peek(self):
        """Return front item without removing"""
        if self.is_empty():
            raise IndexError("Peek from empty queue")
        return self.queue[self.front]
    
    def size(self):
        """Return current size"""
        return self.count
    
    def __str__(self):
        """String representation"""
        elements = []
        for i in range(self.count):
            elements.append(self.queue[(self.front + i) % self.capacity])
        return f"DynamicCircularQueue(capacity={self.capacity}, size={self.count}, elements={elements})"


# Example usage and test cases
if __name__ == "__main__":
    print("=" * 50)
    print("Fixed-Size Circular Queue")
    print("=" * 50)
    
    # Create a circular queue with capacity 5
    cq = CircularQueue(5)
    print(f"Capacity: {cq.get_capacity()}")
    print(f"Is empty: {cq.is_empty()}")
    
    # Enqueue elements
    print("\nEnqueuing: 10, 20, 30, 40, 50")
    for i in [10, 20, 30, 40, 50]:
        cq.enqueue(i)
        print(f"  Enqueued {i}: {cq}")
    
    print(f"\nIs full: {cq.is_full()}")
    print(f"Front element: {cq.peek()}")
    
    # Try to enqueue when full
    print("\nTrying to enqueue 60 (should fail)...")
    try:
        cq.enqueue(60)
    except OverflowError as e:
        print(f"  Caught expected error: {e}")
    
    # Dequeue some elements
    print("\nDequeuing 2 elements:")
    print(f"  Dequeued: {cq.dequeue()}")  # 10
    print(f"  Dequeued: {cq.dequeue()}")  # 20
    print(f"Queue: {cq}")
    
    # Enqueue more elements (demonstrating circular nature)
    print("\nEnqueuing: 60, 70 (demonstrating wrap-around)")
    cq.enqueue(60)
    cq.enqueue(70)
    print(f"Queue: {cq}")
    print(f"Internal array: {cq.queue}")
    print(f"Front index: {cq.front}, Rear index: {cq.rear}")
    
    # Dequeue all elements
    print("\nDequeuing all elements:")
    while not cq.is_empty():
        print(f"  Dequeued: {cq.dequeue()}")
    
    print(f"Is empty: {cq.is_empty()}")
    
    print("\n" + "=" * 50)
    print("Dynamic Circular Queue")
    print("=" * 50)
    
    # Test dynamic resizing
    dq = DynamicCircularQueue(4)
    print(f"Initial capacity: {dq.capacity}")
    
    print("\nEnqueuing 1-8 (should trigger resize):")
    for i in range(1, 9):
        dq.enqueue(i)
        print(f"  After enqueue {i}: capacity={dq.capacity}, size={dq.size()}")
    
    print(f"\nQueue: {dq}")
    
    print("\nDequeuing 6 elements (should trigger shrink):")
    for _ in range(6):
        val = dq.dequeue()
        print(f"  Dequeued {val}: capacity={dq.capacity}, size={dq.size()}")
    
    print(f"\nFinal queue: {dq}")
    
    print("\n" + "=" * 50)
    print("Circular Queue Use Case: Round Robin Scheduling")
    print("=" * 50)
    
    # Simulate round-robin process scheduling
    processes = CircularQueue(5)
    process_names = ["P1", "P2", "P3", "P4", "P5"]
    
    print("Adding processes to queue:")
    for name in process_names:
        processes.enqueue(name)
        print(f"  Added {name}")
    
    print("\nSimulating 10 time slices (round-robin):")
    for i in range(10):
        process = processes.dequeue()
        print(f"  Time slice {i+1}: Executing {process}")
        processes.enqueue(process)  # Re-add to end of queue
    
    print(f"\nFinal queue state: {processes}")
