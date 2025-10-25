"""
Queue Implementation Using Two Stacks
Classic interview problem demonstrating stack-queue conversion

Approach 1: Expensive Enqueue - O(n) enqueue, O(1) dequeue
Approach 2: Expensive Dequeue - O(1) enqueue, Amortized O(1) dequeue
"""

class QueueUsingStacksExpensiveEnqueue:
    """
    Queue using two stacks - Expensive Enqueue approach
    
    Time Complexity:
        - Enqueue: O(n) - transfer all elements
        - Dequeue: O(1)
        - Peek: O(1)
    Space Complexity: O(n)
    """
    
    def __init__(self):
        """Initialize two stacks"""
        self.stack1 = []  # Main stack
        self.stack2 = []  # Temporary stack
    
    def is_empty(self):
        """Check if queue is empty"""
        return len(self.stack1) == 0
    
    def enqueue(self, item):
        """
        Add item to queue - O(n) operation
        Move all elements to stack2, push new item to stack1,
        then move everything back
        """
        # Move all elements from stack1 to stack2
        while self.stack1:
            self.stack2.append(self.stack1.pop())
        
        # Push new item to stack1
        self.stack1.append(item)
        
        # Move all elements back to stack1
        while self.stack2:
            self.stack1.append(self.stack2.pop())
    
    def dequeue(self):
        """Remove and return front element - O(1) operation"""
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        return self.stack1.pop()
    
    def peek(self):
        """Return front element without removing - O(1) operation"""
        if self.is_empty():
            raise IndexError("Peek from empty queue")
        return self.stack1[-1]
    
    def size(self):
        """Return number of elements"""
        return len(self.stack1)
    
    def __str__(self):
        """String representation"""
        return f"Queue({self.stack1[::-1]})"


class QueueUsingStacksExpensiveDequeue:
    """
    Queue using two stacks - Expensive Dequeue approach (Better!)
    
    Time Complexity:
        - Enqueue: O(1)
        - Dequeue: Amortized O(1), Worst case O(n)
        - Peek: Amortized O(1), Worst case O(n)
    Space Complexity: O(n)
    
    This is the preferred approach for most use cases.
    """
    
    def __init__(self):
        """Initialize two stacks"""
        self.inbox = []   # Stack for enqueue operations
        self.outbox = []  # Stack for dequeue operations
    
    def is_empty(self):
        """Check if queue is empty"""
        return len(self.inbox) == 0 and len(self.outbox) == 0
    
    def enqueue(self, item):
        """
        Add item to queue - O(1) operation
        Simply push to inbox stack
        """
        self.inbox.append(item)
    
    def _transfer(self):
        """
        Transfer elements from inbox to outbox
        Only done when outbox is empty
        """
        if not self.outbox:
            while self.inbox:
                self.outbox.append(self.inbox.pop())
    
    def dequeue(self):
        """
        Remove and return front element
        Amortized O(1) - each element moved at most once
        """
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        
        self._transfer()
        return self.outbox.pop()
    
    def peek(self):
        """
        Return front element without removing
        Amortized O(1)
        """
        if self.is_empty():
            raise IndexError("Peek from empty queue")
        
        self._transfer()
        return self.outbox[-1]
    
    def size(self):
        """Return number of elements"""
        return len(self.inbox) + len(self.outbox)
    
    def __str__(self):
        """String representation"""
        # Combine outbox (reversed) and inbox to show queue order
        elements = self.outbox[::-1] + self.inbox
        return f"Queue({elements})"


class QueueUsingStacksOptimized:
    """
    Optimized queue using stacks with detailed tracking
    Includes additional methods for analysis
    """
    
    def __init__(self):
        """Initialize with tracking metrics"""
        self.inbox = []
        self.outbox = []
        self.transfer_count = 0  # Track number of transfers
    
    def is_empty(self):
        """Check if queue is empty"""
        return len(self.inbox) == 0 and len(self.outbox) == 0
    
    def enqueue(self, item):
        """Add item to queue"""
        self.inbox.append(item)
    
    def dequeue(self):
        """Remove and return front element"""
        if self.is_empty():
            raise IndexError("Dequeue from empty queue")
        
        if not self.outbox:
            while self.inbox:
                self.outbox.append(self.inbox.pop())
            self.transfer_count += 1
        
        return self.outbox.pop()
    
    def peek(self):
        """Return front element without removing"""
        if self.is_empty():
            raise IndexError("Peek from empty queue")
        
        if not self.outbox:
            while self.inbox:
                self.outbox.append(self.inbox.pop())
            self.transfer_count += 1
        
        return self.outbox[-1]
    
    def size(self):
        """Return number of elements"""
        return len(self.inbox) + len(self.outbox)
    
    def get_transfer_count(self):
        """Return number of times elements were transferred"""
        return self.transfer_count
    
    def get_state(self):
        """Return current state of both stacks"""
        return {
            'inbox': self.inbox.copy(),
            'outbox': self.outbox.copy(),
            'transfers': self.transfer_count
        }
    
    def __str__(self):
        """String representation"""
        elements = self.outbox[::-1] + self.inbox
        return f"Queue({elements}, transfers={self.transfer_count})"


# Example usage and test cases
if __name__ == "__main__":
    print("=" * 60)
    print("Queue Using Stacks - Expensive Enqueue Approach")
    print("=" * 60)
    
    q1 = QueueUsingStacksExpensiveEnqueue()
    
    print("Enqueuing: 1, 2, 3, 4, 5")
    for i in range(1, 6):
        q1.enqueue(i)
        print(f"  After enqueue {i}: {q1}")
    
    print(f"\nFront element (peek): {q1.peek()}")
    print(f"Size: {q1.size()}")
    
    print("\nDequeuing 3 elements:")
    for _ in range(3):
        val = q1.dequeue()
        print(f"  Dequeued: {val}, Queue: {q1}")
    
    print("\n" + "=" * 60)
    print("Queue Using Stacks - Expensive Dequeue Approach (Better!)")
    print("=" * 60)
    
    q2 = QueueUsingStacksExpensiveDequeue()
    
    print("Enqueuing: 10, 20, 30, 40, 50")
    for i in [10, 20, 30, 40, 50]:
        q2.enqueue(i)
        print(f"  After enqueue {i}: {q2}")
    
    print(f"\nFront element (peek): {q2.peek()}")
    print(f"Size: {q2.size()}")
    
    print("\nDequeuing 2 elements:")
    for _ in range(2):
        val = q2.dequeue()
        print(f"  Dequeued: {val}, Queue: {q2}")
    
    print("\nEnqueuing: 60, 70")
    q2.enqueue(60)
    q2.enqueue(70)
    print(f"Queue: {q2}")
    
    print("\nDequeuing remaining elements:")
    while not q2.is_empty():
        print(f"  Dequeued: {q2.dequeue()}, Queue: {q2}")
    
    print("\n" + "=" * 60)
    print("Optimized Queue with Transfer Tracking")
    print("=" * 60)
    
    q3 = QueueUsingStacksOptimized()
    
    print("Enqueuing 6 elements:")
    for i in range(1, 7):
        q3.enqueue(i)
    
    print(f"Queue: {q3}")
    print(f"State: {q3.get_state()}")
    
    print("\nDequeuing 3 elements (triggers transfer):")
    for _ in range(3):
        val = q3.dequeue()
        print(f"  Dequeued: {val}")
        print(f"  State: {q3.get_state()}")
    
    print("\nEnqueuing 3 more elements:")
    for i in range(7, 10):
        q3.enqueue(i)
    
    print(f"Queue: {q3}")
    print(f"State: {q3.get_state()}")
    
    print("\nDequeuing all remaining elements:")
    while not q3.is_empty():
        val = q3.dequeue()
        print(f"  Dequeued: {val}, Transfers: {q3.get_transfer_count()}")
    
    print("\n" + "=" * 60)
    print("Performance Comparison")
    print("=" * 60)
    
    import time
    
    # Test expensive enqueue
    q_exp_enq = QueueUsingStacksExpensiveEnqueue()
    start = time.time()
    for i in range(1000):
        q_exp_enq.enqueue(i)
    enqueue_time = time.time() - start
    
    # Test expensive dequeue
    q_exp_deq = QueueUsingStacksExpensiveDequeue()
    start = time.time()
    for i in range(1000):
        q_exp_deq.enqueue(i)
    enqueue_time_opt = time.time() - start
    
    print(f"Expensive Enqueue - 1000 enqueues: {enqueue_time:.6f}s")
    print(f"Expensive Dequeue - 1000 enqueues: {enqueue_time_opt:.6f}s")
    print(f"\nExpensive Dequeue is ~{enqueue_time/enqueue_time_opt:.1f}x faster for enqueue!")
