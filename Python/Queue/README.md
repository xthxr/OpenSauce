# Queue Data Structure - Python Implementations

A comprehensive collection of Queue implementations and related problems in Python.

## 📚 Table of Contents

- [Overview](#overview)
- [Files in This Directory](#files-in-this-directory)
- [Queue Operations](#queue-operations)
- [Time Complexity](#time-complexity)
- [When to Use Queues](#when-to-use-queues)
- [Implementation Comparison](#implementation-comparison)
- [Common Applications](#common-applications)
- [How to Use](#how-to-use)

## 🎯 Overview

A **Queue** is a linear data structure that follows the **FIFO (First In First Out)** principle. Elements are added at the rear (enqueue) and removed from the front (dequeue), similar to a real-world queue or line.

```
Front                    Rear
  ↓                        ↓
[10] ← [20] ← [30] ← [40] ← [50]
  ↑                        ↑
Dequeue                 Enqueue
```

## 📁 Files in This Directory

### 1. `basic_queue.py`
- **Basic Queue**: Simple array-based implementation
- **Optimized Queue**: Using `collections.deque` for O(1) operations
- Includes comprehensive examples and test cases

**Key Features:**
- Enqueue (add element)
- Dequeue (remove element)
- Peek (view front element)
- Size and empty checks

### 2. `circular_queue.py`
- **Fixed-Size Circular Queue**: Efficient use of fixed-size array
- **Dynamic Circular Queue**: Auto-resizing circular queue
- Demonstrates wrap-around concept
- Includes round-robin scheduling example

**Key Features:**
- Efficient space utilization
- Constant time operations
- No wasted space after dequeue
- Use case: CPU scheduling, buffer management

### 3. `queue_using_stacks.py`
- **Classic Interview Problem**: Implement queue using two stacks
- **Three Approaches**:
  - Expensive Enqueue: O(n) enqueue, O(1) dequeue
  - Expensive Dequeue: O(1) enqueue, Amortized O(1) dequeue (recommended)
  - Optimized version with transfer tracking

**Key Concepts:**
- Stack-to-queue conversion
- Amortized analysis
- Trade-offs between operations

### 4. `sliding_window_maximum.py`
- **Monotonic Deque**: Advanced deque-based algorithm
- Find maximum in sliding windows efficiently
- Includes minimum, range, and sum variants
- Step-by-step visualization

**Key Features:**
- O(n) time complexity vs O(n*k) naive approach
- Monotonic queue pattern
- Real-world performance comparison

## 🔧 Queue Operations

### Basic Operations

| Operation | Description | Time Complexity |
|-----------|-------------|-----------------|
| `enqueue(item)` | Add element to rear | O(1) with deque |
| `dequeue()` | Remove element from front | O(1) with deque |
| `peek()` | View front element | O(1) |
| `is_empty()` | Check if queue is empty | O(1) |
| `size()` | Get number of elements | O(1) |

### Example Usage

```python
from collections import deque

# Create queue
queue = deque()

# Enqueue elements
queue.append(10)      # [10]
queue.append(20)      # [10, 20]
queue.append(30)      # [10, 20, 30]

# Dequeue elements
front = queue.popleft()  # 10, queue: [20, 30]

# Peek
next_item = queue[0]     # 20

# Check empty
is_empty = len(queue) == 0  # False
```

## ⏱️ Time Complexity

### Array-Based Queue (list)

| Operation | Time Complexity | Note |
|-----------|----------------|------|
| Enqueue | O(1) | Using `append()` |
| Dequeue | O(n) | Using `pop(0)` - shifts all elements |
| Peek | O(1) | |
| Space | O(n) | |

### Deque-Based Queue (recommended)

| Operation | Time Complexity | Note |
|-----------|----------------|------|
| Enqueue | O(1) | Using `append()` |
| Dequeue | O(1) | Using `popleft()` |
| Peek | O(1) | |
| Space | O(n) | |

### Circular Queue

| Operation | Time Complexity | Note |
|-----------|----------------|------|
| Enqueue | O(1) | |
| Dequeue | O(1) | |
| Peek | O(1) | |
| Space | O(capacity) | Fixed size |

## 🎯 When to Use Queues

### Perfect For:
- ✅ **BFS (Breadth-First Search)**: Level-order tree/graph traversal
- ✅ **Task Scheduling**: CPU scheduling, print queue
- ✅ **Buffering**: IO buffers, streaming data
- ✅ **Order Processing**: First-come-first-served scenarios
- ✅ **Cache Implementation**: LRU cache (with modifications)
- ✅ **Asynchronous Operations**: Message queues, event handling

### Not Ideal For:
- ❌ Random access to middle elements
- ❌ LIFO (Last In First Out) operations → Use Stack
- ❌ Priority-based processing → Use Priority Queue/Heap

## 📊 Implementation Comparison

| Implementation | Pros | Cons | Best For |
|---------------|------|------|----------|
| **List-based** | Simple, built-in | O(n) dequeue | Learning, small queues |
| **Deque-based** | O(1) all operations | Slightly more memory | Production code |
| **Circular** | Fixed memory, efficient | Fixed size | Embedded systems, buffers |
| **Two Stacks** | Interview practice | More complex | Understanding data structures |

## 🚀 Common Applications

### 1. Breadth-First Search (BFS)
```python
from collections import deque

def bfs(graph, start):
    queue = deque([start])
    visited = {start}
    
    while queue:
        node = queue.popleft()
        print(node)
        
        for neighbor in graph[node]:
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)
```

### 2. Level Order Tree Traversal
```python
def level_order(root):
    if not root:
        return []
    
    queue = deque([root])
    result = []
    
    while queue:
        level = []
        for _ in range(len(queue)):
            node = queue.popleft()
            level.append(node.val)
            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)
        result.append(level)
    
    return result
```

### 3. Sliding Window Maximum
```python
from collections import deque

def max_sliding_window(nums, k):
    dq = deque()
    result = []
    
    for i, num in enumerate(nums):
        # Remove elements outside window
        if dq and dq[0] < i - k + 1:
            dq.popleft()
        
        # Remove smaller elements
        while dq and nums[dq[-1]] < num:
            dq.pop()
        
        dq.append(i)
        
        if i >= k - 1:
            result.append(nums[dq[0]])
    
    return result
```

### 4. CPU Round-Robin Scheduling
```python
def round_robin(processes, time_quantum):
    queue = deque(processes)
    
    while queue:
        process = queue.popleft()
        
        if process.remaining_time > time_quantum:
            process.remaining_time -= time_quantum
            queue.append(process)  # Back to queue
        else:
            # Process complete
            print(f"{process.name} completed")
```

## 📖 How to Use

### Running Examples

Each file contains comprehensive examples and test cases. Run them directly:

```bash
# Basic queue examples
python basic_queue.py

# Circular queue with round-robin simulation
python circular_queue.py

# Queue using stacks with performance comparison
python queue_using_stacks.py

# Sliding window maximum with visualizations
python sliding_window_maximum.py
```

### Importing in Your Code

```python
# Use optimized queue
from collections import deque

queue = deque()
queue.append(item)      # Enqueue
item = queue.popleft()  # Dequeue

# Or import custom implementations
from basic_queue import OptimizedQueue
from circular_queue import CircularQueue
from queue_using_stacks import QueueUsingStacksExpensiveDequeue
```

## 🎓 Learning Path

1. **Start with**: `basic_queue.py` - Understand fundamentals
2. **Move to**: `circular_queue.py` - Learn space efficiency
3. **Challenge**: `queue_using_stacks.py` - Master conversions
4. **Advanced**: `sliding_window_maximum.py` - Apply to complex problems

## 💡 Pro Tips

1. **Always use `collections.deque`** for queue operations in Python (O(1) for both ends)
2. **Use circular queues** when you know the maximum size beforehand
3. **Monotonic deques** are powerful for sliding window problems
4. **Queue using stacks** teaches important concepts but use deque in production

## 🔗 Related Topics

- **Stack**: LIFO data structure (opposite of queue)
- **Deque**: Double-ended queue (operations on both ends)
- **Priority Queue**: Elements dequeued by priority (use `heapq`)
- **Circular Buffer**: Fixed-size queue with wrap-around

## 📝 Interview Tips

Common queue interview questions:
- ✅ Implement queue using stacks
- ✅ Sliding window maximum/minimum
- ✅ First non-repeating character in stream
- ✅ Generate binary numbers from 1 to n
- ✅ Level order tree traversal
- ✅ Rotting oranges (BFS)

## 🤝 Contributing

Feel free to add more queue implementations or problems! Follow the template in `TEMPLATE.md`.

---

**Happy Coding! 🚀**

For questions or improvements, please refer to the main [OpenSauce README](../../README.md).
