# DSA Hacktober

## LeetCode Problem 155: Min Stack

Design a stack that supports push, pop, top, and retrieving the minimum element in constant time.

Implement the `MinStack` class:

- `MinStack()` initializes the stack object.
- `void push(int val)` pushes the element `val` onto the stack.
- `void pop()` removes the element on the top of the stack.
- `int top()` gets the top element of the stack.
- `int getMin()` retrieves the minimum element in the stack.

### Intuition & Approach

We want standard stack operations (push, pop, top) and want to retrieve the
minimum element. Using a vector helps achieve this purpose.

We use push_back to imitate push, pop_back for pop, back to get the top i.e.
front element and to find the minimum element we use a for loop and iterate
through the vector.

### Time & Space Complexity 

Push/Pop/Top: O(1)

GetMin: O(n)

Space: O(n) for the stack

## LeetCode Problem 225: Implement Stack Using Queues

Implement a last-in-first-out (LIFO) stack using only two queues. The
 implemented stack should support all the functions of a normal stack (`push`, `top`, `pop`, and `empty`).

Implement the `MyStack` class:

- `void push(int x)` Pushes element x to the top of the stack.
- `int pop()` Removes the element on the top of the stack and returns it.
- `int top()` Returns the element on the top of the stack.
- `boolean empty()` Returns `true` if the stack is empty, `false` otherwise.

**Notes:**

- You must use **only** standard operations of a queue, which means that only `push to back`, `peek/pop from front`, `size` and `is empty` operations are valid.

### Intuition & Approach

A stack is Last In First Out (LIFO), while a queue is First In First Out (FIFO). We want to use queues to create a stack with these functions: push (add to front), pop (remove from front), top (element at front), and empty (check if empty). We can simulate stack behavior by keeping the most recently pushed element at the front of the queue—not at the back, where it would normally be in a queue.

For push, we create a temporary queue and add the new element to it first. Then we transfer all existing elements from the original queue behind it, automatically placing the new element at the front. The remaining operations work the same way in both data structures.

### Time & Space Complexity 

Push: O(n)

Pop/Top/Empty: O(1)

Space: O(n) (for the queue)

## LeetCode Problem 232: Implement Queue Using Stacks

Implement a first in first out (FIFO) queue using only two stacks. 
The implemented queue should support all the functions of a normal queue
 (`push`, `peek`, `pop`, and `empty`).

Implement the `MyQueue` class:

- `void push(int x)` Pushes element x to the back of the queue.
- `int pop()` Removes the element from the front of the queue and returns it.
- `int peek()` Returns the element at the front of the queue.
- `boolean empty()` Returns `true` if the queue is empty, `false` otherwise.

**Notes:**

- You must use **only** standard operations of a stack, which means only `push to top`, `peek/pop from top`, `size`, and `is empty` operations are valid.

### Intuition & Approach

Stack is Last In First Out and Queue is First In First Out.
We want to use stacks to simulate a queue.
The functions to be shown are push (add to back), pop (remove from front),
peek(element at front) and empty(tell if empty).
We need to find a way to add elements to the back instead of front (the way it is
in a stack).

We use two stacks: one (`que`) to handle incoming elements (push operations) and another (`que1`) to handle outgoing elements (pop and peek operations). When popping or peeking, if the output stack is empty, we transfer all elements from the input stack to it. This reverses the order, placing the oldest element on top of `que1`—simulating the front of the queue. The remaining operations work the same way in both stacks.

### Time & Space Complexity 

Push: O(1)

Pop/Peek: O(1) amortized

Space: O(n) total for the stacks

## LeetCode Problem 239: Sliding Window Maximum

You are given an array of integers `nums`, there is a sliding window of size `k` which is moving from the very left of the array to the very right. You can only see the `k` numbers in the window. Each time the sliding window moves right by one position.

Return *the max sliding window*.

## Intuition & Approach

We want to find the maximum value in every sliding window of size k as it moves across the vector. We use a deque to implement this. The front of the deque always stores the maximum element for the current window. As the window slides, we remove elements that fall out of range or are smaller than the current element. This prevents overlapping or unnecessary comparisons.

At each step, we remove front elements that fall outside the sliding window. Then we compare recently added elements with nums[i]. If nums[i] is greater, we remove elements from the back until this condition no longer holds. This makes the algorithm more efficient. Once the sliding window reaches size k, we return the front value—the maximum for that window.

### Time & Space Complexity 

Time: O(n)

Space: O(k) (excluding output array)

## LeetCode Problem 402: Remove K digits

Given string num representing a non-negative integer `num`, and an integer `k`, return *the smallest possible integer after removing* `k` *digits from* `num`.

### Intuition & Approach

We want to remove k digits from the number to create the smallest possible result. To do this, we remove digits that are larger than the next digit—removing a bigger digit before a smaller one reduces the overall value. We use a stack to build the new number.

We iterate through the input digits. If the stack's top element is larger than the current digit, we pop it and push the current one instead. After this process, if k isn't 0, we remove the remaining larger digits from the front. We also remove all leading zeros, reverse the number (since this approach stores digits in reverse order in the stack), and return the result.

### Time & Space Complexity 

Time: O(n)

Space: O(n)

## LeetCode Problem 503: Next Greater Element 2

Given a circular integer array `nums` (i.e., the next element of `nums[nums.length - 1]` is `nums[0]`), return *the **next greater number** for every element in* `nums`.

The **next greater number** of a number `x` 
is the first greater number to its traversing-order next in the array, 
which means you could search circularly to find its next greater number.
 If it doesn't exist, return `-1` for this number.

### Intuition & Approach

We have a circular array, so after searching linearly, if we can't find the next greater element, we loop back to the start and continue searching. We use a stack to track the elements as we traverse.

We iterate from 2n−1 down to 0, effectively processing the array twice. For each element, we pop from the stack while the top is smaller or equal—these can't be the next greater element. If the stack isn't empty after popping, its top is the next greater element for the current position; otherwise, it's −1. We then push the current element onto the stack as a candidate for future elements.

### Time & Space Complexity 

Time: O(n)

Space: O(n)

## LeetCode Problem 735: Asteroid Collision

We are given an array `asteroids` of integers representing
 asteroids in a row. The indices of the asteriod in the array represent 
their relative position in space.

For each asteroid, the absolute value represents its size, and the 
sign represents its direction (positive meaning right, negative meaning 
left). Each asteroid moves at the same speed.

Find out the state of the asteroids after all collisions. If two 
asteroids meet, the smaller one will explode. If both are the same size,
 both will explode. Two asteroids moving in the same direction will 
never meet.

### Intuition & Approach

Asteroids collide only when one is moving right and the other is moving left. If two asteroids meet, the one with the larger absolute value survives, and if they are equal in size, both are destroyed. Asteroids moving in the same direction never collide, so they can safely coexist. A stack is used to efficiently track these interactions because the most recent right-moving asteroid is always the only one that a new left-moving asteroid can collide with.

When a new asteroid arrives, we compare it with the top of the stack. If the stack is empty or both are moving in the same direction, we simply push it. A collision is only possible when the top of the stack is moving right and the new asteroid is moving left. In that case, we repeatedly pop from the stack while the top is smaller in magnitude. If the top is larger, the new asteroid gets destroyed. If both are equal, they destroy each other. Only if the new asteroid survives all collisions do we push it onto the stack.

### Time & Space Complexity 

Time: O(n)

Space: O(n)

## LeetCode Problem 739: Daily Temperatures

Given an array of integers `temperatures` represents the daily temperatures, return *an array* `answer` *such that* `answer[i]` *is the number of days you have to wait after the* `ith` *day to get a warmer temperature*. If there is no future day for which this is possible, keep `answer[i] == 0` instead.

### Intuition & Approach

This problem works much like the *Next Greater Element* pattern. We use a stack to store the **indices** of days whose warmer day hasn't been found yet. As we iterate through the temperature array, we compare the current temperature with the temperature at the index stored at the top of the stack. If the current temperature is warmer, it means we’ve found the next warmer day for that stored index, so we pop it and compute the difference in days.

We continue popping as long as the stack isn’t empty and the current day’s temperature is greater than the temperature at the top index of the stack. This ensures that only the closest warmer day is recorded for each earlier day. Once all valid pops are done, we push the current index onto the stack. Any remaining indices in the stack by the end simply have no warmer day ahead, so their result stays zero.

### Time & Space Complexity 

Time: O(n)

Space: O(n)
