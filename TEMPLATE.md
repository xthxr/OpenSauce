# Algorithm/Data Structure Template

This is a template to help you structure your contributions. Copy and adapt this format for your implementations.

## File Structure Template

```
Language/
└── topic/
    └── algorithm_name.ext
```

## Code Template

### Python Example

```python
"""
[Algorithm/Data Structure Name]

Time Complexity: O(?)
Space Complexity: O(?)

Brief description of what the algorithm/data structure does and when to use it.
"""

def algorithm_name(parameters):
    """
    Detailed description of the function.
    
    Args:
        param1: Description of first parameter
        param2: Description of second parameter
    
    Returns:
        Description of what is returned
    
    Example:
        >>> algorithm_name(example_input)
        expected_output
    """
    # Implementation here
    pass

if __name__ == "__main__":
    # Example usage and test cases
    print("Example usage:")
    # Your test cases here
```

### Java Example

```java
/**
 * [Algorithm/Data Structure Name]
 * 
 * Time Complexity: O(?)
 * Space Complexity: O(?)
 * 
 * Brief description of what the algorithm/data structure does and when to use it.
 */
public class AlgorithmName {
    
    /**
     * Detailed description of the method.
     * 
     * @param param1 Description of first parameter
     * @param param2 Description of second parameter
     * @return Description of what is returned
     */
    public static ReturnType algorithmName(Type param1, Type param2) {
        // Implementation here
        return result;
    }
    
    /**
     * Main method with example usage
     */
    public static void main(String[] args) {
        // Example usage and test cases
        System.out.println("Example usage:");
        // Your test cases here
    }
}
```

### C++ Example

```cpp
/*
 * [Algorithm/Data Structure Name]
 * 
 * Time Complexity: O(?)
 * Space Complexity: O(?)
 * 
 * Brief description of what the algorithm/data structure does and when to use it.
 */

#include <iostream>
#include <vector>
using namespace std;

/**
 * Detailed description of the function.
 * 
 * @param param1 Description of first parameter
 * @param param2 Description of second parameter
 * @return Description of what is returned
 */
ReturnType algorithmName(Type param1, Type param2) {
    // Implementation here
    return result;
}

int main() {
    // Example usage and test cases
    cout << "Example usage:" << endl;
    // Your test cases here
    return 0;
}
```

### JavaScript Example

```javascript
/**
 * [Algorithm/Data Structure Name]
 * 
 * Time Complexity: O(?)
 * Space Complexity: O(?)
 * 
 * Brief description of what the algorithm/data structure does and when to use it.
 */

/**
 * Detailed description of the function.
 * 
 * @param {Type} param1 - Description of first parameter
 * @param {Type} param2 - Description of second parameter
 * @returns {Type} Description of what is returned
 * 
 * @example
 * algorithmName(exampleInput)
 * // returns expectedOutput
 */
function algorithmName(param1, param2) {
    // Implementation here
    return result;
}

// Example usage and test cases
console.log("Example usage:");
// Your test cases here
```

## What to Include

### 1. Header Comment
- Name of the algorithm/data structure
- Time complexity analysis
- Space complexity analysis
- Brief description of the approach

### 2. Implementation
- Clean, readable code
- Meaningful variable names
- Comments for complex logic
- Error handling where appropriate

### 3. Documentation
- Function/method documentation
- Parameter descriptions
- Return value description
- Example usage

### 4. Test Cases
- Basic functionality test
- Edge cases (empty input, single element, etc.)
- Example with expected output

## Best Practices

### Do's ✅
- Write clear, self-documenting code
- Include both time and space complexity
- Provide multiple test cases
- Follow language-specific conventions
- Add comments for complex logic
- Test your code before submitting

### Don'ts ❌
- Don't submit code without testing
- Don't copy-paste without understanding
- Don't forget complexity analysis
- Don't use unclear variable names
- Don't submit without documentation

## Example Topics and Algorithms

### Arrays
- Two Sum, Three Sum
- Maximum Subarray (Kadane's Algorithm)
- Rotate Array
- Dutch National Flag Problem

### Linked Lists
- Reverse Linked List
- Detect Cycle
- Merge Two Sorted Lists
- Find Middle Element

### Trees
- Tree Traversals (Inorder, Preorder, Postorder)
- Level Order Traversal
- Height/Depth of Tree
- Lowest Common Ancestor

### Sorting
- Bubble Sort, Selection Sort, Insertion Sort
- Merge Sort, Quick Sort, Heap Sort
- Counting Sort, Radix Sort

### Searching
- Binary Search
- Linear Search
- Jump Search
- Exponential Search

### Dynamic Programming
- Fibonacci Series
- Longest Common Subsequence
- Knapsack Problem
- Coin Change Problem

### Graphs
- DFS and BFS
- Dijkstra's Algorithm
- Bellman-Ford Algorithm
- Topological Sort

## Questions?

If you have any questions about the format or structure, please:
1. Check the CONTRIBUTING.md file
2. Look at existing examples in the repository
3. Open an issue for clarification

---

Happy Coding! 🚀
