# Java Programming - DSA Contributions

Welcome to the Java folder for DSA (Data Structures and Algorithms) contributions!

## Table of Contents
- [Overview](#overview)
- [Getting Started](#getting-started)
- [Project Structure](#project-structure)
- [Language-Specific Coding Conventions](#language-specific-coding-conventions)
- [Naming Conventions](#naming-conventions)
- [Documentation Requirements](#documentation-requirements)
- [Example Implementation](#example-implementation)
- [Testing Guidelines](#testing-guidelines)
- [Performance Benchmarks](#performance-benchmarks)
- [Contribution Guidelines](#contribution-guidelines)
- [Resources](#resources)

## Overview
Java is a high-level, class-based, object-oriented programming language that is designed to have as few implementation dependencies as possible. It's platform-independent, robust, secure, and widely used in enterprise applications, Android development, and competitive programming.

## Getting Started
To contribute to this repository, you'll need Java Development Kit (JDK) 8 or later.

### Prerequisites
- JDK 8 or later (OpenJDK or Oracle JDK)
- Basic knowledge of Java programming
- Understanding of data structures and algorithms

### Installation
- Download and install JDK from Oracle's website or OpenJDK
- Set JAVA_HOME environment variable
- Verify installation with `java -version`

To compile and run a Java file:
```bash
javac BinarySearch.java
java BinarySearch
```

## Project Structure
Organize your code by topic:
- `arrays/` - Array-related problems
- `subarrays/` - Subarray problems (Kadane's, Sliding Window, Prefix Sum)
- `linked_lists/` - Linked list implementations and problems
- `stacks/` - Stack implementations and problems
- `queues/` - Queue implementations and problems
- `trees/` - Tree data structures and algorithms
- `graphs/` - Graph algorithms
- `sorting/` - Sorting algorithms
- `searching/` - Searching algorithms
- `dynamic_programming/` - DP problems
- `greedy/` - Greedy algorithms
- `backtracking/` - Backtracking problems
- `math/` - Mathematical algorithms
- `strings/` - String manipulation problems
- `heaps/` - Heap implementations and problems
- `tries/` - Trie implementations and problems

## Language-Specific Coding Conventions
- Use 4 spaces for indentation (no tabs)
- Maximum line length of 100 characters
- Use camelCase for variables and methods
- Use PascalCase for class names
- Use UPPER_SNAKE_CASE for constants
- Use proper access modifiers (private, protected, public)
- Use meaningful variable and method names
- Follow the single responsibility principle
- Use generics where appropriate
- Prefer enhanced for loops over traditional for loops when possible

## Naming Conventions
### Files
- Use PascalCase for Java source files that contain a public class: `BinarySearch.java`
- Each public class should be in its own file with the same name

### Classes
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Methods and Variables
- Use camelCase for method names: `insertElement`, `binarySearch`
- Use camelCase for variable names: `currentNode`, `elementCount`
- Use descriptive names that reflect purpose
- Use UPPER_SNAKE_CASE for constants: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with dots for package names: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /** */ for documentation comments (Javadoc)
- Use // for single-line comments
- Use /* */ for multi-line comments
- Document all public classes, interfaces, methods, and fields
- Include time and space complexity in method documentation
- Add brief descriptions for complex algorithms
- Include example usage in comments

### Documentation Format (Javadoc)
Each public class and method should include:
- Brief description of the algorithm/class
- @param tags for all parameters with description
- @return tag for return values with description
- @throws tag for exceptions
- @author tag with contributor name
- @since tag indicating version
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.

## Example Implementation
```java
/**
 * Binary search implementation in Java
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 * 
 * @param arr Sorted array to search in
 * @param target Element to search for
 * @return Index of target element, or -1 if not found
 */
public class BinarySearch {
    
    public static int binarySearch(int[] arr, int target) {
        int left = 0;
        int right = arr.length - 1;
        
        while (left <= right) {
            int mid = left + (right - left) / 2;
            
            if (arr[mid] == target) {
                return mid;
            }
            
            if (arr[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        
        return -1;  // Element not found
    }
    
    public static void main(String[] args) {
        int[] arr = {2, 3, 4, 10, 40};
        int target = 10;
        int result = binarySearch(arr, target);
        
        if (result != -1) {
            System.out.println("Element found at index " + result);
        } else {
            System.out.println("Element not found");
        }
    }
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use Test suffix: `BinarySearchTest.java`

### Testing Framework
- Use JUnit 5 for comprehensive testing
- Follow test-driven development principles

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Test exception handling where applicable

## Performance Benchmarks
### Benchmarking Tools
- Use JMH (Java Microbenchmark Harness)
- Profile with VisualVM or JProfiler

### Performance Metrics
- Execution time
- Memory allocation
- Garbage collection impact
- Comparison with naive implementations
- Big O verification

## Contribution Guidelines
1. Fork the repository
2. Create a new branch for your feature: `git checkout -b feature/my-feature`
3. Follow the coding and documentation standards
4. Add comprehensive tests
5. Create the implementation in the appropriate directory
6. Submit a pull request

### Before Submitting
- Test your code thoroughly
- Ensure code follows the naming and style conventions
- Verify documentation is complete
- Check that your code compiles without warnings
- Use Java best practices and design patterns where appropriate

## Resources
- [Oracle Java Documentation](https://docs.oracle.com/javase/) - Official Java documentation
- [Effective Java](https://www.oreilly.com/library/view/effective-java-3rd/9780134686097/) - Joshua Bloch's book on Java best practices
- [Google Java Style Guide](https://google.github.io/styleguide/javaguide.html) - Comprehensive style guide

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.