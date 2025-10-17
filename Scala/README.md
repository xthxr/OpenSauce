# Scala Programming - DSA Contributions

Welcome to the Scala folder for DSA (Data Structures and Algorithms) contributions!

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
Scala is a general-purpose programming language that combines object-oriented and functional programming paradigms. Scala runs on the Java Virtual Machine (JVM) and is compatible with existing Java code. It's designed to be concise and type-safe, making it excellent for developing complex algorithms with fewer bugs.

## Getting Started
To contribute to this repository, you'll need Scala 2.12 or 2.13 with Java 8 or higher.

### Prerequisites
- Java 8 or higher
- Scala 2.12 or 2.13
- SBT (Scala Build Tool) or Maven
- Basic knowledge of Scala programming
- Understanding of data structures and algorithms

### Installation
- Install Scala using SDKMAN: `sdk install scala`
- Install SBT: `sdk install sbt`
- Or download from scala-lang.org
- Verify installation with `scala -version` and `sbt version`

To run a Scala file:
```bash
scala BinarySearch.scala
# Or using SBT:
sbt run
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
- Use 2 spaces for indentation (no tabs)
- Maximum line length of 100 characters
- Follow Scala Style Guide
- Use camelCase for methods, functions, and variables
- Use PascalCase for classes, traits, and objects
- Use UPPER_SNAKE_CASE for constants
- Prefer val over var (immutability)
- Prefer functional programming approaches (map, filter, fold)
- Use case classes for data structures
- Use pattern matching instead of if/else chains
- Use apply/unapply methods appropriately
- Use implicit parameters and conversions carefully

## Naming Conventions
### Files
- Use PascalCase for Scala source files: `BinarySearch.scala`, `BubbleSort.scala`
- Include problem name in filename if solving a specific problem

### Classes, Traits, and Objects
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Methods, Functions, and Variables
- Use camelCase: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose
- Use camelCase for variable names: `elementCount`, `isSorted`

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /** */ for documentation comments (Scaladoc)
- Use // for single-line comments
- Document all public classes, methods, and functions
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in documentation

### Scaladoc Format
Each public class/method should include:
- Brief description of the algorithm/class
- @param tags for all parameters with description
- @return tag for return values with description
- @throws tag for exceptions
- @author tag with contributor name
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```scala
/**
 * Binary search implementation in Scala
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 * 
 * @param arr Sorted array to search in
 * @param target Element to search for
 * @return Index of target element, or -1 if not found
 */
object BinarySearch {
  def binarySearch(arr: Array[Int], target: Int): Int = {
    var left = 0
    var right = arr.length - 1
    
    while (left <= right) {
      val mid = left + (right - left) / 2
      
      if (arr(mid) == target) {
        return mid
      } else if (arr(mid) < target) {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    -1 // Element not found
  }
  
  def main(args: Array[String]): Unit = {
    val arr = Array(2, 3, 4, 10, 40)
    val target = 10
    val result = binarySearch(arr, target)
    
    if (result != -1) {
      println(s"Element found at index $result")
    } else {
      println("Element not found")
    }
  }
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `Test.scala` suffix: `BinarySearchTest.scala`

### Testing Framework
- Use ScalaTest with various styles
- Consider using Specs2 for specification-based testing

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use property-based testing where appropriate

## Performance Benchmarks
### Benchmarking Tools
- Use ScalaMeter for comprehensive benchmarking
- Use JMH (Java Microbenchmark Harness) from Scala
- Use SBT with benchmark plugins

### Performance Metrics
- Execution time
- Memory allocation
- GC pressure
- Comparison with naive implementations
- Big O verification with different input sizes

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
- Verify documentation is complete (Scaladoc comments)
- Check that your code passes linting (use Scalastyle or Wartremover)
- Use Scala idioms and functional programming best practices

## Resources
- [Scala Style Guide](https://docs.scala-lang.org/style/) - Official Scala style guide
- [Scala Documentation](https://docs.scala-lang.org/) - Comprehensive Scala documentation
- [Scala API Documentation](https://www.scala-lang.org/api/current/) - Official Scala API
- [Effective Scala](https://twitter.github.io/effectivescala/) - Twitter's guide to Scala best practices

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.