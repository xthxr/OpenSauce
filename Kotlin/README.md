# Kotlin Programming - DSA Contributions

Welcome to the Kotlin folder for DSA (Data Structures and Algorithms) contributions!

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
Kotlin is a cross-platform, statically typed, general-purpose programming language with type inference. Kotlin is designed to interoperate fully with Java, and the JVM runtime allows Android development to make use of the existing Java ecosystem. Kotlin is known for its concise syntax, null safety, and functional programming features.

## Getting Started
To contribute to this repository, you'll need Kotlin 1.4 or later with JDK 8 or higher.

### Prerequisites
- JDK 8 or higher
- Kotlin 1.4 or later
- Basic knowledge of Kotlin programming
- Understanding of data structures and algorithms

### Installation
- Install Kotlin using SDKMAN: `sdk install kotlin`
- Or download from kotlinlang.org
- Verify installation with `kotlin -version`

To run a Kotlin file:
```bash
kotlinc binary_search.kt -include-runtime -d binary_search.jar
java -jar binary_search.jar
# Or compile and run in one step:
kotlin binary_search.kt
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
- Maximum line length of 120 characters
- Follow Kotlin coding conventions
- Use camelCase for functions and properties
- Use PascalCase for classes and interfaces
- Use UPPER_SNAKE_CASE for constants
- Use val instead of var where possible (immutability)
- Use Kotlin idioms like when expressions, safe calls, and elvis operator
- Use extension functions when appropriate
- Use data classes for simple data containers
- Use sealed classes for restricted class hierarchies

## Naming Conventions
### Files
- Use PascalCase for Kotlin source files: `BinarySearch.kt`, `BubbleSort.kt`
- Include problem name in filename if solving a specific problem

### Classes and Interfaces
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Functions and Properties
- Use camelCase: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose
- Use camelCase for property names: `elementCount`, `isSorted`

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`
- Or PascalCase for constants in companion objects

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /** */ for documentation comments (KDoc, compatible with JavaDoc)
- Use // for single-line comments
- Document all public classes, functions, and properties
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in documentation

### KDoc Format
Each public class/function should include:
- Brief description of the algorithm/function
- @param tags for all parameters with description
- @return tag for return values with description
- @throws tag for exceptions
- @author tag with contributor name
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```kotlin
/**
 * Binary search implementation in Kotlin
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 *
 * @param arr Sorted array to search in
 * @param target Element to search for
 * @return Index of target element, or -1 if not found
 */
fun binarySearch(arr: IntArray, target: Int): Int {
    var left = 0
    var right = arr.size - 1
    
    while (left <= right) {
        val mid = left + (right - left) / 2
        
        when {
            arr[mid] == target -> return mid
            arr[mid] < target -> left = mid + 1
            else -> right = mid - 1
        }
    }
    
    return -1 // Element not found
}

fun main() {
    val arr = intArrayOf(2, 3, 4, 10, 40)
    val target = 10
    val result = binarySearch(arr, target)
    
    if (result != -1) {
        println("Element found at index $result")
    } else {
        println("Element not found")
    }
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `Test.kt` suffix: `BinarySearchTest.kt`

### Testing Framework
- Use JUnit 5 with Kotlin
- Use KotlinTest or Spek for more Kotlin-specific testing

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use parameterized tests for multiple inputs

## Performance Benchmarks
### Benchmarking Tools
- Use JMH (Java Microbenchmark Harness) through Kotlin
- Use Kotlin's built-in benchmark utilities
- Profile with Java profilers

### Performance Metrics
- Execution time
- Memory allocation
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
- Verify documentation is complete (KDoc comments)
- Check that your code passes linting (use detekt)
- Use Kotlin idioms and best practices where appropriate

## Resources
- [Kotlin Coding Conventions](https://kotlinlang.org/docs/coding-conventions.html) - Official Kotlin style guide
- [Kotlin Style Guide](https://developer.android.com/kotlin/style-guide) - Android-style Kotlin guide
- [Kotlin Documentation](https://kotlinlang.org/docs/home.html) - Official Kotlin documentation
- [Kotlin for Java Developers](https://kotlinlang.org/docs/java-to-kotlin-comparison.html) - Kotlin vs Java comparison

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.