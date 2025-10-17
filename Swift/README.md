# Swift Programming - DSA Contributions

Welcome to the Swift folder for DSA (Data Structures and Algorithms) contributions!

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
Swift is a powerful and intuitive programming language for iOS, iPadOS, macOS, tvOS, and watchOS. It's designed to be safe, fast, and expressive, with modern features like type inference, memory safety, and protocol-oriented programming. Swift combines the performance of compiled languages with the expressiveness of scripting languages.

## Getting Started
To contribute to this repository, you'll need Swift 5.0 or later (Xcode 10.2+ on macOS).

### Prerequisites
- Swift 5.0 or later (Xcode Command Line Tools on macOS: `xcode-select --install`)
- On Linux: Swift toolchain 5.0 or later
- Basic knowledge of Swift programming
- Understanding of data structures and algorithms

### Installation
- On macOS: Install Xcode from App Store or command line tools
- On Linux: Download Swift toolchain from swift.org
- Verify installation with `swift --version`

To run a Swift file:
```bash
swift binary_search.swift
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
- Follow Swift API Design Guidelines
- Use camelCase for functions, methods, variables, and properties
- Use PascalCase for types (classes, structs, enums, protocols)
- Use UPPER_SNAKE_CASE for constants
- Prefer value types (structs, enums) over reference types (classes) when possible
- Use optionals instead of nil pointers
- Use closures instead of function pointers
- Use protocol-oriented programming when appropriate
- Use guard statements for early exits and error handling

## Naming Conventions
### Files
- Use PascalCase for Swift source files: `BinarySearch.swift`, `BubbleSort.swift`
- Include problem name in filename if solving a specific problem

### Classes, Structs, Enums, and Protocols
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Functions, Methods, Variables, and Properties
- Use camelCase: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose
- Use camelCase for property names: `elementCount`, `isSorted`

### Constants
- Use camelCase: `maxSize`, `defaultCapacity`
- Or UPPER_SNAKE_CASE for global constants

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /// for documentation comments (supports Markdown)
- Use // for single-line comments
- Use /* */ for multi-line comments
- Document all public types, functions, and methods
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in documentation

### Documentation Format
Each public type/function should include:
- Brief description of the algorithm/type
- Parameters with descriptions
- Return values with descriptions
- Throws conditions if applicable
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```swift
/// Binary search implementation in Swift
/// Time Complexity: O(log n)
/// Space Complexity: O(1)
///
/// - Parameters:
///   - arr: Sorted array to search in
///   - target: Element to search for
/// - Returns: Index of target element, or nil if not found
func binarySearch<T: Comparable>(arr: [T], target: T) -> Int? {
    var left = 0
    var right = arr.count - 1
    
    while left <= right {
        let mid = left + (right - left) / 2
        
        if arr[mid] == target {
            return mid
        } else if arr[mid] < target {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    
    return nil // Element not found
}

// Example usage
let arr = [2, 3, 4, 10, 40]
let target = 10
if let result = binarySearch(arr: arr, target: target) {
    print("Element found at index \(result)")
} else {
    print("Element not found")
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `Tests.swift` suffix: `BinarySearchTests.swift`

### Testing Framework
- Use XCTest framework (built into Swift)
- Follow Swift testing best practices

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use parameterized tests where appropriate

## Performance Benchmarks
### Benchmarking Tools
- Use XCTest's performance testing capabilities
- Use Instruments for detailed profiling
- Use Dispatch for time measurements

### Performance Metrics
- Execution time
- Memory allocation
- CPU utilization
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
- Verify documentation is complete (SwiftDoc comments)
- Check that your code passes linting (use SwiftLint)
- Use Swift idioms and best practices where appropriate

## Resources
- [Swift API Design Guidelines](https://swift.org/documentation/api-design-guidelines/) - Official Swift API design guidelines
- [The Swift Programming Language](https://docs.swift.org/swift-book/) - Official Swift documentation
- [Swift Style Guide](https://github.com/github/swift-style-guide) - GitHub's Swift style guide
- [Swift.org](https://swift.org/) - Official Swift language resources

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.