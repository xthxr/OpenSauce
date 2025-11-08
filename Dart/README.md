# Dart Programming - DSA Contributions

Welcome to the Dart folder for DSA (Data Structures and Algorithms) contributions!

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
Dart is a client-optimized language for developing fast apps on any platform. It's the language behind Flutter, Google's UI toolkit for building natively compiled applications for mobile, web, and desktop from a single codebase. Dart is object-oriented, class-based, with C-style syntax that transcompiles optionally to JavaScript.

## Getting Started
To contribute to this repository, you'll need Dart SDK 2.12 or later.

### Prerequisites
- Dart SDK 2.12 or later
- Basic knowledge of Dart programming
- Understanding of data structures and algorithms

### Installation
- Download and install Dart SDK from dart.dev
- Verify installation with `dart --version`

To run a Dart file:
```bash
dart binary_search.dart
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
- Maximum line length of 80 characters
- Follow Dart style guide and use dart format
- Use camelCase for methods, variables, parameters
- Use PascalCase for classes, typedefs, and type parameters
- Use underscore_separated_names for libraries, packages, directories, and private members
- Use UPPER_CASE for constants
- Use late keyword for lazy initialization when appropriate
- Use null safety features (null-aware operators, nullable types)
- Use cascades (..) for method chaining
- Follow idiomatic Dart patterns and naming conventions

## Naming Conventions
### Files
- Use snake_case for Dart source files: `binary_search.dart`, `bubble_sort.dart`
- Include problem name in filename if solving a specific problem

### Classes and Enums
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Methods and Variables
- Use camelCase: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose
- Use camelCase for variable names: `elementCount`, `isSorted`

### Constants
- Use UPPER_CASE: `maxSize`, `defaultCapacity` (for variables) or `MAX_SIZE`, `DEFAULT_CAPACITY` (for static constants)

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /// for documentation comments (supports Markdown)
- Use // for single-line comments
- Use /* */ for multi-line comments
- Document all public classes, methods, and functions
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in documentation

### Documentation Format
Each public class/method should include:
- Brief description of the algorithm/method
- Parameters with descriptions
- Return values with descriptions
- Exception conditions if applicable
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```dart
/// Binary search implementation in Dart
/// Time Complexity: O(log n)
/// Space Complexity: O(1)
int binarySearch(List<int> arr, int target) {
  int left = 0;
  int right = arr.length - 1;
  
  while (left <= right) {
    int mid = left + (right - left) ~/ 2;
    
    if (arr[mid] == target) {
      return mid;
    } else if (arr[mid] < target) {
      left = mid + 1;
    } else {
      right = mid - 1;
    }
  }
  
  return -1; // Element not found
}

void main() {
  List<int> arr = [2, 3, 4, 10, 40];
  int target = 10;
  int result = binarySearch(arr, target);
  
  if (result != -1) {
    print('Element found at index $result');
  } else {
    print('Element not found');
  }
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `_test.dart` suffix: `binary_search_test.dart`

### Testing Framework
- Use Dart's built-in test package
- Follow Flutter/Dart testing conventions

### Test Requirements
- Include edge cases (empty lists, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use parameterized tests when testing multiple inputs

## Performance Benchmarks
### Benchmarking Tools
- Use benchmark_harness package for detailed benchmarking
- Use Stopwatch class for simple timing
- Use Observatory for detailed profiling

### Performance Metrics
- Execution time
- Memory allocation
- Garbage collection impact
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
- Test your code thoroughly: `dart test`
- Format your code: `dart format`
- Check for issues: `dart analyze`
- Verify documentation is complete
- Ensure code follows the naming and style conventions
- Use null safety features appropriately

## Resources
- [Dart Style Guide](https://dart.dev/guides/language/effective-dart/style) - Official Dart style guide
- [Effective Dart](https://dart.dev/guides/language/effective-dart) - Best practices for Dart
- [Dart Documentation](https://dart.dev/guides) - Comprehensive Dart documentation
- [Dart API Reference](https://api.dart.dev/) - Official Dart API documentation

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.