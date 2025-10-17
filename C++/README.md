# C++ Programming - DSA Contributions

Welcome to the C++ folder for DSA (Data Structures and Algorithms) contributions!

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
C++ is a general-purpose programming language created by Bjarne Stroustrup as an extension of the C programming language. It supports procedural, object-oriented, generic, and functional programming paradigms. Its performance makes it ideal for competitive programming and system-level applications.

## Getting Started
To contribute to this repository, you'll need a C++ compiler that supports C++11 or later.

### Prerequisites
- C++ compiler (GCC, Clang, or MSVC) supporting C++11 or later
- Basic knowledge of C++ programming
- Understanding of data structures and algorithms

### Installation
- On Windows: Install MinGW-w64 or Visual Studio
- On macOS: Install Xcode command line tools with `xcode-select --install`
- On Linux: Install GCC with `sudo apt-get install build-essential`

To compile and run a C++ file:
```bash
g++ -std=c++11 -o program program.cpp
./program
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
- Use consistent brace style (same line or next line, but be consistent)
- Prefer initialization lists for constructors
- Use const whenever possible (const parameters, const member functions)
- Use auto for complex types or when type is obvious from context
- Use nullptr instead of NULL
- Use RAII (Resource Acquisition Is Initialization) principle
- Prefer pre-increment over post-increment when return value is not needed: ++i vs i++

## Naming Conventions
### Files
- Use snake_case for source files: `binary_search.cpp`, `bubble_sort.cpp`
- Use snake_case for header files: `linked_list.h` or `linked_list.hpp`
- Include problem name in filename if solving a specific problem

### Classes and Types
- Use PascalCase for class names: `LinkedList`, `BinarySearchTree`
- Use PascalCase for type aliases: `using Integer = int;`

### Functions and Variables
- Use snake_case for function names: `create_node`, `insert_element`
- Use snake_case for variable names: `current_node`, `element_count`
- Use descriptive names that reflect purpose
- Use m_ prefix for member variables: `m_head`, `m_size`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /** */ for documentation comments (compatible with Doxygen)
- Use // for single-line comments
- Document all classes, functions, and complex code blocks
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in comments

### Documentation Format
Each code file should include:
- Brief description of the algorithm/data structure
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Author information
- Date of implementation
- Example usage and output
- References to relevant resources

## Example Implementation
```cpp
#include <iostream>
#include <vector>

/**
 * Binary search implementation in C++
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 * 
 * @param arr: Sorted vector to search in
 * @param target: Element to search for
 * @return: Index of target element, or -1 if not found
 */
int binary_search(const std::vector<int>& arr, int target) {
    int left = 0;
    int right = arr.size() - 1;
    
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

int main() {
    std::vector<int> arr = {2, 3, 4, 10, 40};
    int target = 10;
    int result = binary_search(arr, target);
    
    if (result != -1) {
        std::cout << "Element found at index " << result << std::endl;
    } else {
        std::cout << "Element not found" << std::endl;
    }
    
    return 0;
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `_test.cpp` suffix: `binary_search_test.cpp`

### Testing Framework
- Use Google Test framework for comprehensive testing
- Consider using Catch2 for header-only testing

### Test Requirements
- Include edge cases (empty vectors, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Test exception safety where applicable

## Performance Benchmarks
### Benchmarking Tools
- Use Google Benchmark library
- Profile with tools like valgrind and perf

### Performance Metrics
- Execution time
- Memory usage
- Cache efficiency
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
- Use modern C++ features where appropriate (C++11 and later)

## Resources
- [C++ Reference](https://en.cppreference.com/) - Comprehensive C++ reference
- [Effective C++](https://www.aristeia.com/books.html) - Scott Meyers' book on C++ best practices
- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines) - Guidelines by Bjarne Stroustrup and Herb Sutter

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.