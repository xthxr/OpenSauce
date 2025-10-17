# C Programming - DSA Contributions

Welcome to the C folder for DSA (Data Structures and Algorithms) contributions!

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
C is a general-purpose, procedural computer programming language supporting structured programming, lexical variable scope, and recursion, with a static type system. It is ideal for systems programming and embedded systems due to its efficiency and low-level memory control.

## Getting Started
To contribute to this repository, you'll need a C compiler like GCC or Clang.

### Prerequisites
- GCC compiler (or equivalent)
- Basic knowledge of C programming
- Understanding of data structures and algorithms

### Installation
- On Windows: Install MinGW or use an IDE like Code::Blocks
- On macOS: Install Xcode command line tools with `xcode-select --install`
- On Linux: Install GCC with `sudo apt-get install build-essential`

To compile and run a C file:
```bash
gcc -o program program.c
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
- Maximum line length of 80 characters
- Use K&R or Allman style for braces
- Use meaningful variable and function names
- Declare variables at the beginning of functions (traditional C style)
- Use const for read-only parameters and variables
- Use void for functions that don't return a value
- Avoid global variables unless absolutely necessary

## Naming Conventions
### Files
- Use snake_case for source files: `binary_search.c`, `bubble_sort.c`
- Use snake_case for header files: `linked_list.h`
- Include problem name in filename if solving a specific problem

### Variables and Functions
- Use snake_case: `current_node`, `insert_element`
- Use descriptive names that reflect purpose
- Prefix function names with their module: `ll_create()` for linked list functions
- Use uppercase for constants: `MAX_SIZE`, `PI`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /* */ for multi-line comments
- Use // for single-line comments
- Document all functions with purpose, parameters, and return values
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
```c
#include <stdio.h>
#include <stdlib.h>

/**
 * Binary search implementation in C
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 * 
 * @param arr: Sorted array to search in
 * @param size: Size of the array
 * @param target: Element to search for
 * @return: Index of target element, or -1 if not found
 */
int binary_search(int arr[], int size, int target) {
    int left = 0;
    int right = size - 1;
    
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
    int arr[] = {2, 3, 4, 10, 40};
    int n = sizeof(arr) / sizeof(arr[0]);
    int target = 10;
    int result = binary_search(arr, n, target);
    
    if (result != -1) {
        printf("Element found at index %d\n", result);
    } else {
        printf("Element not found\n");
    }
    
    return 0;
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `_test.c` suffix: `binary_search_test.c`

### Testing Framework
- Use a simple assertion-based testing approach
- Consider using CMocka or CuTest for more complex testing

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms

## Performance Benchmarks
### Benchmarking Tools
- Use GNU time to measure execution time
- Consider using Valgrind for memory profiling

### Performance Metrics
- Execution time
- Memory usage
- Cache efficiency
- How to record and report performance in comments

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

## Resources
- [The C Programming Language (K&R)](https://en.wikipedia.org/wiki/The_C_Programming_Language) - Classic book by Brian Kernighan and Dennis Ritchie
- [GNU C Manual](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html) - Comprehensive guide to C
- [C FAQ](http://c-faq.com/) - Commonly asked questions about C

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.