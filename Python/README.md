# Python Programming - DSA Contributions

Welcome to the Python folder for DSA (Data Structures and Algorithms) contributions!

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
Python is an interpreted, high-level, general-purpose programming language known for its simple, readable syntax. It's excellent for rapid prototyping of algorithms and is widely used in data science, scripting, automation, and competitive programming.

## Getting Started
To contribute to this repository, you'll need Python 3.6 or later.

### Prerequisites
- Python 3.6 or later
- Basic knowledge of Python programming
- Understanding of data structures and algorithms

### Installation
- Download and install Python from python.org
- Verify installation with `python --version` or `python3 --version`

To run a Python file:
```bash
python binary_search.py
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
- Follow PEP 8 style guide
- Use 4 spaces for indentation (no tabs)
- Maximum line length of 79 characters (72 for docstrings)
- Use snake_case for functions, variables, and file names
- Use PascalCase for class names
- Use UPPER_SNAKE_CASE for constants
- Use blank lines appropriately (2 before top-level functions/classes, 1 before methods)
- Avoid wildcard imports (*)
- Use f-strings for string formatting
- Use type hints for function parameters and return values

## Naming Conventions
### Files
- Use snake_case for Python source files: `binary_search.py`, `bubble_sort.py`
- Include problem name in filename if solving a specific problem

### Functions and Variables
- Use snake_case: `binary_search`, `current_node`
- Use descriptive names that reflect purpose
- Use lowercase with underscores for variables: `element_count`, `is_sorted`

### Classes
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Docstrings
- Use triple quotes for docstrings
- Follow PEP 257 conventions
- Include function descriptions, parameters, return values, and exceptions
- Include examples when useful
- Document time and space complexity in docstrings

### Documentation Format
Each function/class should include:
- Brief description of the algorithm/class
- Args: parameter names, types, and descriptions
- Returns: return value type and description
- Raises: exceptions that might be raised
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage when helpful

## Example Implementation
```python
"""
Binary search implementation in Python

Time Complexity: O(log n)
Space Complexity: O(1)

Args:
    arr: Sorted list to search in
    target: Element to search for

Returns:
    Index of target element, or -1 if not found
"""
from typing import List, Union

def binary_search(arr: List[int], target: int) -> int:
    """
    Performs binary search on a sorted array.
    
    Args:
        arr: A sorted list of integers
        target: The integer value to search for
        
    Returns:
        The index of the target if found, otherwise -1
    """
    left, right = 0, len(arr) - 1
    
    while left <= right:
        mid = left + (right - left) // 2
        
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1  # Element not found

if __name__ == "__main__":
    arr = [2, 3, 4, 10, 40]
    target = 10
    result = binary_search(arr, target)
    
    if result != -1:
        print(f"Element found at index {result}")
    else:
        print("Element not found")
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `test_` prefix: `test_binary_search.py`

### Testing Framework
- Use PyTest for comprehensive testing
- Consider using unittest for standard library approach

### Test Requirements
- Include edge cases (empty lists, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use parametrized tests for multiple inputs

## Performance Benchmarks
### Benchmarking Tools
- Use timeit module for simple benchmarks
- Use cProfile for detailed profiling
- Consider using pytest-benchmark for test-based benchmarks

### Performance Metrics
- Execution time
- Memory usage (using memory_profiler)
- Comparison with naive implementations
- Big O verification
- Time complexity verification with different input sizes

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
- Check that your code passes linting (use flake8 or black)
- Use type hints where appropriate
- Consider using mypy for type checking

## Resources
- [PEP 8](https://pep8.org/) - Official Python style guide
- [Effective Python](https://effectivepython.com/) - Brett Slatkin's book on Python best practices
- [Python Enhancement Proposals](https://www.python.org/dev/peps/) - Python standards and proposals
- [Real Python](https://realpython.com/) - High-quality Python tutorials

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.