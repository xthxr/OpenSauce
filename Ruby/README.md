# Ruby Programming - DSA Contributions

Welcome to the Ruby folder for DSA (Data Structures and Algorithms) contributions!

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
Ruby is a dynamic, reflective, object-oriented, general-purpose programming language that combines syntax inspired by Perl, Smalltalk, and Lisp. It emphasizes simplicity and productivity, with an elegant syntax that feels natural to read and easy to write. Ruby is great for rapid prototyping of algorithms and clean, readable code.

## Getting Started
To contribute to this repository, you'll need Ruby 2.5 or later.

### Prerequisites
- Ruby 2.5 or later
- Basic knowledge of Ruby programming
- Understanding of data structures and algorithms

### Installation
- Download and install Ruby from ruby-lang.org
- On macOS: Use Homebrew with `brew install ruby`
- On Linux: Use package manager (e.g., `sudo apt-get install ruby`)
- Verify installation with `ruby --version`

To run a Ruby file:
```bash
ruby binary_search.rb
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
- Use snake_case for methods, variables, and files
- Use PascalCase for class names and modules
- Use UPPER_SNAKE_CASE for constants
- Prefer single quotes for strings unless interpolation is needed
- Use consistent spacing around operators
- Prefer %i for symbol arrays and %w for string arrays
- Use Ruby idioms when possible (e.g., `unless` instead of `if !`)
- Use Ruby enumerable methods when appropriate

## Naming Conventions
### Files
- Use snake_case for Ruby source files: `binary_search.rb`, `bubble_sort.rb`
- Include problem name in filename if solving a specific problem

### Methods and Variables
- Use snake_case: `binary_search`, `current_node`
- Use descriptive names that reflect purpose
- Use snake_case for variable names: `element_count`, `is_sorted`

### Classes and Modules
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use YARD documentation format for detailed documentation
- Use # for single-line comments
- Use =begin/=end for multi-line comments
- Document all public methods with purpose, parameters, and return values
- Include time and space complexity in method documentation
- Add brief descriptions for complex algorithms
- Include example usage in comments

### Documentation Format
Each method/class should include:
- Brief description of the algorithm/method
- @param tags for all parameters with type and description
- @return tag for return values with type and description
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage when helpful

## Example Implementation
```ruby
# Binary search implementation in Ruby
# Time Complexity: O(log n)
# Space Complexity: O(1)
#
# @param arr [Array<Integer>] Sorted array to search in
# @param target [Integer] Element to search for
# @return [Integer] Index of target element, or -1 if not found
def binary_search(arr, target)
  left = 0
  right = arr.length - 1
  
  while left <= right
    mid = left + (right - left) / 2
    
    if arr[mid] == target
      return mid
    elsif arr[mid] < target
      left = mid + 1
    else
      right = mid - 1
    end
  end
  
  -1  # Element not found
end

# Example usage
arr = [2, 3, 4, 10, 40]
target = 10
result = binary_search(arr, target)

if result != -1
  puts "Element found at index #{result}"
else
  puts "Element not found"
end
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `_test.rb` suffix: `binary_search_test.rb`

### Testing Framework
- Use Minitest for simple testing (part of standard library)
- Use RSpec for more comprehensive testing

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Test for proper exception handling where applicable

## Performance Benchmarks
### Benchmarking Tools
- Use Benchmark module (part of standard library)
- Use benchmark-ips for more accurate iterations per second measurements

### Performance Metrics
- Execution time
- Memory usage
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
- Verify documentation is complete
- Check that your code passes linting (use rubocop)
- Use Ruby idioms and best practices where appropriate

## Resources
- [Ruby Style Guide](https://rubystyle.guide/) - Community-driven Ruby coding style
- [Ruby Documentation](https://ruby-doc.org/) - Official Ruby documentation
- [Programming Ruby](https://ruby-doc.com/docs/ProgrammingRuby/) - The Pickaxe Book
- [Ruby Best Practices](https://github.com/rubocop/ruby-style-guide) - Community best practices

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.