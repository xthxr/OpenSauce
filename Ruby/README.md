# Ruby Programming - DSA Contributions

Welcome to the Ruby folder for DSA (Data Structures and Algorithms) contributions!

This directory contains Ruby implementations of all algorithms available in the Haskell directory, providing idiomatic Ruby solutions with comprehensive examples and test cases.

## Structure

Organize your code by topic:
- `arrays/` - Array-related problems and algorithms
- `dynamic_programming/` - Dynamic programming solutions
- `searching/` - Search algorithms
- `sorting/` - Sorting algorithms
- `trees/` - Tree data structures and algorithms
- `queues/` - Queue implementations and problems
- `graphs/` - Graph algorithms
- `greedy/` - Greedy algorithms
- `backtracking/` - Backtracking problems
- `math/` - Mathematical algorithms
- `strings/` - String manipulation problems

## Implemented Algorithms

### Arrays
- **Two Sum** (`arrays/two_sum.rb`)
  - Brute force approach: O(n²) time, O(1) space
  - Hash map approach: O(n) time, O(n) space
  - Two pointers approach for sorted arrays: O(n) time, O(1) space
  - Find all pairs variant

### Dynamic Programming
- **Fibonacci Sequence** (`dynamic_programming/fibonacci.rb`)
  - Naive recursive: O(2^n) time, O(n) space
  - Memoized recursive: O(n) time, O(n) space
  - Iterative: O(n) time, O(1) space
  - Dynamic programming array: O(n) time, O(n) space
  - Matrix exponentiation: O(log n) time, O(log n) space
  - Golden ratio (Binet's formula): O(1) time, O(1) space

### Searching
- **Binary Search** (`searching/binary_search.rb`)
  - Recursive implementation: O(log n) time, O(log n) space
  - Iterative implementation: O(log n) time, O(1) space
  - Find first occurrence variant
  - Find last occurrence variant
  - Find insertion point variant

### Sorting
- **Merge Sort** (`sorting/merge_sort.rb`)
  - Standard recursive approach: O(n log n) time, O(n) space
  - In-place variant: O(n log n) time, O(n) space
  - Bottom-up iterative approach: O(n log n) time, O(n) space

- **Quick Sort** (`sorting/quick_sort.rb`)
  - Standard recursive approach: O(n log n) average, O(n²) worst case
  - In-place variant with Lomuto partitioning
  - Hoare partitioning scheme
  - Hybrid approach with insertion sort for small arrays
  - Multiple pivot selection strategies

### Trees
- **Binary Search Tree** (`trees/binary_tree.rb`)
  - BST insertion: O(log n) average, O(n) worst case
  - BST search: O(log n) average, O(n) worst case
  - Tree traversals: In-order, Pre-order, Post-order, Level-order
  - Tree properties: Height, node count, min/max values
  - BST validation
  - Node deletion with all cases handled

## Running the Code

Each file can be run independently to see examples and test cases:

```bash
# Run individual files (requires Ruby to be installed)
ruby arrays/two_sum.rb
ruby dynamic_programming/fibonacci.rb
ruby searching/binary_search.rb
ruby sorting/merge_sort.rb
ruby sorting/quick_sort.rb
ruby trees/binary_tree.rb
```

## Features

- **Comprehensive implementations**: Multiple approaches for each algorithm
- **Detailed comments**: Time and space complexity analysis
- **Example usage**: Each file includes working examples
- **Test cases**: Edge cases and various scenarios covered
- **Ruby idioms**: Uses Ruby's expressive syntax and built-in methods
- **Object-oriented design**: Clean class structures where appropriate

## Naming Convention

- Use snake_case for filenames: `binary_search.rb`, `bubble_sort.rb`
- Include problem name in filename if solving a specific problem

## Code Guidelines

- Add comments explaining the algorithm
- Include time and space complexity in comments
- Provide example usage at the bottom of the file
- Follow Ruby style guide (Rubocop standards)
- Use Ruby idioms and built-in methods appropriately

## Algorithm Comparison with Haskell

These Ruby implementations are functionally equivalent to their Haskell counterparts but leverage Ruby's:
- Object-oriented features for better code organization
- Built-in methods for cleaner, more readable code
- Dynamic typing for simpler generic implementations
- Imperative style where it improves clarity (especially for in-place algorithms)

## Contributing

When adding new algorithms:
1. Create the file in the appropriate subdirectory
2. Include multiple implementation approaches where applicable
3. Add comprehensive comments and complexity analysis
4. Provide working examples and test cases
5. Update this README with the new algorithm description
