# Haskell Programming - DSA Contributions

Welcome to the Haskell folder for DSA (Data Structures and Algorithms) contributions!

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
Haskell is a general-purpose, statically-typed, purely functional programming language with type inference and lazy evaluation. It's ideal for mathematical computations, algorithmic prototyping, and exploring functional programming paradigms. Haskell's strong type system helps catch errors at compile time and its functional nature leads to concise, mathematically elegant solutions.

## Getting Started
To contribute to this repository, you'll need GHC (Glasgow Haskell Compiler) and Cabal or Stack.

### Prerequisites
- GHC (Glasgow Haskell Compiler) 8.0 or later
- Cabal or Stack build tool
- Basic knowledge of Haskell programming
- Understanding of data structures and algorithms

### Installation
- Install GHC from haskell.org or use Haskell Platform
- Install Stack: `curl -sSL https://get.haskellstack.org/ | sh`
- Verify installation with `ghc --version` and `stack --version`

To run a Haskell file:
```bash
# Using GHCi (interactive)
ghci binary_search.hs

# Using GHC compiler
ghc binary_search.hs
./binary_search

# Using Stack
stack runghc binary_search.hs
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
- Use camelCase for functions and variables (lowercase first letter)
- Use PascalCase for type names and constructors
- Use descriptive names that reflect purpose
- Use type signatures for all top-level functions
- Prefer pattern matching over conditional expressions
- Use guards instead of nested if-then-else
- Use let/in, where, and $ appropriately for readability
- Use point-free style where it improves readability
- Use higher-order functions when appropriate (map, filter, fold)

## Naming Conventions
### Files
- Use PascalCase or camelCase for Haskell source files: `BinarySearch.hs`, `binarySearch.hs`
- Include problem name in filename if solving a specific problem

### Functions and Variables
- Use camelCase starting with lowercase letter: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose

### Types and Constructors
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use -- for single-line comments
- Use {- -} for multi-line comments
- Use Haddock format for API documentation
- Document all exported functions and types with purpose and complexity
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in documentation

### Haddock Format
Each exported function/type should include:
- Brief description of the algorithm/function
- Parameters with descriptions
- Return values with descriptions
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```haskell
-- | Binary search implementation in Haskell
-- Time Complexity: O(log n)
-- Space Complexity: O(log n) due to recursion
module BinarySearch where

-- | Performs binary search on a sorted list
-- Returns the index of the target element, or Nothing if not found
binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch xs target = binarySearchHelper xs target 0 (length xs - 1)
  where
    binarySearchHelper [] _ _ _ = Nothing
    binarySearchHelper arr t left right
      | left > right = Nothing
      | arr !! mid == t = Just mid
      | arr !! mid < t = binarySearchHelper arr t (mid + 1) right
      | otherwise = binarySearchHelper arr t left (mid - 1)
      where
        mid = left + (right - left) `div` 2

-- Example usage
main :: IO ()
main = do
    let arr = [2, 3, 4, 10, 40]
    let target = 10
    case binarySearch arr target of
        Just index -> putStrLn $ "Element found at index " ++ show index
        Nothing -> putStrLn "Element not found"
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `_test.hs` or `_spec.hs` suffix: `binarySearch_test.hs`

### Testing Framework
- Use Hspec for behavior-driven testing
- Consider using QuickCheck for property-based testing

### Test Requirements
- Include edge cases (empty lists, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use property-based testing where appropriate

## Performance Benchmarks
### Benchmarking Tools
- Use criterion library for comprehensive benchmarking
- Use GHC's profiling capabilities with -prof flag
- Use time command for simple timing

### Performance Metrics
- Execution time
- Memory usage
- Allocation patterns
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
- Verify documentation is complete (Haddock comments)
- Check that your code compiles without warnings
- Use functional programming idioms where appropriate

## Resources
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell) - Comprehensive Haskell guide
- [Real World Haskell](http://book.realworldhaskell.org/) - Practical Haskell book
- [Learn You a Haskell](http://learnyouahaskell.com/) - Friendly Haskell tutorial
- [Haskell Documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/) - Official GHC documentation

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.