# Go Programming - DSA Contributions

Welcome to the Go folder for DSA (Data Structures and Algorithms) contributions!

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
Go (or Golang) is a statically typed, compiled programming language designed at Google. It's known for its simplicity, efficiency, and built-in support for concurrent programming. Go's garbage collector and efficient runtime make it suitable for high-performance applications while maintaining readability and ease of use.

## Getting Started
To contribute to this repository, you'll need Go 1.15 or later.

### Prerequisites
- Go 1.15 or later
- Basic knowledge of Go programming
- Understanding of data structures and algorithms

### Installation
- Download and install Go from golang.org
- Set up GOPATH environment variable
- Verify installation with `go version`

To run a Go file:
```bash
go run binary_search.go
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
- Maximum line length of 120 characters (preferred)
- Use camelCase for functions and variables (or MixedCaps)
- Use PascalCase for exported functions, types, and constants
- Use underscore_separation for package names and file names
- Use explicit variable declarations when type is not obvious
- Use short variable declarations (:=) for local variables when possible
- Follow the idiomatic Go style: simple, clear, and efficient
- Use proper error handling with multiple return values
- Avoid unnecessary package prefixes in names

## Naming Conventions
### Files
- Use snake_case or lowercase for Go source files: `binary_search.go`, `bubble_sort.go`
- Use package name as directory name
- Include problem name in filename if solving a specific problem

### Functions and Variables
- Use camelCase (or MixedCaps) for exported functions/types: `BinarySearch`, `LinkedList`
- Use camelCase for unexported functions/variables: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose

### Types and Structs
- Use PascalCase for struct names: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Constants
- Use UPPER_SNAKE_CASE for exported constants: `MaxSize`, `DefaultCapacity`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use // for single-line comments
- Use /* */ for multi-line comments
- Use GoDoc format for exported functions/types with descriptive comments
- Document all exported functions, types, and packages
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in documentation

### Documentation Format
Each exported function/type should include:
- Brief description of the algorithm/type
- Parameters with descriptions
- Return values with descriptions
- Error conditions if applicable
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```go
// Package search provides implementations of search algorithms
package main

import "fmt"

// BinarySearch performs binary search on a sorted array
// Time Complexity: O(log n)
// Space Complexity: O(1)
// 
// arr: Sorted slice of integers to search in
// target: Element to search for
// Returns: Index of target element, or -1 if not found
func BinarySearch(arr []int, target int) int {
	left, right := 0, len(arr)-1
	
	for left <= right {
		mid := left + (right-left)/2
		
		if arr[mid] == target {
			return mid
		} else if arr[mid] < target {
			left = mid + 1
		} else {
			right = mid - 1
		}
	}
	
	return -1 // Element not found
}

func main() {
	arr := []int{2, 3, 4, 10, 40}
	target := 10
	result := BinarySearch(arr, target)
	
	if result != -1 {
		fmt.Printf("Element found at index %d\n", result)
	} else {
		fmt.Println("Element not found")
	}
}
```

## Testing Guidelines
### Test Structure
- Create test files with `_test.go` suffix: `binary_search_test.go`
- Use the same package name as the file being tested

### Testing Framework
- Use built-in testing package with `go test`
- Use table-driven tests for multiple test cases

### Test Requirements
- Include edge cases (empty slices, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use subtests for organized testing

## Performance Benchmarks
### Benchmarking Tools
- Use built-in testing package with Benchmark functions
- Use `go test -bench=.` to run benchmarks

### Performance Metrics
- Execution time
- Memory allocation (using b.ReportAllocs())
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
- Test your code thoroughly: `go test`
- Format your code: `go fmt`
- Check for issues: `go vet`
- Verify documentation is complete: `go doc`
- Ensure code follows the naming and style conventions

## Resources
- [Effective Go](https://golang.org/doc/effective_go.html) - Official guide to writing effective Go code
- [Go Code Review Comments](https://github.com/golang/go/wiki/CodeReviewComments) - Common comments for code reviews
- [Go Style Guide](https://github.com/golang/go/wiki/CodeReviewComments) - Community-driven style guide
- [The Go Programming Language Specification](https://golang.org/ref/spec) - Official language specification

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.