# JavaScript Programming - DSA Contributions

Welcome to the JavaScript folder for DSA (Data Structures and Algorithms) contributions!

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
JavaScript is a high-level, dynamic, untyped, and interpreted programming language. It's primarily used for web development but has expanded to server-side development, mobile applications, and desktop applications. JavaScript is ideal for demonstrating algorithms due to its flexibility and ease of testing.

## Getting Started
To contribute to this repository, you'll need Node.js installed.

### Prerequisites
- Node.js (version 12 or later)
- npm (Node Package Manager) or yarn
- Basic knowledge of JavaScript programming
- Understanding of data structures and algorithms

### Installation
- Download and install Node.js from nodejs.org
- Verify installation with `node --version` and `npm --version`

To run a JavaScript file:
```bash
node binarySearch.js
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
- Use camelCase for variables and functions
- Use PascalCase for class names and constructor functions
- Use UPPER_SNAKE_CASE for constants
- Use semicolons consistently
- Use strict equality (===) instead of loose equality (==)
- Use const by default, let when variables need to be reassigned
- Use arrow functions when appropriate
- Use template literals for string concatenation
- Prefer functional programming methods (map, filter, reduce) when appropriate

## Naming Conventions
### Files
- Use camelCase for JavaScript source files: `binarySearch.js`, `bubbleSort.js`
- Include problem name in filename if solving a specific problem

### Variables and Functions
- Use camelCase: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose
- Use camelCase for variable names: `elementCount`, `isSorted`

### Classes
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with hyphens if needed: `dynamic-programming/`

## Documentation Requirements
### Comments
- Use JSDoc format for function documentation
- Use // for single-line comments
- Use /* */ for multi-line comments
- Document all functions with purpose, parameters, and return values
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in comments

### JSDoc Format
Each function should include:
- Brief description of the algorithm
- @param tags for all parameters with type and description
- @returns tag for return values with type and description
- @author tag with contributor name
- @since tag if applicable
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.

## Example Implementation
```javascript
/**
 * Binary search implementation in JavaScript
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Sorted array to search in
 * @param {number} target - Element to search for
 * @returns {number} - Index of target element, or -1 if not found
 */
function binarySearch(arr, target) {
    let left = 0;
    let right = arr.length - 1;
    
    while (left <= right) {
        const mid = Math.floor(left + (right - left) / 2);
        
        if (arr[mid] === target) {
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

// Example usage
const arr = [2, 3, 4, 10, 40];
const target = 10;
const result = binarySearch(arr, target);

if (result !== -1) {
    console.log(`Element found at index ${result}`);
} else {
    console.log('Element not found');
}

// Export for testing
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { binarySearch };
}
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `.test.js` or `.spec.js` suffix: `binarySearch.test.js`

### Testing Framework
- Use Jest for comprehensive testing
- Consider using Mocha with Chai for more flexibility

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use describe and it blocks for clear test organization

## Performance Benchmarks
### Benchmarking Tools
- Use console.time() and console.timeEnd() for simple benchmarks
- Use benchmark.js library for more accurate measurements
- Use Node.js built-in performance hooks

### Performance Metrics
- Execution time
- Memory usage
- Comparison with naive implementations
- Big O verification with different input sizes
- Browser performance tools (for frontend implementations)

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
- Verify documentation is complete (JSDoc comments)
- Check that your code passes linting (use ESLint or StandardJS)
- Use modern JavaScript features (ES6+) where appropriate

## Resources
- [MDN JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide) - Comprehensive JavaScript documentation
- [Airbnb JavaScript Style Guide](https://github.com/airbnb/javascript) - Popular JavaScript style guide
- [Eloquent JavaScript](https://eloquentjavascript.net/) - Free online book about JavaScript
- [You Don't Know JS](https://github.com/getify/You-Dont-Know-JS) - Book series about JavaScript

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.