# TypeScript Programming - DSA Contributions

Welcome to the TypeScript folder for DSA (Data Structures and Algorithms) contributions!

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
TypeScript is a strongly typed programming language that builds on JavaScript, giving you better tooling at any scale. It's a superset of JavaScript that compiles to plain JavaScript, adding optional static typing and other features. TypeScript provides compile-time error checking, better IDE support, and cleaner, more maintainable code.

## Getting Started
To contribute to this repository, you'll need Node.js and TypeScript.

### Prerequisites
- Node.js (version 12 or later)
- TypeScript (version 4.0 or later)
- Basic knowledge of TypeScript/JavaScript programming
- Understanding of data structures and algorithms

### Installation
- Install Node.js from nodejs.org
- Install TypeScript globally: `npm install -g typescript`
- Install TypeScript compiler: `tsc --version`

To run a TypeScript file:
```bash
# Compile TypeScript to JavaScript
tsc binarySearch.ts
# Run the compiled JavaScript
node binarySearch.js

# Or use ts-node for direct execution
npx ts-node binarySearch.ts
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
- Use PascalCase for class names, interfaces, and types
- Use UPPER_SNAKE_CASE for constants
- Use semicolons consistently
- Use strict equality (===) instead of loose equality (==)
- Use type annotations for function parameters and return values
- Use interfaces for object shapes
- Use type aliases for primitive unions
- Use generics when possible for type safety
- Prefer const over let, let over var

## Naming Conventions
### Files
- Use camelCase for TypeScript source files: `binarySearch.ts`, `bubbleSort.ts`
- Include problem name in filename if solving a specific problem

### Classes, Interfaces, and Types
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Functions, Variables, and Properties
- Use camelCase: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose
- Use camelCase for property names: `elementCount`, `isSorted`

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with hyphens if needed: `dynamic-programming/`

## Documentation Requirements
### Comments
- Use TSDoc format for function and class documentation (extends JSDoc)
- Use // for single-line comments
- Use /* */ for multi-line comments
- Document all exported functions, classes, interfaces, and types
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in comments

### TSDoc Format
Each exported function/class should include:
- Brief description of the algorithm/class
- @param tags for all parameters with type and description
- @returns tag for return values with type and description
- @author tag with contributor name
- @since tag if applicable
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```typescript
/**
 * Binary search implementation in TypeScript
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 * 
 * @param arr - Sorted array to search in
 * @param target - Element to search for
 * @returns Index of target element, or -1 if not found
 */
function binarySearch(arr: number[], target: number): number {
    let left: number = 0;
    let right: number = arr.length - 1;
    
    while (left <= right) {
        const mid: number = Math.floor(left + (right - left) / 2);
        
        if (arr[mid] === target) {
            return mid;
        }
        
        if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    
    return -1; // Element not found
}

// Example usage
const arr: number[] = [2, 3, 4, 10, 40];
const target: number = 10;
const result: number = binarySearch(arr, target);

if (result !== -1) {
    console.log(`Element found at index ${result}`);
} else {
    console.log('Element not found');
}

// Export for testing
export { binarySearch };
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `.spec.ts` or `.test.ts` suffix: `binarySearch.test.ts`

### Testing Framework
- Use Jest with TypeScript support
- Consider using Mocha/Chai with TypeScript definitions

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
- Use benchmark.js with TypeScript definitions
- Use Node.js built-in performance hooks

### Performance Metrics
- Execution time
- Memory usage
- Type checking overhead
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
- Verify documentation is complete (TSDoc comments)
- Check that your code passes type checking: `tsc --noEmit`
- Check that your code passes linting (use ESLint with TypeScript preset)
- Use modern TypeScript features where appropriate

## Resources
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) - Official TypeScript documentation
- [TypeScript Deep Dive](https://basarat.gitbooks.io/typescript/) - Comprehensive TypeScript guide
- [TSLint/ESLint](https://typescript-eslint.io/) - TypeScript linting rules
- [TypeScript Config Reference](https://www.typescriptlang.org/tsconfig) - TypeScript compiler options

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.