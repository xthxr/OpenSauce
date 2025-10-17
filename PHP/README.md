# PHP Programming - DSA Contributions

Welcome to the PHP folder for DSA (Data Structures and Algorithms) contributions!

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
PHP (Hypertext Preprocessor) is a popular general-purpose scripting language that is especially suited to web development. It's a server-side scripting language with a primary focus on web development but can be used as a general-purpose programming language. PHP is known for its ease of use and rapid development capabilities.

## Getting Started
To contribute to this repository, you'll need PHP 7.4 or later.

### Prerequisites
- PHP 7.4 or later
- Basic knowledge of PHP programming
- Understanding of data structures and algorithms

### Installation
- Download and install PHP from php.net
- On macOS: Use Homebrew with `brew install php`
- On Linux: Use package manager (e.g., `sudo apt-get install php`)
- Verify installation with `php --version`

To run a PHP file:
```bash
php binary_search.php
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
- Maximum line length of 120 characters
- Use PSR-12 coding standards
- Use camelCase for methods and variables
- Use PascalCase for class names
- Use UPPER_SNAKE_CASE for constants
- Use strict comparison operators (===, !==) instead of loose comparison
- Use consistent spacing around operators
- Declare strict types with `declare(strict_types=1);` at the top of files
- Use type hints for function parameters and return types

## Naming Conventions
### Files
- Use PascalCase or snake_case for PHP source files: `BinarySearch.php`, `binary_search.php`
- Include problem name in filename if solving a specific problem
- Use `.php` extension

### Classes and Interfaces
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Methods and Variables
- Use camelCase: `binarySearch`, `currentNode`
- Use descriptive names that reflect purpose
- Use camelCase for variable names: `elementCount`, `isSorted`

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use PHPDoc format for class and method documentation
- Use // for single-line comments
- Use /* */ for multi-line comments
- Document all public methods and classes with purpose, parameters, and return values
- Include time and space complexity in method documentation
- Add brief descriptions for complex algorithms
- Include example usage in comments

### PHPDoc Format
Each class/method should include:
- Brief description of the algorithm/class
- @param tags for all parameters with type and description
- @return tag for return values with type and description
- @throws tag for exceptions
- @author tag with contributor name
- @since tag if applicable
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.

## Example Implementation
```php
<?php

declare(strict_types=1);

/**
 * Binary search implementation in PHP
 * Time Complexity: O(log n)
 * Space Complexity: O(1)
 * 
 * @param array $arr Sorted array to search in
 * @param int $target Element to search for
 * @return int Index of target element, or -1 if not found
 */
function binarySearch(array $arr, int $target): int
{
    $left = 0;
    $right = count($arr) - 1;
    
    while ($left <= $right) {
        $mid = $left + (int)(($right - $left) / 2);
        
        if ($arr[$mid] === $target) {
            return $mid;
        }
        
        if ($arr[$mid] < $target) {
            $left = $mid + 1;
        } else {
            $right = $mid - 1;
        }
    }
    
    return -1; // Element not found
}

// Example usage
$arr = [2, 3, 4, 10, 40];
$target = 10;
$result = binarySearch($arr, $target);

if ($result !== -1) {
    echo "Element found at index {$result}\n";
} else {
    echo "Element not found\n";
}
?>
```

## Testing Guidelines
### Test Structure
- Create test files in the same directory as the implementation
- Use `Test.php` suffix: `BinarySearchTest.php`

### Testing Framework
- Use PHPUnit for comprehensive testing
- Follow PSR standards for testing

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use data providers for multiple test cases

## Performance Benchmarks
### Benchmarking Tools
- Use microtime() for simple benchmarking
- Use Xdebug profiler for detailed analysis
- Use Blackfire for comprehensive profiling

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
- Verify documentation is complete (PHPDoc comments)
- Check that your code passes linting (use PHP_CodeSniffer)
- Use modern PHP features (7.4+ types, etc.) where appropriate

## Resources
- [PSR Standards](https://www.php-fig.org/psr/) - PHP Standard Recommendations
- [PHP The Right Way](https://phptherightway.com/) - Guide to PHP best practices
- [PHP Manual](https://www.php.net/manual/en/) - Official PHP documentation
- [PHP FIG](https://www.php-fig.org/) - Framework Interoperability Group

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.