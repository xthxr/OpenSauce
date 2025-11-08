# Rust Programming - DSA Contributions

Welcome to the Rust folder for DSA (Data Structures and Algorithms) contributions!

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
Rust is a systems programming language that runs blazingly fast, prevents segfaults, and guarantees thread safety. It combines memory safety without garbage collection, making it ideal for performance-critical applications and systems where efficiency is paramount. Rust's ownership model ensures memory safety without requiring a garbage collector.

## Getting Started
To contribute to this repository, you'll need Rust 1.40 or later.

### Prerequisites
- Rust 1.40 or later
- Cargo (Rust's package manager and build tool)
- Basic knowledge of Rust programming
- Understanding of data structures and algorithms

### Installation
- Install Rust and Cargo using rustup: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
- Verify installation with `rustc --version` and `cargo --version`

To run a Rust file:
```bash
# For simple single-file programs
rustc binary_search.rs && ./binary_search

# For projects managed with Cargo
cargo run
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
- Follow Rust style guide: `cargo fmt` and `cargo clippy`
- Use snake_case for functions and variables
- Use PascalCase for structs, enums, and traits
- Use UPPER_SNAKE_CASE for constants
- Prefer immutable bindings (let) over mutable (let mut)
- Use proper error handling with Result and Option types
- Use early returns instead of nested conditionals when possible
- Use iterators instead of loops when appropriate
- Implement proper trait bounds for generic functions

## Naming Conventions
### Files
- Use snake_case for Rust source files: `binary_search.rs`, `bubble_sort.rs`
- Include problem name in filename if solving a specific problem

### Functions and Variables
- Use snake_case: `binary_search`, `current_node`
- Use descriptive names that reflect purpose
- Use snake_case for variable names: `element_count`, `is_sorted`

### Structs and Enums
- Use PascalCase: `BinarySearchTree`, `LinkedList`
- Use descriptive names that reflect purpose

### Constants
- Use UPPER_SNAKE_CASE: `MAX_SIZE`, `DEFAULT_CAPACITY`

### Directories
- Use lowercase with underscores if needed: `dynamic_programming/`

## Documentation Requirements
### Comments
- Use /// for documentation comments (supports Markdown)
- Use //! for module-level documentation
- Use // for single-line comments
- Document all public functions, structs, enums, and traits
- Include time and space complexity in function documentation
- Add brief descriptions for complex algorithms
- Include example usage in documentation

### Documentation Format
Each public item should include:
- Brief description of the algorithm/structure
- Parameters with descriptions
- Return values with descriptions
- Error conditions if applicable
- Time complexity: O(n), O(n log n), etc.
- Space complexity: O(n), O(1), etc.
- Example usage in documentation

## Example Implementation
```rust
/// Binary search implementation in Rust
/// Time Complexity: O(log n)
/// Space Complexity: O(1)
/// 
/// # Arguments
/// 
/// * `arr` - A sorted slice of integers to search in
/// * `target` - Element to search for
/// 
/// # Returns
/// 
/// Index of target element, or None if not found
pub fn binary_search(arr: &[i32], target: i32) -> Option<usize> {
    let mut left = 0;
    let mut right = arr.len();
    
    while left < right {
        let mid = left + (right - left) / 2;
        
        match arr[mid].cmp(&target) {
            std::cmp::Ordering::Equal => return Some(mid),
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Greater => right = mid,
        }
    }
    
    None // Element not found
}

fn main() {
    let arr = [2, 3, 4, 10, 40];
    let target = 10;
    match binary_search(&arr, target) {
        Some(index) => println!("Element found at index {}", index),
        None => println!("Element not found"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_search_found() {
        let arr = [2, 3, 4, 10, 40];
        assert_eq!(binary_search(&arr, 10), Some(3));
    }

    #[test]
    fn test_binary_search_not_found() {
        let arr = [2, 3, 4, 10, 40];
        assert_eq!(binary_search(&arr, 5), None);
    }
}
```

## Testing Guidelines
### Test Structure
- Include tests within the same file using #[cfg(test)] module
- Create separate test files in tests/ directory for integration tests
- Use _test.rs suffix for integration test files

### Testing Framework
- Use built-in test framework with #[test] attribute
- Use assert_eq! and assert_ne! macros for comparisons

### Test Requirements
- Include edge cases (empty arrays, single elements, etc.)
- Test both positive and negative scenarios
- Verify time and space complexity claims
- Test boundary conditions
- Include performance tests for algorithms
- Use property-based testing when appropriate

## Performance Benchmarks
### Benchmarking Tools
- Use Criterion crate for comprehensive benchmarking
- Use built-in test framework with #[bench] attribute (unstable)

### Performance Metrics
- Execution time
- Memory allocation patterns
- Zero-cost abstractions verification
- Comparison with other implementations
- Big O verification

## Contribution Guidelines
1. Fork the repository
2. Create a new branch for your feature: `git checkout -b feature/my-feature`
3. Follow the coding and documentation standards
4. Add comprehensive tests
5. Create the implementation in the appropriate directory
6. Submit a pull request

### Before Submitting
- Test your code thoroughly: `cargo test`
- Format your code: `cargo fmt`
- Check for common issues: `cargo clippy`
- Ensure code follows the naming and style conventions
- Verify documentation is complete (run `cargo doc --document-private-items`)
- Ensure safe Rust practices (avoid unnecessary use of unsafe)

## Resources
- [Rust Book](https://doc.rust-lang.org/book/) - Official Rust guide
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/) - Practical Rust examples
- [Rust Style Guide](https://github.com/rust-dev-tools/fmt-rfcs/blob/master/guide/guide.md) - Rust formatting guidelines
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/) - Best practices for designing Rust APIs

## Code of Conduct
Please follow our [Code of Conduct](../../CODE_OF_CONDUCT.md).

## License
This project is licensed under the MIT License - see the [LICENSE](../../LICENSE) file for details.