# Contributing to OpenSauce

Thank you for your interest in contributing to OpenSauce! This document provides guidelines and instructions for contributing.

## 🎯 Getting Started

### Prerequisites
- A GitHub account
- Git installed on your local machine
- Basic knowledge of the programming language you want to contribute in
- Understanding of Data Structures and Algorithms

### First-Time Contributors
If this is your first contribution to open source:
1. Read through this guide completely
2. Check out [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)
3. Look for beginner-friendly issues or start with a simple algorithm implementation

## 🔄 Contribution Workflow

### 1. Fork and Clone
```bash
# Fork the repository on GitHub, then clone your fork
git clone https://github.com/your-username/OpenSauce.git
cd OpenSauce

# Add upstream remote
git remote add upstream https://github.com/xthxr/OpenSauce.git
```

### 2. Create a Branch
```bash
# Create and switch to a new branch
git checkout -b feature/algorithm-name

# Branch naming conventions:
# - feature/binary-search-python
# - add/merge-sort-java
# - fix/bubble-sort-cpp
```

### 3. Make Your Changes
- Navigate to the appropriate language folder
- Create subdirectories for organizing by topic (e.g., `sorting/`, `trees/`)
- Write your code following the language-specific guidelines
- Add comprehensive comments

### 4. Commit Your Changes
```bash
# Stage your changes
git add .

# Commit with a meaningful message
git commit -m "Add binary search implementation in Python"

# Commit message format:
# - "Add [algorithm] in [language]"
# - "Fix [issue] in [algorithm]"
# - "Update [algorithm] documentation"
```

### 5. Keep Your Fork Updated
```bash
# Fetch latest changes from upstream
git fetch upstream

# Merge upstream changes
git merge upstream/main
```

### 6. Push and Create PR
```bash
# Push to your fork
git push origin feature/algorithm-name

# Then create a Pull Request on GitHub
```

## ✅ Code Quality Guidelines

### General Requirements
1. **Originality**: Write your own code or properly attribute sources
2. **Documentation**: Include clear comments explaining the algorithm
3. **Complexity Analysis**: Add time and space complexity in comments
4. **Testing**: Provide example usage or test cases
5. **Formatting**: Follow language-specific style guides

### Code Structure Template

**For algorithms:**
```
1. Brief description of the algorithm
2. Time complexity: O(?)
3. Space complexity: O(?)
4. Implementation
5. Example usage/test cases
```

**Example (Python):**
```python
"""
Binary Search Algorithm

Time Complexity: O(log n)
Space Complexity: O(1)

Searches for a target value in a sorted array using divide and conquer.
"""

def binary_search(arr, target):
    """
    Performs binary search on a sorted array.
    
    Args:
        arr: Sorted list of comparable elements
        target: Element to search for
    
    Returns:
        Index of target if found, -1 otherwise
    """
    left, right = 0, len(arr) - 1
    
    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1

# Example usage
if __name__ == "__main__":
    arr = [1, 3, 5, 7, 9, 11, 13]
    target = 7
    result = binary_search(arr, target)
    print(f"Element found at index: {result}")
```

### Language-Specific Guidelines

#### C/C++
- Use descriptive variable names
- Include necessary headers
- Add function prototypes if needed
- Use proper memory management

#### Java
- Follow camelCase for methods, PascalCase for classes
- Include proper package declarations if needed
- Use Java Collections Framework appropriately

#### Python
- Follow PEP 8 style guide
- Use type hints (Python 3.5+)
- Include docstrings
- Use meaningful variable names

#### JavaScript/TypeScript
- Use ES6+ features
- Follow camelCase convention
- Add JSDoc/TSDoc comments
- Use proper error handling

## 📂 File Organization

### Directory Structure
Organize your code by topic within each language folder:

```
Language/
├── README.md
├── arrays/
│   ├── two_sum.ext
│   └── rotate_array.ext
├── sorting/
│   ├── bubble_sort.ext
│   ├── quick_sort.ext
│   └── merge_sort.ext
├── trees/
│   ├── binary_tree.ext
│   └── bfs_traversal.ext
└── graphs/
    ├── dfs.ext
    └── dijkstra.ext
```

### Naming Conventions
- **C/C++**: `snake_case.c`, `snake_case.cpp`
- **Java/Kotlin**: `PascalCase.java`, `PascalCase.kt`
- **Python**: `snake_case.py`
- **JavaScript**: `camelCase.js`
- **Go**: `snake_case.go`
- **Rust**: `snake_case.rs`

## 🚫 What NOT to Do

❌ **Don't submit:**
- Duplicate implementations (check existing code first)
- Code without proper documentation
- Code copied from other sources without attribution
- Spam or meaningless contributions
- Code that doesn't run or compile
- Unrelated files (build artifacts, IDE configs, etc.)

❌ **Don't:**
- Submit multiple PRs for the same algorithm
- Make unnecessary formatting changes to existing code
- Submit broken or untested code

## ✨ PR Review Process

### What Happens After You Submit
1. **Automated Checks**: Basic checks run automatically
2. **Review**: Maintainers review your code
3. **Feedback**: You may receive suggestions for improvements
4. **Approval**: Once approved, your PR will be merged

### Making Changes After Review
```bash
# Make requested changes
git add .
git commit -m "Address review comments"
git push origin feature/algorithm-name
```

## 🏆 Recognition

All contributors will be:
- Listed in the Contributors section
- Credited in the commit history
- Eligible for Hacktoberfest rewards (during October)

## 📧 Getting Help

- **Questions?** Open an issue with the `question` label
- **Found a bug?** Open an issue with the `bug` label
- **Have an idea?** Open an issue with the `enhancement` label

## 🎯 Hacktoberfest Specific Guidelines

If contributing for Hacktoberfest:
1. Register on [hacktoberfest.com](https://hacktoberfest.com)
2. Make quality contributions (not spam)
3. Complete 4 valid PRs during October
4. Wait for PRs to be reviewed and accepted

### Quality Standards for Hacktoberfest
- PRs must add value to the repository
- No minor formatting or whitespace changes only
- Must follow all contribution guidelines above

## 📜 Code of Conduct

### Our Standards
- Be respectful and inclusive
- Provide constructive feedback
- Accept constructive criticism gracefully
- Focus on what's best for the community

### Unacceptable Behavior
- Harassment or discriminatory language
- Trolling or insulting comments
- Spam or promotional content
- Publishing others' private information

## 📚 Additional Resources

- [GitHub Flow Guide](https://guides.github.com/introduction/flow/)
- [Writing Good Commit Messages](https://chris.beams.io/posts/git-commit/)
- [How to Write a Good PR Description](https://github.blog/2015-01-21-how-to-write-the-perfect-pull-request/)

---

Thank you for contributing to OpenSauce! Happy coding! 🎉
