# GitHub Actions Workflows

This directory contains automated workflows for the OpenSauce repository.

## Workflows

### 1. PR Code Validation and Auto-merge (`pr-validation.yml`)

Automatically validates pull requests and merges them if they meet quality standards.

#### When it runs
- When a PR is opened
- When a PR is updated (new commits pushed)
- When a PR is reopened

#### What it does

**For Valid PRs:**
1. ✅ Validates code quality against CONTRIBUTING.md standards
2. ✅ Adds labels: `Hacktoberfest` and `Hacktoberfest-accepted`
3. ✅ Approves the PR
4. ✅ Automatically merges the PR

**For Invalid PRs:**
1. ❌ Comments with message: "Your code is incorrect"
2. ❌ Lists specific validation errors
3. ❌ Requests changes
4. ❌ Does NOT merge

#### Validation Checks

The workflow validates that code files meet these requirements:

1. **Documentation**: Must include comments or documentation
2. **Complexity Analysis**: Must mention time and space complexity (e.g., "Time Complexity: O(n)")
3. **Examples/Tests**: Must include example usage, test cases, or a main function
4. **Syntax**: Basic syntax validation (balanced brackets, braces, parentheses)
5. **Content**: Must have meaningful content (at least 10 lines of actual code)
6. **No Spam**: Filters out obvious spam patterns

#### Supported Languages

- Python (`.py`)
- Java (`.java`)
- C++ (`.cpp`)
- C (`.c`)
- JavaScript (`.js`)
- TypeScript (`.ts`)
- Go (`.go`)
- Rust (`.rs`)
- Ruby (`.rb`)
- PHP (`.php`)
- Swift (`.swift`)
- Kotlin (`.kt`)
- Scala (`.scala`)
- Dart (`.dart`)
- Haskell (`.hs`)

#### Example: Good Code

```python
"""
Binary Search Algorithm

Time Complexity: O(log n)
Space Complexity: O(1)

Searches for a target value in a sorted array.
"""

def binary_search(arr, target):
    """Find target in sorted array."""
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
    arr = [1, 3, 5, 7, 9]
    print(binary_search(arr, 5))  # Output: 2
```

This code will **PASS** validation because it has:
- ✅ Documentation (docstrings)
- ✅ Complexity analysis
- ✅ Example usage
- ✅ Meaningful content

#### Example: Bad Code

```python
def test():
    return "test"
```

This code will **FAIL** validation because:
- ❌ No documentation
- ❌ No complexity analysis
- ❌ No example usage
- ❌ Insufficient content

### 2. Hacktoberfest Issue Labeling (`hacktoberfest-automation.yml`)

Automatically adds Hacktoberfest labels to new issues.

#### When it runs
- When a new issue is opened

#### What it does
- Adds labels: `Hacktoberfest` and `good first issue`

## Permissions

These workflows require the following permissions:
- `contents: write` - To merge PRs
- `pull-requests: write` - To approve/request changes on PRs
- `issues: write` - To add labels and comments

## Notes

- The auto-merge feature uses the `squash` merge method
- Labels must exist in the repository for the workflow to add them
- PRs with merge conflicts cannot be auto-merged
- The workflow re-runs automatically when PRs are updated
