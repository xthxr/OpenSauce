# 36. Valid Sudoku

## Problem Description

Determine if a `9 x 9` Sudoku board is valid. Only the filled cells need to be validated according to the following rules:

1. **Each row** must contain the digits `1-9` without repetition
2. **Each column** must contain the digits `1-9` without repetition  
3. **Each of the nine 3 x 3 sub-boxes** of the grid must contain the digits `1-9` without repetition

### Important Notes:
- A Sudoku board (partially filled) could be valid but is not necessarily solvable
- Only the **filled cells** need to be validated according to the mentioned rules
- Empty cells are represented by `'.'`

## Examples

### Example 1:
```
Input: board = 
[["5","3",".",".","7",".",".",".","."],
 ["6",".",".","1","9","5",".",".","."],
 [".","9","8",".",".",".",".","6","."],
 ["8",".",".",".","6",".",".",".","3"],
 ["4",".",".","8",".","3",".",".","1"],
 ["7",".",".",".","2",".",".",".","6"],
 [".","6",".",".",".",".","2","8","."],
 [".",".",".","4","1","9",".",".","5"],
 [".",".",".",".","8",".",".","7","9"]]

Output: true
```

### Example 2:
```
Input: board = 
[["8","3",".",".","7",".",".",".","."],
 ["6",".",".","1","9","5",".",".","."],
 [".","9","8",".",".",".",".","6","."],
 ["8",".",".",".","6",".",".",".","3"],
 ["4",".",".","8",".","3",".",".","1"],
 ["7",".",".",".","2",".",".",".","6"],
 [".","6",".",".",".",".","2","8","."],
 [".",".",".","4","1","9",".",".","5"],
 [".",".",".",".","8",".",".","7","9"]]

Output: false
```
**Explanation:** Same as Example 1, except with the `5` in the top left corner being modified to `8`. Since there are two `8`'s in the top left 3x3 sub-box, it is invalid.

## Constraints

- `board.length == 9`
- `board[i].length == 9`  
- `board[i][j]` is a digit `1-9` or `'.'`

---

## Solution Approach & Intuition

This problem requires validating three different constraints simultaneously while traversing the board only once.

### Approach: Single Pass with Hash Tables
**Time Complexity:** O(81) = O(1) | **Space Complexity:** O(243) = O(1)

> Note: Since the board size is fixed at 9×9, the complexity is technically constant.

### Key Insights

1. **Single Traversal**: We can check all three constraints (rows, columns, boxes) in one pass
2. **Hash Table Tracking**: Use boolean arrays to track which numbers have been seen
3. **Box Index Calculation**: Each 3×3 box can be uniquely identified by an index
4. **Early Termination**: Return false immediately when a duplicate is found

### Algorithm Breakdown

#### 1. Data Structures Setup
```cpp
bool rows[9][9] = {false};    // rows[i][num] = true if 'num' seen in row i
bool cols[9][9] = {false};    // cols[j][num] = true if 'num' seen in column j  
bool boxes[9][9] = {false};   // boxes[boxIndex][num] = true if 'num' seen in box
```

#### 2. Box Index Calculation
The most crucial part is calculating which 3×3 box a cell belongs to:

```cpp
int boxIndex = (i / 3) * 3 + (j / 3);
```

**Box Index Mapping:**
```
+-------+-------+-------+
|   0   |   1   |   2   |
| (0,0) | (0,1) | (0,2) |
+-------+-------+-------+
|   3   |   4   |   5   |
| (1,0) | (1,1) | (1,2) |
+-------+-------+-------+
|   6   |   7   |   8   |
| (2,0) | (2,1) | (2,2) |
+-------+-------+-------+
```

For cell `(i,j)`:
- `i/3` gives the box row (0, 1, or 2)
- `j/3` gives the box column (0, 1, or 2)
- `boxIndex = (box_row * 3) + box_column`

#### 3. Validation Logic
```cpp
for (int i = 0; i < 9; i++) {
    for (int j = 0; j < 9; j++) {
        if (board[i][j] != '.') {
            int num = board[i][j] - '1';  // Convert '1'-'9' to 0-8
            int boxIndex = (i / 3) * 3 + (j / 3);
            
            // Check for duplicates
            if (rows[i][num] || cols[j][num] || boxes[boxIndex][num]) {
                return false;  // Invalid!
            }
            
            // Mark as seen
            rows[i][num] = cols[j][num] = boxes[boxIndex][num] = true;
        }
    }
}
return true;  // All validations passed
```

### Step-by-Step Process

1. **Initialize**: Create three 2D boolean arrays to track seen numbers
2. **Traverse**: Go through each cell in the 9×9 board
3. **Skip Empty**: Ignore cells containing `'.'`
4. **Convert**: Transform character digit to array index (`'1'` → `0`, `'9'` → `8`)
5. **Calculate Box**: Determine which 3×3 box the current cell belongs to
6. **Check Duplicates**: Verify if the number was already seen in:
   - Current row
   - Current column  
   - Current 3×3 box
7. **Update State**: Mark the number as seen in all three tracking arrays
8. **Return Result**: True if no duplicates found, false otherwise

### Why This Approach Works

- **Efficiency**: Single pass through the board
- **Memory Optimization**: Fixed-size arrays (no dynamic allocation)
- **Early Detection**: Stops immediately upon finding a violation
- **Comprehensive**: Checks all three Sudoku rules simultaneously

### Example Walkthrough

For the cell `board[4][7] = '3'` (row 4, column 7):

1. **Convert**: `'3' - '1' = 2` (array index)
2. **Box Index**: `(4/3) * 3 + (7/3) = 1 * 3 + 2 = 5`
3. **Check**: 
   - `rows[4][2]` - Has '3' been seen in row 4?
   - `cols[7][2]` - Has '3' been seen in column 7?
   - `boxes[5][2]` - Has '3' been seen in box 5?
4. **Update**: Set all three to `true`

### Alternative Approaches

- **Set-Based Solution**: Use `unordered_set` for each row/column/box
- **Three Separate Passes**: Check rows, then columns, then boxes
- **Bit Manipulation**: Use integers as bit arrays for space optimization

## Implementation

The complete optimized solution can be found in the `36-Valid Sudoku.cpp` file in this directory.