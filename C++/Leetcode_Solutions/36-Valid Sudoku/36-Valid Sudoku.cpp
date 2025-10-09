#include <bits/stdc++.h>
using namespace std;

// Define a class named Solution
class Solution {
public:
    // Main function to check if the given Sudoku board is valid
    bool isValidSudoku(vector<vector<char>>& board) {
        // Declare 2D boolean arrays to track digits (1–9) seen in:
        // rows[i][num] = true if number 'num' is seen in row i
        bool rows[9][9] = {false};  

        // cols[j][num] = true if number 'num' is seen in column j
        bool cols[9][9] = {false};  

        // boxes[boxIndex][num] = true if number 'num' is seen in 3x3 box with index boxIndex
        bool boxes[9][9] = {false};  

        // Iterate through each cell in the 9x9 board
        for (int i = 0; i < 9; i++) {          // Loop over each row
            for (int j = 0; j < 9; j++) {      // Loop over each column in the row
                // Only check cells that are not empty ('.')
                if (board[i][j] != '.') {
                    // Convert the character digit ('1' to '9') to a 0-based index (0 to 8)
                    int num = board[i][j] - '1';

                    // Calculate which 3x3 box this cell belongs to
                    // There are 9 boxes:
                    // boxIndex = 0 to 8
                    // For example: cell (4,7) is in box (1,2) => index = 1*3 + 2 = 5
                    int boxIndex = (i / 3) * 3 + (j / 3);

                    // Check if this number has already been seen in:
                    // - the current row (rows[i][num])
                    // - the current column (cols[j][num])
                    // - the current 3x3 box (boxes[boxIndex][num])
                    if (rows[i][num] || cols[j][num] || boxes[boxIndex][num]) {
                        return false;  // Duplicate found: board is invalid
                    }

                    // Mark this number as seen in the current row, column, and box
                    rows[i][num] = true;
                    cols[j][num] = true;
                    boxes[boxIndex][num] = true;
                }
            }
        }

        // If no duplicates found in any row, column, or box — the board is valid
        return true;
    }
};
