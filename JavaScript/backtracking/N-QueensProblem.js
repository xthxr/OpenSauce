/**
 * 🧩 N-Queens Problem (Backtracking Solution in JavaScript)
 *
 * The N-Queens problem is a classic backtracking challenge where we must place N queens
 * on an N×N chessboard such that no two queens attack each other. That means:
 * - No two queens share the same row
 * - No two queens share the same column
 * - No two queens share the same diagonal
 *
 * This solution uses backtracking to explore all valid placements row by row.
 * When a valid board configuration is found, it is added to the result.
 *
 * Time Complexity: O(N!) — due to recursive placement checks.
 * Space Complexity: O(N^2) — for storing board states.
 */

function solveNQueens(n) {
  const board = Array.from({ length: n }, () => Array(n).fill('.'));
  const results = [];

  function isSafe(row, col) {
    // Check column
    for (let i = 0; i < row; i++) {
      if (board[i][col] === 'Q') return false;
    }

    // Check upper-left diagonal
    for (let i = row - 1, j = col - 1; i >= 0 && j >= 0; i--, j--) {
      if (board[i][j] === 'Q') return false;
    }

    // Check upper-right diagonal
    for (let i = row - 1, j = col + 1; i >= 0 && j < n; i--, j++) {
      if (board[i][j] === 'Q') return false;
    }

    return true;
  }

  function backtrack(row = 0) {
    if (row === n) {
      results.push(board.map(r => r.join('')));
      return;
    }

    for (let col = 0; col < n; col++) {
      if (isSafe(row, col)) {
        board[row][col] = 'Q';
        backtrack(row + 1);
        board[row][col] = '.'; // backtrack
      }
    }
  }

  backtrack();
  return results;
}

