def solveNQueens(n):
    """
    Solves the N-Queens problem using backtracking.

    The goal is to place N queens on an N×N chessboard such that
    no two queens threaten each other (no same row, column, or diagonal).

    Returns a list of all valid board configurations.
    Each configuration is represented as a list of strings.

    Time Complexity: O(N!)
    Space Complexity: O(N^2)
    """
    
    # Result list to store all valid configurations
    result = []

    # Current board state (initialize with '.')
    board = [["."] * n for _ in range(n)]
    
    # Helper function to check if placing a queen is valid
    def is_safe(row, col):
        # Check column
        for i in range(row):
            if board[i][col] == "Q":
                return False
        
        # Check upper left diagonal
        i, j = row - 1, col - 1
        while i >= 0 and j >= 0:
            if board[i][j] == "Q":
                return False
            i -= 1
            j -= 1

        # Check upper right diagonal
        i, j = row - 1, col + 1
        while i >= 0 and j < n:
            if board[i][j] == "Q":
                return False
            i -= 1
            j += 1

        return True
    
    # Backtracking function to try placing queens row by row
    def backtrack(row):
        # Base case: all queens placed
        if row == n:
            # Add a valid configuration
            result.append(["".join(r) for r in board])
            return
        
        # Try placing a queen in each column of the current row
        for col in range(n):
            if is_safe(row, col):
                board[row][col] = "Q"   # Place queen
                backtrack(row + 1)      # Move to next row
                board[row][col] = "."   # Backtrack (remove queen)
    
    # Start the backtracking from the first row
    backtrack(0)
    
    return result


# -------------------------------
# Test Cases
# -------------------------------
if __name__ == "__main__":
    # Example: n = 4
    solutions = solveNQueens(4)
    print(f"Total Solutions: {len(solutions)}")
    for sol in solutions:
        for row in sol:
            print(row)
        print()
    
    # Example: n = 1
    print(solveNQueens(1))  # Expected: [['Q']]
