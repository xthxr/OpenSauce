import math

# Initialize board
board = [
    ['-', '-', '-'],
    ['-', '-', '-'],
    ['-', '-', '-']
]

AI = "X"
HUMAN = "O"

# Function to display the board
def print_board(b):
    print("\n")
    for row in b:
        print(" | ".join(row))
    print("\n")

# Check for winner
def check_winner(b):
    for row in b:
        if row[0] == row[1] == row[2] != '-':
            return row[0]
    for col in range(3):
        if b[0][col] == b[1][col] == b[2][col] != '-':
            return b[0][col]
    if b[0][0] == b[1][1] == b[2][2] != '-':
        return b[0][0]
    if b[0][2] == b[1][1] == b[2][0] != '-':
        return b[0][2]
    return None

# Check for empty spaces
def is_moves_left(b):
    for row in b:
        if '-' in row:
            return True
    return False

# Minimax function
def minimax(b, depth, is_maximizing):
    winner = check_winner(b)
    if winner == AI:
        return 1
    elif winner == HUMAN:
        return -1
    elif not is_moves_left(b):
        return 0

    if is_maximizing:
        best_score = -math.inf
        for i in range(3):
            for j in range(3):
                if b[i][j] == '-':
                    b[i][j] = AI
                    score = minimax(b, depth + 1, False)
                    b[i][j] = '-'
                    best_score = max(score, best_score)
        return best_score
    else:
        best_score = math.inf
        for i in range(3):
            for j in range(3):
                if b[i][j] == '-':
                    b[i][j] = HUMAN
                    score = minimax(b, depth + 1, True)
                    b[i][j] = '-'
                    best_score = min(score, best_score)
        return best_score

# AI finds best move
def find_best_move(b):
    best_score = -math.inf
    best_move = None
    for i in range(3):
        for j in range(3):
            if b[i][j] == '-':
                b[i][j] = AI
                score = minimax(b, 0, False)
                b[i][j] = '-'
                if score > best_score:
                    best_score = score
                    best_move = (i, j)
    return best_move

# Main game loop
def play_game():
    print("Welcome to Tic-Tac-Toe!")
    print("You are 'O' and AI is 'X'")
    print_board(board)

    while True:
        # Human move
        while True:
            try:
                row = int(input("Enter row (0-2): "))
                col = int(input("Enter col (0-2): "))
                if board[row][col] == '-':
                    board[row][col] = HUMAN
                    break
                else:
                    print("That spot is already taken!")
            except (ValueError, IndexError):
                print("Invalid input! Enter numbers between 0 and 2.")

        print_board(board)
        if check_winner(board) == HUMAN:
            print("🎉 You win!")
            break
        if not is_moves_left(board):
            print("It's a draw!")
            break

        # AI move
        print("AI is thinking...")
        move = find_best_move(board)
        if move:
            board[move[0]][move[1]] = AI

        print_board(board)
        if check_winner(board) == AI:
            print("💻 AI wins!")
            break
        if not is_moves_left(board):
            print("It's a draw!")
            break

# Infinite loop to restart the game
while True:
    # Reset board for a new game
    board = [['-', '-', '-'] for _ in range(3)]
    play_game()
    again = input("Play again? (y/n): ").lower()
    if again != 'y':
        print("Thanks for playing!")
        break
