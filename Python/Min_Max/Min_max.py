# Python3 program to play Tic-Tac-Toe using Minimax Algorithm in an infinite loop

player, opponent = 'x', 'o'

def isMovesLeft(board):
    for i in range(3):
        for j in range(3):
            if board[i][j] == '_':
                return True
    return False

def evaluate(b):
    # Checking for Rows
    for row in range(3):
        if b[row][0] == b[row][1] == b[row][2]:
            if b[row][0] == player:
                return 10
            elif b[row][0] == opponent:
                return -10

    # Checking for Columns
    for col in range(3):
        if b[0][col] == b[1][col] == b[2][col]:
            if b[0][col] == player:
                return 10
            elif b[0][col] == opponent:
                return -10

    # Checking Diagonals
    if b[0][0] == b[1][1] == b[2][2]:
        if b[0][0] == player:
            return 10
        elif b[0][0] == opponent:
            return -10

    if b[0][2] == b[1][1] == b[2][0]:
        if b[0][2] == player:
            return 10
        elif b[0][2] == opponent:
            return -10

    return 0

def minimax(board, depth, isMax):
    score = evaluate(board)

    if score == 10:
        return score - depth  # prefer faster wins
    if score == -10:
        return score + depth  # prefer slower losses
    if not isMovesLeft(board):
        return 0

    if isMax:
        best = -1000
        for i in range(3):
            for j in range(3):
                if board[i][j] == '_':
                    board[i][j] = player
                    best = max(best, minimax(board, depth + 1, False))
                    board[i][j] = '_'
        return best
    else:
        best = 1000
        for i in range(3):
            for j in range(3):
                if board[i][j] == '_':
                    board[i][j] = opponent
                    best = min(best, minimax(board, depth + 1, True))
                    board[i][j] = '_'
        return best

def findBestMove(board):
    bestVal = -1000
    bestMove = (-1, -1)

    for i in range(3):
        for j in range(3):
            if board[i][j] == '_':
                board[i][j] = player
                moveVal = minimax(board, 0, False)
                board[i][j] = '_'
                if moveVal > bestVal:
                    bestMove = (i, j)
                    bestVal = moveVal
    return bestMove

def printBoard(board):
    for row in board:
        print(" | ".join(row))
    print()

# Infinite Game Loop
while True:
    board = [
        ['_', '_', '_'],
        ['_', '_', '_'],
        ['_', '_', '_']
    ]
    print("New Game Started! You are 'o'. AI is 'x'.")
    printBoard(board)

    while True:
        # Player move
        try:
            r, c = map(int, input("Enter your move (row and column: 0 1 2): ").split())
        except:
            print("Invalid input! Please enter two numbers between 0 and 2.")
            continue

        if 0 <= r < 3 and 0 <= c < 3 and board[r][c] == '_':
            board[r][c] = opponent
        else:
            print("Invalid move! Try again.")
            continue

        print("\nBoard after your move:")
        printBoard(board)

        if evaluate(board) == -10:
            print("🎉 You win!")
            break
        elif not isMovesLeft(board):
            print("😐 It's a tie!")
            break

        # AI move
        bestMove = findBestMove(board)
        board[bestMove[0]][bestMove[1]] = player

        print("AI played:")
        printBoard(board)

        if evaluate(board) == 10:
            print("🤖 AI wins!")
            break
        elif not isMovesLeft(board):
            print("😐 It's a tie!")
            break

    # Ask to play again
    ans = input("Play again? (y/n): ").lower()
    if ans != 'y':
        print("Thanks for playing! 👋")
        break
