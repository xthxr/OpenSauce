# Tic-Tac-Toe with Minimax + Alpha-Beta pruning
# Board uses: 'x' for X, 'o' for O, '_' for empty

import math
import random

# Utility / helpers
def print_board(board):
    for row in board:
        print(' '.join(row))
    print()

def is_moves_left(board):
    for r in board:
        if '_' in r:
            return True
    return False

def evaluate(board, ai_player, human_player):
    """
    Returns:
      +10  if ai_player has three in a row (win)
      -10  if human_player has three in a row (loss)
       0   otherwise (no winner)
    We subtract depth in minimax to prefer faster wins / slower losses if needed
    """
    # rows
    for row in board:
        if row[0] == row[1] == row[2] != '_':
            return 10 if row[0] == ai_player else -10

    # cols
    for c in range(3):
        if board[0][c] == board[1][c] == board[2][c] != '_':
            return 10 if board[0][c] == ai_player else -10

    # diagonals
    if board[0][0] == board[1][1] == board[2][2] != '_':
        return 10 if board[0][0] == ai_player else -10
    if board[0][2] == board[1][1] == board[2][0] != '_':
        return 10 if board[0][2] == ai_player else -10

    return 0

def minimax_alpha_beta(board, depth, is_maximizing, alpha, beta, ai_player, human_player):
    """
    Minimax with Alpha-Beta pruning.
    - is_maximizing: True if current node is maximizing player's turn (AI)
    - returns best score (int)
    """
    score = evaluate(board, ai_player, human_player)

    # Terminal checks
    if score == 10:
        # AI has won
        return score - depth  # prefer faster win
    if score == -10:
        # Human has won
        return score + depth  # prefer slower loss
    if not is_moves_left(board):
        return 0  # draw

    if is_maximizing:
        best = -math.inf
        for i in range(3):
            for j in range(3):
                if board[i][j] == '_':
                    board[i][j] = ai_player
                    val = minimax_alpha_beta(board, depth + 1, False, alpha, beta, ai_player, human_player)
                    board[i][j] = '_'
                    best = max(best, val)
                    alpha = max(alpha, best)
                    # Alpha Beta Prune
                    if beta <= alpha:
                        return best
        return best
    else:
        best = math.inf
        for i in range(3):
            for j in range(3):
                if board[i][j] == '_':
                    board[i][j] = human_player
                    val = minimax_alpha_beta(board, depth + 1, True, alpha, beta, ai_player, human_player)
                    board[i][j] = '_'
                    best = min(best, val)
                    beta = min(beta, best)
                    # Alpha Beta Prune
                    if beta <= alpha:
                        return best
        return best

def find_best_move(board, ai_player, human_player):
    best_val = -math.inf
    best_moves = []
    for i in range(3):
        for j in range(3):
            if board[i][j] == '_':
                board[i][j] = ai_player
                move_val = minimax_alpha_beta(board, 0, False, -math.inf, math.inf, ai_player, human_player)
                board[i][j] = '_'
                # Collect best moves (helps randomize equal best options)
                if move_val > best_val:
                    best_val = move_val
                    best_moves = [(i, j)]
                elif move_val == best_val:
                    best_moves.append((i, j))
    return random.choice(best_moves) if best_moves else None

# Interactive play loop (human vs AI). Repeats until user quits.
def play_loop():
    print("Tic-Tac-Toe: Minimax with Alpha-Beta pruning")
    human_player = ''
    while human_player not in ('x', 'o'):
        human_player = input("Choose your symbol (x/o): ").strip().lower()
    ai_player = 'o' if human_player == 'x' else 'x'

    while True:
        board = [['_', '_', '_'] for _ in range(3)]
        turn = 'x'  # X always starts
        print("\nNew game! Board positions are (row,col) from 0..2")
        print_board(board)

        while True:
            if turn == human_player:
                # Human move
                try:
                    move = input(f"Your move (row col) or 'q' to quit current game: ").strip()
                    if move.lower() == 'q':
                        print("Quitting current game.")
                        break
                    r, c = map(int, move.split())
                except Exception:
                    print("Invalid input. Enter two integers (row col) like: 1 2")
                    continue
                if not (0 <= r <= 2 and 0 <= c <= 2) or board[r][c] != '_':
                    print("Illegal move. Try again.")
                    continue
                board[r][c] = human_player
            else:
                # AI move
                move = find_best_move(board, ai_player, human_player)
                if move is None:
                    # no moves left
                    pass
                else:
                    r, c = move
                    board[r][c] = ai_player
                    print(f"AI ({ai_player}) plays: {r} {c}")

            print_board(board)
            score = evaluate(board, ai_player, human_player)
            if score == 10:
                print(f"AI ({ai_player}) wins!")
                break
            elif score == -10:
                print(f"You ({human_player}) win! Congratulations!")
                break
            elif not is_moves_left(board):
                print("It's a draw.")
                break

            # swap turn
            turn = ai_player if turn == human_player else human_player

        # after game
        cont = input("Play again? (y/n) ").strip().lower()
        if cont != 'y':
            print("Thanks for playing. Bye!")
            return

if __name__ == "__main__":
    play_loop()
