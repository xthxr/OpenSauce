"""
edit_distance_topdown.py

Alternative top-down (recursive + memoization) implementation of Edit Distance (Levenshtein Distance).
Includes optional operation tracking for insertions, deletions, and substitutions.
"""

from typing import List, Tuple, Dict

def edit_distance_topdown(s1: str, s2: str) -> int:
    """
    Computes the minimum edit distance between two strings using top-down DP (memoization).

    Args:
        s1 (str): Source string
        s2 (str): Target string

    Returns:
        int: Minimum number of operations (insertions, deletions, substitutions) to convert s1 to s2
    """
    memo: Dict[Tuple[int, int], int] = {}

    def helper(i: int, j: int) -> int:
        # Base cases
        if i == len(s1):
            return len(s2) - j  # insert remaining characters
        if j == len(s2):
            return len(s1) - i  # delete remaining characters

        if (i, j) in memo:
            return memo[(i, j)]

        if s1[i] == s2[j]:
            memo[(i, j)] = helper(i + 1, j + 1)
        else:
            insert_op = 1 + helper(i, j + 1)       # insert
            delete_op = 1 + helper(i + 1, j)       # delete
            replace_op = 1 + helper(i + 1, j + 1)  # substitute
            memo[(i, j)] = min(insert_op, delete_op, replace_op)

        return memo[(i, j)]

    return helper(0, 0)


def edit_distance_operations_topdown(s1: str, s2: str) -> Tuple[int, List[str]]:
    """
    Computes edit distance and returns the sequence of operations to convert s1 to s2.

    Args:
        s1 (str): Source string
        s2 (str): Target string

    Returns:
        Tuple[int, List[str]]: (Minimum number of operations, List of operations)
    """
    memo: Dict[Tuple[int, int], int] = {}
    op_memo: Dict[Tuple[int, int], List[str]] = {}

    def helper(i: int, j: int) -> int:
        if i == len(s1):
            op_memo[(i, j)] = [f"Insert '{s2[k]}' at position {i+k-j}" for k in range(j, len(s2))]
            return len(s2) - j
        if j == len(s2):
            op_memo[(i, j)] = [f"Delete '{s1[k]}' from position {k}" for k in range(i, len(s1))]
            return len(s1) - i

        if (i, j) in memo:
            return memo[(i, j)]

        if s1[i] == s2[j]:
            dist = helper(i + 1, j + 1)
            ops = op_memo[(i + 1, j + 1)]
        else:
            # Insert
            insert_dist = 1 + helper(i, j + 1)
            insert_ops = [f"Insert '{s2[j]}' at position {i}"] + op_memo[(i, j + 1)]
            # Delete
            delete_dist = 1 + helper(i + 1, j)
            delete_ops = [f"Delete '{s1[i]}' from position {i}"] + op_memo[(i + 1, j)]
            # Replace
            replace_dist = 1 + helper(i + 1, j + 1)
            replace_ops = [f"Replace '{s1[i]}' with '{s2[j]}' at position {i}"] + op_memo[(i + 1, j + 1)]

            # Choose minimum
            min_dist = min(insert_dist, delete_dist, replace_dist)
            if min_dist == insert_dist:
                dist, ops = insert_dist, insert_ops
            elif min_dist == delete_dist:
                dist, ops = delete_dist, delete_ops
            else:
                dist, ops = replace_dist, replace_ops

        memo[(i, j)] = dist
        op_memo[(i, j)] = ops
        return dist

    distance = helper(0, 0)
    operations = op_memo[(0, 0)]
    return distance, operations


# =========================
# Sample Test Cases
# =========================
if __name__ == "__main__":
    test_cases = [
        ("kitten", "sitting"),
        ("", "hello"),
        ("same", "same"),
        ("abc", "xyz"),
        ("saturday", "sunday"),
        ("GATTACA", "GCATGCU"),
    ]

    for s1, s2 in test_cases:
        dist = edit_distance_topdown(s1, s2)
        dist_ops, ops_list = edit_distance_operations_topdown(s1, s2)
        print(f"\nEdit distance (top-down) between '{s1}' and '{s2}': {dist}")
        print(f"Edit distance with operations: {dist_ops}")
        for op in ops_list:
            print(f" - {op}")
