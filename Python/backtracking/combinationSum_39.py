def combinationSum(candidates, target):
    """
    Solves the Combination Sum problem using backtracking.

    Problem:
    - Given a list of distinct integers (candidates) and a target sum,
      find all unique combinations of candidates that sum to the target.
    - Each number in candidates can be used **unlimited times** in the combination.
    
    Returns:
        A list of lists, where each inner list is one valid combination.

    Time Complexity: O(N^(T/M))
        N = number of candidates, T = target, M = minimum candidate value
        (exponential because we explore all combination possibilities)
    Space Complexity: O(T/M)
        (recursion depth proportional to target/min(candidates))
    """

    # This will store all valid combinations we find
    result = []

    # Define a helper function for backtracking
    # Arguments:
    # - current: the current combination being constructed
    # - total: remaining sum needed to reach the target
    # - idx: current index in candidates we're considering
    def backtrack(current, total, idx):
        # Base case 1: total is exactly zero -> valid combination
        if total == 0:
            # Add a copy of current combination to the result
            result.append(list(current))
            return

        # Base case 2: total becomes negative -> invalid path
        if total < 0:
            return

        # Iterate over candidates starting from current index
        for i in range(idx, len(candidates)):
            candidate = candidates[i]

            # Choose: add candidate to current combination
            current.append(candidate)

            # Explore: recursively try next step with updated total
            # i, not i+1, because we can reuse the same candidate multiple times
            backtrack(current, total - candidate, i)

            # Backtrack: remove last candidate to try next option
            current.pop()

    # Start the backtracking with empty combination, full target, starting at index 0
    backtrack([], target, 0)

    return result


# -------------------------------
# Test Cases
# -------------------------------
if __name__ == "__main__":
    # Example 1
    print("Example 1: candidates = [2,3,6,7], target = 7")
    solutions = combinationSum([2,3,6,7], 7)
    print("All valid combinations:")
    for combo in solutions:
        print(combo)
    # Expected output: [[2,2,3], [7]]

    print("\nExample 2: candidates = [2,3,5], target = 8")
    solutions = combinationSum([2,3,5], 8)
    for combo in solutions:
        print(combo)
    # Expected output: [[2,2,2,2], [2,3,3], [3,5]]
