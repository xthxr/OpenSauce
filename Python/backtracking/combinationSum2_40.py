def combinationSum2(candidates, target):
    """
    Solves the Combination Sum II problem using backtracking.

    Problem:
    - Given a list of integers (candidates, may contain duplicates) and a target,
      find all unique combinations where each number may be used **at most once**.
    - The solution must not contain duplicate combinations.

    Returns:
        A list of lists, each inner list is one valid combination.

    Time Complexity: O(2^N)
        Because we explore all subsets.
    Space Complexity: O(N)
        Maximum recursion depth proportional to the number of candidates.
    """

    # Sort the candidates first to make it easier to skip duplicates
    candidates.sort()

    # Result list to store all valid combinations
    result = []

    # Helper function for backtracking
    # Arguments:
    # - start: index in candidates to start exploring
    # - path: the current combination being constructed
    # - total: sum of elements in the current path
    def backtrack(start, path, total):
        # Base case 1: total matches target -> valid combination
        if total == target:
            # Append a copy of current path
            result.append(path[:])
            return

        # Base case 2: total exceeds target -> invalid path
        if total > target:
            return

        # Iterate through candidates starting at index start
        for i in range(start, len(candidates)):
            candidate = candidates[i]

            # Skip duplicates at the same recursion level
            # i > start ensures only the first occurrence is used
            if i > start and candidate == candidates[i - 1]:
                continue

            # Choose: add candidate to current combination
            path.append(candidate)

            # Explore: move to next index because each candidate can only be used once
            backtrack(i + 1, path, total + candidate)

            # Backtrack: remove last candidate to try next possibility
            path.pop()

    # Start backtracking from index 0, empty path, total 0
    backtrack(0, [], 0)

    return result


# -------------------------------
# Test Cases
# -------------------------------
if __name__ == "__main__":
    print("Example 1: candidates = [10,1,2,7,6,1,5], target = 8")
    solutions = combinationSum2([10,1,2,7,6,1,5], 8)
    print("All valid combinations:")
    for combo in solutions:
        print(combo)
    # Expected: [[1,1,6],[1,2,5],[1,7],[2,6]]

    print("\nExample 2: candidates = [2,5,2,1,2], target = 5")
    solutions = combinationSum2([2,5,2,1,2], 5)
    for combo in solutions:
        print(combo)
    # Expected: [[1,2,2],[5]]
