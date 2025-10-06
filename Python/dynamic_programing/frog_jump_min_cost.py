import sys
sys.setrecursionlimit(10**6)


def frog_jump_memoization(heights):
    """
    Frog Jump - Top-Down (Memoization) approach
    Frog can jump 1 or 2 steps, minimize total energy cost
    Time Complexity: O(n)
    Space Complexity: O(n) for memoization + O(n) recursion stack
    """
    if not heights or len(heights) == 1:
        return 0
    
    dp = {}
    return solve_memo(0, heights, dp)


def solve_memo(index, heights, dp):
    """
    Recursive helper with memoization for Frog Jump
    """
    # Base case: reached or passed the last stone
    if index >= len(heights) - 1:
        return 0
    
    # Check if result is already computed
    if index in dp:
        return dp[index]
    
    # Option 1: Jump one step
    one_jump = abs(heights[index + 1] - heights[index]) + solve_memo(index + 1, heights, dp)
    
    # Option 2: Jump two steps (if possible)
    two_jump = float('inf')
    if index + 2 < len(heights):
        two_jump = abs(heights[index + 2] - heights[index]) + solve_memo(index + 2, heights, dp)
    
    # Store and return minimum cost
    dp[index] = min(one_jump, two_jump)
    return dp[index]


def frog_jump_tabulation(heights):
    """
    Frog Jump - Bottom-Up (Tabulation) approach
    Time Complexity: O(n)
    Space Complexity: O(n) for DP table
    More efficient than memoization (no recursion overhead)
    """
    n = len(heights)
    
    if n == 1:
        return 0
    
    # dp[i] represents minimum cost to reach stone i
    dp = [float('inf')] * n
    dp[0] = 0  # Starting position has 0 cost
    
    for i in range(n):
        # Jump one step forward
        if i + 1 < n:
            dp[i + 1] = min(dp[i + 1], dp[i] + abs(heights[i + 1] - heights[i]))
        
        # Jump two steps forward
        if i + 2 < n:
            dp[i + 2] = min(dp[i + 2], dp[i] + abs(heights[i + 2] - heights[i]))
    
    return dp[n - 1]


def frog_jump_space_optimized(heights):
    """
    Space-optimized Frog Jump using only two variables
    Time Complexity: O(n)
    Space Complexity: O(1)
    Most efficient approach
    """
    n = len(heights)
    
    if n == 1:
        return 0
    
    # Only need to track last two positions
    prev2 = 0  # dp[i-2]
    prev1 = 0  # dp[i-1]
    
    for i in range(1, n):
        # Jump from i-1 to i
        one_jump = prev1 + abs(heights[i] - heights[i - 1])
        
        # Jump from i-2 to i (if possible)
        two_jump = float('inf')
        if i > 1:
            two_jump = prev2 + abs(heights[i] - heights[i - 2])
        
        curr = min(one_jump, two_jump)
        
        # Update for next iteration
        prev2 = prev1
        prev1 = curr
    
    return prev1


def frog_jump_with_path(heights):
    """
    Returns both minimum cost and the actual path taken
    """
    n = len(heights)
    
    if n == 1:
        return 0, [0]
    
    # dp[i] represents minimum cost to reach stone i
    dp = [float('inf')] * n
    dp[0] = 0
    
    # Track the path
    parent = [-1] * n
    
    for i in range(n):
        # Jump one step forward
        if i + 1 < n:
            cost = dp[i] + abs(heights[i + 1] - heights[i])
            if cost < dp[i + 1]:
                dp[i + 1] = cost
                parent[i + 1] = i
        
        # Jump two steps forward
        if i + 2 < n:
            cost = dp[i] + abs(heights[i + 2] - heights[i])
            if cost < dp[i + 2]:
                dp[i + 2] = cost
                parent[i + 2] = i
    
    # Reconstruct path
    path = []
    curr = n - 1
    while curr != -1:
        path.append(curr)
        curr = parent[curr]
    
    path.reverse()
    
    return dp[n - 1], path


def frog_jump_k_steps(heights, k):
    """
    Extended version: Frog can jump up to k steps
    Time Complexity: O(n * k)
    Space Complexity: O(n)
    """
    n = len(heights)
    
    if n == 1:
        return 0
    
    dp = [float('inf')] * n
    dp[0] = 0
    
    for i in range(n):
        # Try all possible jumps from 1 to k steps
        for j in range(1, k + 1):
            if i + j < n:
                dp[i + j] = min(dp[i + j], dp[i] + abs(heights[i + j] - heights[i]))
    
    return dp[n - 1]


# Test the implementations
if __name__ == "__main__":
    # Test cases
    test_cases = [
        [10, 20, 30, 10],           # Expected: 20
        [10, 50, 10],               # Expected: 0
        [7, 4, 4, 2, 6, 6, 3, 4],   # Expected: 7
        [30, 10, 60, 10, 60, 50],   # Expected: 40
        [10],                       # Expected: 0
        [10, 20],                   # Expected: 10
    ]
    
    print("=" * 70)
    print("FROG JUMP (MINIMUM COST) - All Approaches")
    print("=" * 70)
    
    for heights in test_cases:
        print(f"\nHeights: {heights}")
        
        # Test all approaches
        result_memo = frog_jump_memoization(heights)
        result_tab = frog_jump_tabulation(heights)
        result_opt = frog_jump_space_optimized(heights)
        cost, path = frog_jump_with_path(heights)
        
        print(f"Min Cost (Memoization):     {result_memo}")
        print(f"Min Cost (Tabulation):      {result_tab}")
        print(f"Min Cost (Space Optimized): {result_opt}")
        print(f"Path taken (indices):       {path}")
        print(f"Path taken (heights):       {[heights[i] for i in path]}")
    
    # Test k-steps variant
    print("\n" + "=" * 70)
    print("FROG JUMP WITH K STEPS")
    print("=" * 70)
    
    heights = [10, 20, 30, 10, 60, 50]
    for k in range(2, 5):
        result = frog_jump_k_steps(heights, k)
        print(f"Heights: {heights}, k={k} steps -> Min Cost: {result}")
    
    # Performance comparison
    import time
    
    print("\n" + "=" * 70)
    print("PERFORMANCE COMPARISON")
    print("=" * 70)
    
    # Large test case
    import random
    large_heights = [random.randint(1, 100) for _ in range(10000)]
    
    # Test memoization
    start = time.time()
    result_memo = frog_jump_memoization(large_heights)
    time_memo = (time.time() - start) * 1000
    
    # Test tabulation
    start = time.time()
    result_tab = frog_jump_tabulation(large_heights)
    time_tab = (time.time() - start) * 1000
    
    # Test space optimized
    start = time.time()
    result_opt = frog_jump_space_optimized(large_heights)
    time_opt = (time.time() - start) * 1000
    
    print(f"\nArray size: {len(large_heights)} stones")
    print(f"Min Cost: {result_memo}")
    print(f"\nMemoization (Top-Down):     {time_memo:.2f}ms")
    print(f"Tabulation (Bottom-Up):     {time_tab:.2f}ms")
    print(f"Space Optimized (O(1)):     {time_opt:.2f}ms")