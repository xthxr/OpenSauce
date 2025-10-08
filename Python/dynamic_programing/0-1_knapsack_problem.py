def knapsack_memoization(weights, values, W):
    """
    0/1 Knapsack - Top-Down (Memoization) approach
    Maximize value while keeping total weight <= W
    Time Complexity: O(n * W)
    Space Complexity: O(n * W) for memoization + O(n) recursion stack
    """
    if not weights or not values or W <= 0:
        return 0
    
    dp = {}
    return solve_memo(0, W, weights, values, dp)


def solve_memo(index, remaining_cap, weights, values, dp):
    """
    Recursive helper with memoization for Knapsack
    """
    # Base case: no more items or no capacity left
    if index >= len(weights) or remaining_cap == 0:
        return 0
    
    # Check if result is already computed
    if (index, remaining_cap) in dp:
        return dp[(index, remaining_cap)]
    
    # Option 1: Don't take current item
    not_take = solve_memo(index + 1, remaining_cap, weights, values, dp)
    
    # Option 2: Take current item (if it fits)
    take = 0
    if weights[index] <= remaining_cap:
        take = values[index] + solve_memo(index + 1, remaining_cap - weights[index], weights, values, dp)
    
    # Store and return maximum value
    dp[(index, remaining_cap)] = max(take, not_take)
    return dp[(index, remaining_cap)]


def knapsack_tabulation(weights, values, W):
    """
    0/1 Knapsack - Bottom-Up (Tabulation) approach
    Time Complexity: O(n * W)
    Space Complexity: O(n * W) for DP table
    More efficient than memoization (no recursion overhead)
    """
    n = len(weights)
    
    if n == 0 or W <= 0:
        return 0
    
    # dp[i][w] = max value using first i items with capacity w
    dp = [[0] * (W + 1) for _ in range(n + 1)]
    
    # Fill the DP table
    for i in range(1, n + 1):
        for w in range(W + 1):
            # Don't take current item
            dp[i][w] = dp[i - 1][w]
            
            # Take current item (if it fits)
            if weights[i - 1] <= w:
                dp[i][w] = max(dp[i][w], values[i - 1] + dp[i - 1][w - weights[i - 1]])
    
    return dp[n][W]


def knapsack_space_optimized(weights, values, W):
    """
    Space-optimized Knapsack using only one array
    Time Complexity: O(n * W)
    Space Complexity: O(W)
    Most memory efficient approach
    """
    n = len(weights)
    
    if n == 0 or W <= 0:
        return 0
    
    # Only need to track current capacity states
    dp = [0] * (W + 1)
    
    for i in range(n):
        # Traverse from right to left to avoid using updated values
        for w in range(W, weights[i] - 1, -1):
            dp[w] = max(dp[w], values[i] + dp[w - weights[i]])
    
    return dp[W]


def knapsack_with_items(weights, values, W):
    """
    Returns both maximum value and the items selected
    """
    n = len(weights)
    
    if n == 0 or W <= 0:
        return 0, []
    
    # dp[i][w] = max value using first i items with capacity w
    dp = [[0] * (W + 1) for _ in range(n + 1)]
    
    # Fill the DP table
    for i in range(1, n + 1):
        for w in range(W + 1):
            dp[i][w] = dp[i - 1][w]
            
            if weights[i - 1] <= w:
                dp[i][w] = max(dp[i][w], values[i - 1] + dp[i - 1][w - weights[i - 1]])
    
    # Backtrack to find selected items
    selected_items = []
    w = W
    
    for i in range(n, 0, -1):
        # If value changed, item was taken
        if dp[i][w] != dp[i - 1][w]:
            selected_items.append(i - 1)  # Item index (0-based)
            w -= weights[i - 1]
    
    selected_items.reverse()
    
    return dp[n][W], selected_items


def fractional_knapsack(weights, values, W):
    """
    Fractional Knapsack - can take fractions of items
    Greedy approach (sort by value/weight ratio)
    Time Complexity: O(n log n)
    Space Complexity: O(n)
    """
    n = len(weights)
    
    if n == 0 or W <= 0:
        return 0.0
    
    # Calculate value per unit weight
    items = []
    for i in range(n):
        ratio = values[i] / weights[i]
        items.append((ratio, weights[i], values[i], i))
    
    # Sort by ratio in descending order
    items.sort(reverse=True)
    
    total_value = 0.0
    remaining_cap = W
    
    for ratio, weight, value, idx in items:
        if remaining_cap == 0:
            break
        
        if weight <= remaining_cap:
            # Take whole item
            total_value += value
            remaining_cap -= weight
        else:
            # Take fraction of item
            fraction = remaining_cap / weight
            total_value += value * fraction
            remaining_cap = 0
    
    return total_value


def unbounded_knapsack(weights, values, W):
    """
    Unbounded Knapsack - can take unlimited copies of each item
    Time Complexity: O(n * W)
    Space Complexity: O(W)
    """
    n = len(weights)
    
    if n == 0 or W <= 0:
        return 0
    
    # dp[w] = max value with capacity w
    dp = [0] * (W + 1)
    
    for w in range(1, W + 1):
        for i in range(n):
            if weights[i] <= w:
                dp[w] = max(dp[w], values[i] + dp[w - weights[i]])
    
    return dp[W]


# Test the implementations
if __name__ == "__main__":
    # Test cases
    test_cases = [
        {
            'weights': [1, 3, 4, 5],
            'values': [1, 4, 5, 7],
            'W': 7,
            'expected': 9  # Items: index 1 (w=3, v=4) + index 3 (w=5, v=7) = w=8 WRONG
                           # Correct: index 1 (w=3, v=4) + index 2 (w=4, v=5) = w=7, v=9
        },
        {
            'weights': [2, 1, 3, 2],
            'values': [12, 10, 20, 15],
            'W': 5,
            'expected': 37  # Items: index 1 (w=1, v=10) + index 2 (w=3, v=20) + wait that's 4
                            # index 0 (w=2, v=12) + index 2 (w=3, v=20) = w=5, v=32
                            # index 1 (w=1, v=10) + index 3 (w=2, v=15) + index 3 (w=2, v=15) = unbounded
                            # 0/1: index 1 (w=1, v=10) + index 2 (w=3, v=20) + index 0 (w=2, v=12) nope 6
                            # Best: index 1,1,2,3 wait 0/1
        },
        {
            'weights': [10, 20, 30],
            'values': [60, 100, 120],
            'W': 50,
            'expected': 220  # Items: index 1 (w=20, v=100) + index 2 (w=30, v=120)
        },
        {
            'weights': [5, 10, 15, 20],
            'values': [10, 20, 30, 40],
            'W': 30,
            'expected': 60  # Items: index 1 (w=10, v=20) + index 3 (w=20, v=40)
        },
        {
            'weights': [1, 2, 3],
            'values': [6, 10, 12],
            'W': 5,
            'expected': 22  # Items: index 1 (w=2, v=10) + index 2 (w=3, v=12)
        },
    ]
    
    print("=" * 70)
    print("0/1 KNAPSACK PROBLEM - All Approaches")
    print("=" * 70)
    
    for i, test in enumerate(test_cases, 1):
        weights = test['weights']
        values = test['values']
        W = test['W']
        
        print(f"\nTest Case {i}:")
        print(f"Weights:  {weights}")
        print(f"Values:   {values}")
        print(f"Capacity: {W}")
        
        # Test all approaches
        result_memo = knapsack_memoization(weights, values, W)
        result_tab = knapsack_tabulation(weights, values, W)
        result_opt = knapsack_space_optimized(weights, values, W)
        max_value, selected = knapsack_with_items(weights, values, W)
        
        print(f"\nMax Value (Memoization):     {result_memo}")
        print(f"Max Value (Tabulation):      {result_tab}")
        print(f"Max Value (Space Optimized): {result_opt}")
        print(f"Selected items (indices):    {selected}")
        if selected:
            print(f"Selected weights:            {[weights[i] for i in selected]}")
            print(f"Selected values:             {[values[i] for i in selected]}")
            print(f"Total weight used:           {sum(weights[i] for i in selected)}/{W}")
    
    # Test fractional and unbounded variants
    print("\n" + "=" * 70)
    print("KNAPSACK VARIANTS")
    print("=" * 70)
    
    weights = [10, 20, 30]
    values = [60, 100, 120]
    W = 50
    
    print(f"\nWeights:  {weights}")
    print(f"Values:   {values}")
    print(f"Capacity: {W}")
    
    result_01 = knapsack_tabulation(weights, values, W)
    result_frac = fractional_knapsack(weights, values, W)
    result_unb = unbounded_knapsack(weights, values, W)
    
    print(f"\n0/1 Knapsack:         {result_01}")
    print(f"Fractional Knapsack:  {result_frac:.2f}")
    print(f"Unbounded Knapsack:   {result_unb}")
    
    # Performance comparison
    import time
    import random
    
    print("\n" + "=" * 70)
    print("PERFORMANCE COMPARISON")
    print("=" * 70)
    
    # Large test case
    n = 1000
    W_large = 5000
    weights_large = [random.randint(1, 50) for _ in range(n)]
    values_large = [random.randint(1, 100) for _ in range(n)]
    
    # Test memoization
    start = time.time()
    result_memo = knapsack_memoization(weights_large, values_large, W_large)
    time_memo = (time.time() - start) * 1000
    
    # Test tabulation
    start = time.time()
    result_tab = knapsack_tabulation(weights_large, values_large, W_large)
    time_tab = (time.time() - start) * 1000
    
    # Test space optimized
    start = time.time()
    result_opt = knapsack_space_optimized(weights_large, values_large, W_large)
    time_opt = (time.time() - start) * 1000
    
    print(f"\nItems: {n}, Capacity: {W_large}")
    print(f"Max Value: {result_memo}")
    print(f"\nMemoization (Top-Down):     {time_memo:.2f}ms")
    print(f"Tabulation (Bottom-Up):     {time_tab:.2f}ms")
    print(f"Space Optimized (O(W)):     {time_opt:.2f}ms")