def climbStairs(n):
    """
    Returns the number of distinct ways to climb n stairs
    when you can take 1 or 2 steps at a time.

    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    # Base case: if there are 0 or 1 stairs, there is only 1 way to climb
    if n == 0 or n == 1:
        return 1
    
    # Initialize DP table
    # dp[i] will store the number of ways to reach the i-th step
    dp = [0] * (n + 1)
    
    # There is 1 way to stay at the bottom (0-th step)
    dp[0] = 1
    # There is 1 way to reach the first step
    dp[1] = 1
    
    # Fill the DP table from step 2 to n
    for i in range(2, n + 1):
        # You can reach step i either from step i-1 (1 step) or step i-2 (2 steps)
        dp[i] = dp[i - 1] + dp[i - 2]
    
    # The result is the number of ways to reach the n-th step
    return dp[n]


# -------------------------------
# Test Cases
# -------------------------------
if __name__ == "__main__":
    print(climbStairs(3))  # Expected: 3
    print(climbStairs(5))  # Expected: 8
    print(climbStairs(1))  # Expected: 1
    print(climbStairs(0))  # Expected: 1
