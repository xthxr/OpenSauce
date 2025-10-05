def lcs_memoization(text1, text2):
    """
    Longest Common Subsequence - Top-Down (Memoization) approach
    Time Complexity: O(n * m)
    Space Complexity: O(n * m) for memoization table + O(n + m) recursion stack
    """
    n = len(text1)
    m = len(text2)
    dp = {}
    return solve_memo(text1, text2, n, m, dp)


def solve_memo(text1, text2, n, m, dp):
    """
    Recursive helper with memoization for LCS
    """
    # Check if result is already computed
    if (n, m) in dp:
        return dp[(n, m)]
    
    # Base case: if either string is empty
    if n == 0 or m == 0:
        return 0
    
    # If last characters match, include it in LCS
    if text1[n - 1] == text2[m - 1]:
        dp[(n, m)] = 1 + solve_memo(text1, text2, n - 1, m - 1, dp)
        return dp[(n, m)]
    
    # If last characters don't match, try both possibilities
    dp[(n, m)] = max(
        solve_memo(text1, text2, n - 1, m, dp),  # Exclude last char of text1
        solve_memo(text1, text2, n, m - 1, dp)   # Exclude last char of text2
    )
    
    return dp[(n, m)]


def lcs_tabulation(text1, text2):
    """
    Longest Common Subsequence - Bottom-Up (Tabulation) approach
    Time Complexity: O(n * m)
    Space Complexity: O(n * m) for DP table
    More efficient than memoization (no recursion overhead)
    """
    n = len(text1)
    m = len(text2)
    
    # Create DP table with (n+1) x (m+1) dimensions
    dp = [[0] * (m + 1) for _ in range(n + 1)]
    
    # Fill the DP table
    for i in range(1, n + 1):
        for j in range(1, m + 1):
            if text1[i - 1] == text2[j - 1]:
                # Characters match: add 1 to previous diagonal
                dp[i][j] = 1 + dp[i - 1][j - 1]
            else:
                # Characters don't match: take max of top or left
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    return dp[n][m]


def lcs_space_optimized(text1, text2):
    """
    Space-optimized LCS using only two rows
    Time Complexity: O(n * m)
    Space Complexity: O(min(n, m))
    """
    # Ensure text2 is the shorter string for space optimization
    if len(text1) < len(text2):
        text1, text2 = text2, text1
    
    n = len(text1)
    m = len(text2)
    
    # Only keep track of previous and current row
    prev = [0] * (m + 1)
    curr = [0] * (m + 1)
    
    for i in range(1, n + 1):
        for j in range(1, m + 1):
            if text1[i - 1] == text2[j - 1]:
                curr[j] = 1 + prev[j - 1]
            else:
                curr[j] = max(prev[j], curr[j - 1])
        
        # Swap rows for next iteration
        prev, curr = curr, prev
    
    return prev[m]


def lcs_with_string(text1, text2):
    """
    Returns both the length and the actual LCS string
    """
    n = len(text1)
    m = len(text2)
    
    # Create DP table
    dp = [[0] * (m + 1) for _ in range(n + 1)]
    
    # Fill the DP table
    for i in range(1, n + 1):
        for j in range(1, m + 1):
            if text1[i - 1] == text2[j - 1]:
                dp[i][j] = 1 + dp[i - 1][j - 1]
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    # Backtrack to find the actual LCS string
    lcs_str = []
    i, j = n, m
    
    while i > 0 and j > 0:
        if text1[i - 1] == text2[j - 1]:
            lcs_str.append(text1[i - 1])
            i -= 1
            j -= 1
        elif dp[i - 1][j] > dp[i][j - 1]:
            i -= 1
        else:
            j -= 1
    
    # Reverse since we built it backwards
    lcs_str.reverse()
    
    return dp[n][m], ''.join(lcs_str)


# Test the implementations
if __name__ == "__main__":
    # Test cases
    test_cases = [
        ("abcde", "ace"),           # Expected: 3 ("ace")
        ("abc", "abc"),             # Expected: 3 ("abc")
        ("abc", "def"),             # Expected: 0 ("")
        ("aggtab", "gxtxayb"),      # Expected: 4 ("gtab")
        ("ABCDGH", "AEDFHR"),       # Expected: 3 ("ADH")
        ("", "abc"),                # Expected: 0 ("")
        ("programming", "gaming"),  # Expected: 6 ("gaming")
    ]
    
    print("=" * 70)
    print("LONGEST COMMON SUBSEQUENCE (LCS) - All Approaches")
    print("=" * 70)
    
    for text1, text2 in test_cases:
        print(f"\nText1: '{text1}'")
        print(f"Text2: '{text2}'")
        
        # Test all approaches
        result_memo = lcs_memoization(text1, text2)
        result_tab = lcs_tabulation(text1, text2)
        result_opt = lcs_space_optimized(text1, text2)
        length, lcs_str = lcs_with_string(text1, text2)
        
        print(f"LCS Length (Memoization):     {result_memo}")
        print(f"LCS Length (Tabulation):      {result_tab}")
        print(f"LCS Length (Space Optimized): {result_opt}")
        print(f"Actual LCS String:            '{lcs_str}'")
    
    # Performance comparison
    import time
    
    print("\n" + "=" * 70)
    print("PERFORMANCE COMPARISON")
    print("=" * 70)
    
    # Large test strings
    text1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" * 20
    text2 = "ACEGIKMOQSUWYABCDEFGHIJKLM" * 20
    
    # Test memoization
    start = time.time()
    result_memo = lcs_memoization(text1, text2)
    time_memo = (time.time() - start) * 1000
    
    # Test tabulation
    start = time.time()
    result_tab = lcs_tabulation(text1, text2)
    time_tab = (time.time() - start) * 1000
    
    # Test space optimized
    start = time.time()
    result_opt = lcs_space_optimized(text1, text2)
    time_opt = (time.time() - start) * 1000
    
    print(f"\nString lengths: {len(text1)} x {len(text2)}")
    print(f"LCS Length: {result_memo}")
    print(f"\nMemoization (Top-Down):     {time_memo:.2f}ms")
    print(f"Tabulation (Bottom-Up):     {time_tab:.2f}ms")
    print(f"Space Optimized:            {time_opt:.2f}ms")