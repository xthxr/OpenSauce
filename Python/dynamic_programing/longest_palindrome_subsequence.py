def lps_memoization(s):
    """
    Longest Palindromic Subsequence - Top-Down (Memoization) approach
    Find the longest subsequence that reads same forwards and backwards
    Time Complexity: O(n²)
    Space Complexity: O(n²) for memoization + O(n) recursion stack
    """
    if not s:
        return 0
    
    n = len(s)
    dp = {}
    return solve_memo(s, 0, n - 1, dp)


def solve_memo(s, left, right, dp):
    """
    Recursive helper with memoization for LPS
    """
    # Check if result is already computed
    if (left, right) in dp:
        return dp[(left, right)]
    
    # Base case: pointers crossed, no characters left
    if left > right:
        return 0
    
    # Base case: single character is a palindrome of length 1
    if left == right:
        return 1
    
    # If characters match, include both in palindrome
    if s[left] == s[right]:
        dp[(left, right)] = 2 + solve_memo(s, left + 1, right - 1, dp)
        return dp[(left, right)]
    
    # If characters don't match, try both possibilities
    dp[(left, right)] = max(
        solve_memo(s, left + 1, right, dp),  # Exclude left character
        solve_memo(s, left, right - 1, dp)   # Exclude right character
    )
    
    return dp[(left, right)]


def lps_tabulation(s):
    """
    Longest Palindromic Subsequence - Bottom-Up (Tabulation) approach
    Time Complexity: O(n²)
    Space Complexity: O(n²) for DP table
    More efficient than memoization (no recursion overhead)
    """
    n = len(s)
    
    if n == 0:
        return 0
    
    # dp[i][j] = LPS length in substring s[i:j+1]
    dp = [[0] * n for _ in range(n)]
    
    # Every single character is a palindrome of length 1
    for i in range(n):
        dp[i][i] = 1
    
    # Fill table for substrings of increasing length
    for length in range(2, n + 1):
        for i in range(n - length + 1):
            j = i + length - 1
            
            if s[i] == s[j]:
                # Characters match
                dp[i][j] = 2 + dp[i + 1][j - 1]
            else:
                # Characters don't match
                dp[i][j] = max(dp[i + 1][j], dp[i][j - 1])
    
    return dp[0][n - 1]


def lps_space_optimized(s):
    """
    Space-optimized LPS using only two arrays
    Time Complexity: O(n²)
    Space Complexity: O(n)
    """
    n = len(s)
    
    if n == 0:
        return 0
    
    # Only need current and previous row
    prev = [0] * n
    curr = [0] * n
    
    # Every single character is a palindrome
    for i in range(n):
        prev[i] = 1
    
    # Fill for substrings of increasing length
    for length in range(2, n + 1):
        for i in range(n - length + 1):
            j = i + length - 1
            
            if s[i] == s[j]:
                if length == 2:
                    curr[i] = 2
                else:
                    curr[i] = 2 + prev[i + 1]
            else:
                curr[i] = max(prev[i + 1] if i + 1 < n else 0, 
                             curr[i] if j > 0 else 0)
        
        prev, curr = curr, prev
    
    return prev[0]


def lps_with_string(s):
    """
    Returns both the length and the actual LPS string
    """
    n = len(s)
    
    if n == 0:
        return 0, ""
    
    # dp[i][j] = LPS length in substring s[i:j+1]
    dp = [[0] * n for _ in range(n)]
    
    # Every single character is a palindrome
    for i in range(n):
        dp[i][i] = 1
    
    # Fill table
    for length in range(2, n + 1):
        for i in range(n - length + 1):
            j = i + length - 1
            
            if s[i] == s[j]:
                dp[i][j] = 2 + dp[i + 1][j - 1]
            else:
                dp[i][j] = max(dp[i + 1][j], dp[i][j - 1])
    
    # Backtrack to find the actual LPS string
    lps_str = []
    left, right = 0, n - 1
    
    while left <= right:
        if left == right:
            # Single character
            lps_str.append(s[left])
            break
        
        if s[left] == s[right]:
            # Characters match
            lps_str.append(s[left])
            left += 1
            right -= 1
        elif dp[left + 1][right] > dp[left][right - 1]:
            # Move left pointer
            left += 1
        else:
            # Move right pointer
            right -= 1
    
    # Build palindrome (first half + reversed second half)
    first_half = ''.join(lps_str)
    
    # Check if we need middle character
    if len(first_half) * 2 < dp[0][n - 1]:
        # Odd length palindrome, we have middle char
        result = first_half + first_half[-2::-1]
    else:
        # Even length or includes middle
        result = first_half + first_half[::-1]
    
    return dp[0][n - 1], result


def lps_via_lcs(s):
    """
    Alternative approach: LPS(s) = LCS(s, reverse(s))
    Time Complexity: O(n²)
    Space Complexity: O(n²)
    """
    n = len(s)
    
    if n == 0:
        return 0
    
    # Reverse the string
    s_rev = s[::-1]
    
    # Find LCS of s and its reverse
    dp = [[0] * (n + 1) for _ in range(n + 1)]
    
    for i in range(1, n + 1):
        for j in range(1, n + 1):
            if s[i - 1] == s_rev[j - 1]:
                dp[i][j] = 1 + dp[i - 1][j - 1]
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    return dp[n][n]


def min_insertions_for_palindrome(s):
    """
    Find minimum insertions needed to make string a palindrome
    Answer = n - LPS(s)
    """
    n = len(s)
    lps_length = lps_tabulation(s)
    return n - lps_length


def min_deletions_for_palindrome(s):
    """
    Find minimum deletions needed to make string a palindrome
    Answer = n - LPS(s)
    """
    n = len(s)
    lps_length = lps_tabulation(s)
    return n - lps_length


# Test the implementations
if __name__ == "__main__":
    # Test cases
    test_cases = [
        "bbbab",           # Expected: 4 ("bbbb")
        "cbbd",            # Expected: 2 ("bb")
        "racecar",         # Expected: 7 ("racecar")
        "abcdcba",         # Expected: 7 ("abcdcba")
        "aabaa",           # Expected: 5 ("aabaa")
        "agbdba",          # Expected: 5 ("abdba" or "adbda")
        "a",               # Expected: 1 ("a")
        "",                # Expected: 0 ("")
        "aaaa",            # Expected: 4 ("aaaa")
        "abcdef",          # Expected: 1 (any single char)
    ]
    
    print("=" * 70)
    print("LONGEST PALINDROMIC SUBSEQUENCE (LPS) - All Approaches")
    print("=" * 70)
    
    for s in test_cases:
        print(f"\nString: '{s}'")
        
        # Test all approaches
        result_memo = lps_memoization(s)
        result_tab = lps_tabulation(s)
        result_lcs = lps_via_lcs(s)
        
        if s:
            length, lps_str = lps_with_string(s)
            min_ins = min_insertions_for_palindrome(s)
            min_del = min_deletions_for_palindrome(s)
            
            print(f"LPS Length (Memoization): {result_memo}")
            print(f"LPS Length (Tabulation):  {result_tab}")
            print(f"LPS Length (via LCS):     {result_lcs}")
            print(f"LPS String:               '{lps_str}'")
            print(f"Min insertions needed:    {min_ins}")
            print(f"Min deletions needed:     {min_del}")
        else:
            print(f"LPS Length: {result_memo}")
    
    # Performance comparison
    import time
    import random
    
    print("\n" + "=" * 70)
    print("PERFORMANCE COMPARISON")
    print("=" * 70)
    
    # Large test string
    chars = 'abcdefghij'
    large_string = ''.join(random.choice(chars) for _ in range(1000))
    
    # Test memoization
    start = time.time()
    result_memo = lps_memoization(large_string)
    time_memo = (time.time() - start) * 1000
    
    # Test tabulation
    start = time.time()
    result_tab = lps_tabulation(large_string)
    time_tab = (time.time() - start) * 1000
    
    # Test LCS approach
    start = time.time()
    result_lcs = lps_via_lcs(large_string)
    time_lcs = (time.time() - start) * 1000
    
    print(f"\nString length: {len(large_string)}")
    print(f"LPS Length: {result_memo}")
    print(f"\nMemoization (Top-Down):     {time_memo:.2f}ms")
    print(f"Tabulation (Bottom-Up):     {time_tab:.2f}ms")
    print(f"LCS Approach:               {time_lcs:.2f}ms")