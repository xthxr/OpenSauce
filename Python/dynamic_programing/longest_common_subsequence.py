"""
Longest Common Subsequence (LCS) Algorithm

Time Complexity: O(m * n) where m and n are lengths of the two sequences
Space Complexity: O(m * n) for the DP table, O(min(m, n)) for space-optimized version

The Longest Common Subsequence problem finds the longest subsequence that appears
in both sequences. A subsequence is derived by deleting some or no elements
without changing the order of remaining elements.
"""

def lcs_length(text1, text2):
    """
    Find the length of the longest common subsequence between two strings.
    
    Args:
        text1: First string
        text2: Second string
    
    Returns:
        int: Length of the longest common subsequence
    
    Example:
        >>> lcs_length("ABCDGH", "AEDFHR")
        3
        >>> lcs_length("AGGTAB", "GXTXAYB")
        4
    """
    m, n = len(text1), len(text2)
    
    # Create DP table
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if text1[i - 1] == text2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    return dp[m][n]

def lcs_sequence(text1, text2):
    """
    Find the actual longest common subsequence between two strings.
    
    Args:
        text1: First string
        text2: Second string
    
    Returns:
        str: The longest common subsequence
    
    Example:
        >>> lcs_sequence("ABCDGH", "AEDFHR")
        "ADH"
        >>> lcs_sequence("AGGTAB", "GXTXAYB")
        "GTAB"
    """
    m, n = len(text1), len(text2)
    
    # Create DP table
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if text1[i - 1] == text2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    # Backtrack to find the actual sequence
    lcs = []
    i, j = m, n
    
    while i > 0 and j > 0:
        if text1[i - 1] == text2[j - 1]:
            lcs.append(text1[i - 1])
            i -= 1
            j -= 1
        elif dp[i - 1][j] > dp[i][j - 1]:
            i -= 1
        else:
            j -= 1
    
    return ''.join(reversed(lcs))

def lcs_length_optimized(text1, text2):
    """
    Space-optimized version of LCS length calculation.
    Uses only O(min(m, n)) space instead of O(m * n).
    
    Args:
        text1: First string
        text2: Second string
    
    Returns:
        int: Length of the longest common subsequence
    
    Example:
        >>> lcs_length_optimized("ABCDGH", "AEDFHR")
        3
    """
    # Make text1 the shorter string to optimize space
    if len(text1) > len(text2):
        text1, text2 = text2, text1
    
    m, n = len(text1), len(text2)
    
    # Use only two rows instead of full table
    prev = [0] * (m + 1)
    curr = [0] * (m + 1)
    
    for j in range(1, n + 1):
        for i in range(1, m + 1):
            if text1[i - 1] == text2[j - 1]:
                curr[i] = prev[i - 1] + 1
            else:
                curr[i] = max(prev[i], curr[i - 1])
        
        # Swap prev and curr
        prev, curr = curr, prev
    
    return prev[m]

def lcs_all_sequences(text1, text2):
    """
    Find all possible longest common subsequences between two strings.
    
    Args:
        text1: First string
        text2: Second string
    
    Returns:
        set: Set of all longest common subsequences
    
    Example:
        >>> lcs_all_sequences("ABC", "AC")
        {"AC"}
    """
    m, n = len(text1), len(text2)
    
    # Create DP table
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if text1[i - 1] == text2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    def backtrack(i, j):
        if i == 0 or j == 0:
            return {""}
        
        if text1[i - 1] == text2[j - 1]:
            result = set()
            for seq in backtrack(i - 1, j - 1):
                result.add(seq + text1[i - 1])
            return result
        else:
            result = set()
            if dp[i - 1][j] >= dp[i][j - 1]:
                result.update(backtrack(i - 1, j))
            if dp[i][j - 1] >= dp[i - 1][j]:
                result.update(backtrack(i, j - 1))
            return result
    
    return backtrack(m, n)

def lcs_with_lists(list1, list2):
    """
    Find the longest common subsequence between two lists.
    
    Args:
        list1: First list
        list2: Second list
    
    Returns:
        list: The longest common subsequence as a list
    
    Example:
        >>> lcs_with_lists([1, 2, 3, 4], [2, 4, 6, 8])
        [2, 4]
    """
    m, n = len(list1), len(list2)
    
    # Create DP table
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if list1[i - 1] == list2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
    
    # Backtrack to find the actual sequence
    lcs = []
    i, j = m, n
    
    while i > 0 and j > 0:
        if list1[i - 1] == list2[j - 1]:
            lcs.append(list1[i - 1])
            i -= 1
            j -= 1
        elif dp[i - 1][j] > dp[i][j - 1]:
            i -= 1
        else:
            j -= 1
    
    return lcs[::-1]

def lcs_similarity(text1, text2):
    """
    Calculate the similarity ratio between two strings based on LCS.
    
    Args:
        text1: First string
        text2: Second string
    
    Returns:
        float: Similarity ratio between 0 and 1
    
    Example:
        >>> lcs_similarity("ABCDGH", "AEDFHR")
        0.5
    """
    lcs_len = lcs_length(text1, text2)
    max_len = max(len(text1), len(text2))
    
    if max_len == 0:
        return 1.0
    
    return lcs_len / max_len

if __name__ == "__main__":
    # Test cases
    test_cases = [
        ("ABCDGH", "AEDFHR"),
        ("AGGTAB", "GXTXAYB"),
        ("ABC", "AC"),
        ("", "ABC"),
        ("ABC", ""),
        ("", ""),
        ("ABCDEF", "ABCDEF"),
        ("ABCDEF", "FEDCBA"),
        ("HELLO", "WORLD"),
        ("INTENTION", "EXECUTION")
    ]
    
    print("Longest Common Subsequence Test Cases")
    print("=" * 50)
    
    for i, (text1, text2) in enumerate(test_cases):
        print(f"\nTest Case {i + 1}:")
        print(f"String 1: '{text1}'")
        print(f"String 2: '{text2}'")
        
        # Test LCS length
        length = lcs_length(text1, text2)
        print(f"LCS Length: {length}")
        
        # Test LCS sequence
        if length > 0:
            sequence = lcs_sequence(text1, text2)
            print(f"LCS Sequence: '{sequence}'")
            
            # Verify length matches
            assert len(sequence) == length, f"Length mismatch: {len(sequence)} != {length}"
        
        # Test space-optimized version
        length_opt = lcs_length_optimized(text1, text2)
        print(f"LCS Length (Optimized): {length_opt}")
        assert length == length_opt, f"Optimized version mismatch: {length} != {length_opt}"
        
        # Test similarity
        similarity = lcs_similarity(text1, text2)
        print(f"Similarity Ratio: {similarity:.2f}")
        
        # Test with lists
        list1 = list(text1)
        list2 = list(text2)
        lcs_list = lcs_with_lists(list1, list2)
        print(f"LCS as List: {lcs_list}")
        
        # Test all sequences for smaller inputs
        if len(text1) <= 10 and len(text2) <= 10:
            all_sequences = lcs_all_sequences(text1, text2)
            print(f"All LCS Sequences: {all_sequences}")
    
    print("\n" + "=" * 50)
    print("Performance Test:")
    
    # Performance test with larger strings
    import time
    
    text1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" * 10
    text2 = "ZYXWVUTSRQPONMLKJIHGFEDCBA" * 10
    
    start_time = time.time()
    length = lcs_length(text1, text2)
    end_time = time.time()
    
    print(f"Large string test:")
    print(f"String 1 length: {len(text1)}")
    print(f"String 2 length: {len(text2)}")
    print(f"LCS Length: {length}")
    print(f"Time taken: {(end_time - start_time) * 1000:.2f} ms")
