"""
Edit Distance (Levenshtein Distance) Algorithm

Time Complexity: O(m × n) where m and n are the lengths of the two strings
Space Complexity: O(m × n) for the DP table

This algorithm calculates the minimum number of operations (insertion, deletion, 
substitution) required to transform one string into another. It uses a bottom-up 
dynamic programming approach.

Applications:
- Spell checkers
- DNA sequencing
- Natural language processing (NLP)
- Plagiarism detection
"""


def edit_distance(word1: str, word2: str) -> int:
    """
    Calculates the minimum edit distance between two strings using dynamic programming.
    
    Args:
        word1: The source string to transform
        word2: The target string to transform into
    
    Returns:
        The minimum number of operations needed to transform word1 into word2
    
    Operations allowed:
        1. Insert a character
        2. Delete a character
        3. Replace a character
    """
    m, n = len(word1), len(word2)
    
    # Create a DP table with dimensions (m+1) x (n+1)
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Base case: converting empty string to word2 requires j insertions
    for j in range(n + 1):
        dp[0][j] = j
    
    # Base case: converting word1 to empty string requires i deletions
    for i in range(m + 1):
        dp[i][0] = i
    
    # Fill the DP table using the recurrence relation
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if word1[i - 1] == word2[j - 1]:
                # Characters match, no operation needed
                dp[i][j] = dp[i - 1][j - 1]
            else:
                # Take minimum of three operations and add 1
                dp[i][j] = 1 + min(
                    dp[i - 1][j],      # Deletion from word1
                    dp[i][j - 1],      # Insertion into word1
                    dp[i - 1][j - 1]   # Substitution
                )
    
    # The answer is in the bottom-right cell
    return dp[m][n]


def edit_distance_with_operations(word1: str, word2: str) -> tuple[int, list[str]]:
    """
    Calculates edit distance and returns the sequence of operations.
    
    Args:
        word1: The source string to transform
        word2: The target string to transform into
    
    Returns:
        A tuple containing:
        - The minimum number of operations
        - A list of operations performed
    """
    m, n = len(word1), len(word2)
    dp = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Initialize base cases
    for j in range(n + 1):
        dp[0][j] = j
    for i in range(m + 1):
        dp[i][0] = i
    
    # Fill DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if word1[i - 1] == word2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1]
            else:
                dp[i][j] = 1 + min(dp[i - 1][j], dp[i][j - 1], dp[i - 1][j - 1])
    
    # Backtrack to find operations
    operations = []
    i, j = m, n
    
    while i > 0 or j > 0:
        if i == 0:
            operations.append(f"Insert '{word2[j - 1]}' at position {i}")
            j -= 1
        elif j == 0:
            operations.append(f"Delete '{word1[i - 1]}' at position {i - 1}")
            i -= 1
        elif word1[i - 1] == word2[j - 1]:
            i -= 1
            j -= 1
        else:
            deletion = dp[i - 1][j]
            insertion = dp[i][j - 1]
            substitution = dp[i - 1][j - 1]
            
            if substitution <= deletion and substitution <= insertion:
                operations.append(f"Replace '{word1[i - 1]}' with '{word2[j - 1]}' at position {i - 1}")
                i -= 1
                j -= 1
            elif deletion <= insertion:
                operations.append(f"Delete '{word1[i - 1]}' at position {i - 1}")
                i -= 1
            else:
                operations.append(f"Insert '{word2[j - 1]}' at position {i}")
                j -= 1
    
    operations.reverse()
    return dp[m][n], operations


# Example usage and test cases
if __name__ == "__main__":
    print("=" * 60)
    print("Edit Distance (Levenshtein Distance) - Test Cases")
    print("=" * 60)
    
    # Predefined test cases for demonstration
    test_cases = [
        ("kitten", "sitting", 3, "Classic example"),
        ("", "hello", 5, "Empty string to word"),
        ("same", "same", 0, "Identical strings"),
        ("abc", "xyz", 3, "Completely different"),
        ("saturday", "sunday", 3, "Common words"),
        ("AGGTAB", "GXTXAYB", 5, "DNA sequences"),
        ("intention", "execution", 5, "Long words"),
        ("horse", "ros", 3, "Different lengths"),
    ]
    
    print("\n📊 Running predefined test cases:\n")
    for i, (word1, word2, expected, description) in enumerate(test_cases, 1):
        distance = edit_distance(word1, word2)
        status = "✓ PASS" if distance == expected else "✗ FAIL"
        print(f"Test {i}: {description}")
        print(f"  '{word1}' → '{word2}'")
        print(f"  Result: {distance} | Expected: {expected} | {status}\n")
    
    # Example with operation details
    print("=" * 60)
    print("📝 Detailed Operation Tracking Example")
    print("=" * 60)
    word1, word2 = "saturday", "sunday"
    distance, operations = edit_distance_with_operations(word1, word2)
    print(f"\nTransforming '{word1}' → '{word2}'")
    print(f"Minimum operations needed: {distance}\n")
    print("Operations:")
    for i, op in enumerate(operations, 1):
        print(f"  {i}. {op}")
    
    # Optional: Allow user input
    print("\n" + "=" * 60)
    print("🔧 Custom Input (Optional)")
    print("=" * 60)
    user_input = input("\nWould you like to test custom strings? (y/n): ").strip().lower()
    
    if user_input == 'y':
        try:
            word1 = input("Enter first string: ").strip()
            word2 = input("Enter second string: ").strip()
            distance = edit_distance(word1, word2)
            print(f"\nEdit distance between '{word1}' and '{word2}': {distance}")
            
            show_ops = input("\nShow operations? (y/n): ").strip().lower()
            if show_ops == 'y':
                _, operations = edit_distance_with_operations(word1, word2)
                print("\nOperations:")
                for i, op in enumerate(operations, 1):
                    print(f"  {i}. {op}")
        except KeyboardInterrupt:
            print("\n\nExiting...")
    
    print("\n" + "=" * 60)
    print("✅ All tests completed!")
    print("=" * 60)