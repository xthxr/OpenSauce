"""
Longest Subarray of 1's After Deleting One Element (Sliding Window Approach)

Time Complexity: O(n)
Space Complexity: O(1)

The sliding window algorithm dynamically adjusts the window boundaries
to ensure there is at most one '0' in the window. The window expands with `right`
and contracts with `left` when more than one zero is found.
"""

def longest_subarray(nums):
    """
    Finds the longest subarray of 1's after deleting one element using Sliding Window.

    Args:
        nums: List[int] - Binary array containing 0s and 1s.

    Returns:
        int: Length of the longest subarray containing only 1's after deleting one element.

    Example:
        >>> longest_subarray([1,1,0,1])
        3
        >>> longest_subarray([0,1,1,1,0,1,1,0,1])
        5
        >>> longest_subarray([1,1,1])
        2
    """
    left = 0
    zero_count = 0
    max_len = 0

    for right in range(len(nums)):
        # Include current element in the window
        if nums[right] == 0:
            zero_count += 1

        # If window has more than one zero, shrink it from the left
        while zero_count > 1:
            if nums[left] == 0:
                zero_count -= 1
            left += 1

        # Update max length (subtracting one element as per problem requirement)
        max_len = max(max_len, right - left)

    return max_len


if __name__ == "__main__":
    # Example usage
    print("Sliding Window Solution: Longest Subarray of 1's After Deleting One Element\n")

    # Testcase 1
    nums = [1, 1, 0, 1]
    print(f"Input: {nums}")
    print(f"Output: {longest_subarray(nums)}")  # Expected: 3

    # Testcase 2
    nums = [0, 1, 1, 1, 0, 1, 1, 0, 1]
    print(f"\nInput: {nums}")
    print(f"Output: {longest_subarray(nums)}")  # Expected: 5

    # Testcase 3
    nums = [1, 1, 1]
    print(f"\nInput: {nums}")
    print(f"Output: {longest_subarray(nums)}")  # Expected: 2

    # Testcase 4
    nums = [0, 0, 0]
    print(f"\nInput: {nums}")
    print(f"Output: {longest_subarray(nums)}")  # Expected: 0
