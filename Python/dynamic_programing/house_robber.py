def rob(nums):
    """
    Returns the maximum amount of money you can rob from houses
    without robbing two adjacent houses.
    Time Complexity: O(n)
    Space Complexity: O(n)
    """
    # Base case: if there are no houses, return 0
    if not nums:
        return 0
    
    # Base case: if there is only one house, rob it
    if len(nums) == 1:
        return nums[0]
    
    # Initialize DP table
    # dp[i] will store the maximum money that can be robbed up to house i
    dp = [0] * len(nums)
    
    # Maximum money robbing only the first house
    dp[0] = nums[0]
    # Maximum money robbing up to the second house (rob max of first or second)
    dp[1] = max(nums[0], nums[1])
    
    # Fill the DP table from house 2 to n-1
    for i in range(2, len(nums)):
        # Either rob current house + max from i-2, or skip current house (max from i-1)
        dp[i] = max(nums[i] + dp[i - 2], dp[i - 1])
    
    # The result is the maximum money that can be robbed from all houses
    return dp[-1]


# -------------------------------
# Test Cases
# -------------------------------
if __name__ == "__main__":
    print(rob([1, 2, 3, 1]))      # Expected: 4 (rob house 0 and 2)
    print(rob([2, 7, 9, 3, 1]))   # Expected: 12 (rob house 0, 2, and 4)
    print(rob([5, 3, 4, 11, 2]))  # Expected: 16 (rob house 0, 2, and 3)
    print(rob([1]))                # Expected: 1
    print(rob([2, 1, 1, 2]))      # Expected: 4