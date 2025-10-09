# -------------------------------
# Problem
# -------------------------------
"""
Name: Coin Change Problem (Optimal Space & Time)

Problem: Given an integer array coins representing coins of different denominations 
and an integer amount, return the fewest number of coins needed to make up that amount. 
If that amount cannot be made up by any combination of the coins, return -1.

Optimal Approach: Uses a single 1D DP array for space efficiency and bottom-up calculation.

Link: https://leetcode.com/problems/coin-change/
"""

# -------------------------------
# Solution
# -------------------------------
def coinChange(coins, amount):
    dp = [amount + 1] * (amount + 1)  # line 12
    dp[0] = 0 
    for i in range(1, amount + 1):  
        for coin in coins:  
            if coin <= i: 
                dp[i] = min(dp[i], dp[i - coin] + 1)  
    return dp[amount] if dp[amount] <= amount else -1  

# -------------------------------
# Test Cases
# -------------------------------
if __name__ == "__main__": 
    coins = [1, 2, 5] 
    amount = 11 
    print("Minimum coins needed:", coinChange(coins, amount)) # Output: 3 (5 + 5 + 1)



# -------------------------------
# Complexity Analysis:
# -------------------------------
"""
Time Complexity: O(n * amount)
    - n = number of coins, amount = target amount
    - Each amount is processed for every coin

Space Complexity: O(amount)
    - Only a 1D DP array of size amount + 1 is used
"""

# -------------------------------
# Explanation:
# -------------------------------
"""
We create a DP array of size amount + 1, where dp[i] represents the minimum number of coins needed to make amount i.
Initially, we set all values to amount + 1 (an impossible large number) — this acts as infinity, meaning “not yet possible.”
dp[0] = 0 because 0 coins are needed to make amount 0.
For each amount i (from 1 to amount):
Try every coin denomination.
If the coin value is ≤ i, then:
Check how many coins are needed to make up the remaining amount i - coin.
Add 1 to include the current coin.
Take the minimum among all coin choices.
Formula: dp[i]=min(dp[i],1+dp[i−coin])
This ensures that dp[i] always holds the smallest number of coins needed to form i.
After filling the DP table:
If dp[amount] is still greater than amount, it means it was never updated → no combination works → return -1.
Otherwise, return dp[amount] (the minimum coins needed).
"""
