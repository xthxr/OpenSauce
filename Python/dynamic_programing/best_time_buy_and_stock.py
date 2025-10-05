from typing import List

def maxProfit(prices: List[int]) -> int:
    """
    Calculates the maximum profit from buying and selling a stock once.

    Args:
        prices (List[int]): List of stock prices where prices[i] is the price on day i.

    Returns:
        int: The maximum profit achievable. Returns 0 if no profit is possible.

    Time Complexity:
        O(n) - We iterate through the prices list once, where n is the number of days.

    Space Complexity:
        O(1) - Only two extra variables (min_price and max_profit) are used.
    """
    if not prices or len(prices) < 2:
        return 0

    min_price = float('inf')  # Minimum price seen so far
    max_profit = 0            # Maximum profit achieved

    for price in prices:
        min_price = min(min_price, price)        # Update min price
        max_profit = max(max_profit, price - min_price)  # Update max profit

    return max_profit


# -------------------------------
# Test cases
# -------------------------------
if __name__ == "__main__":
    # Test case 1: normal case
    prices = [7, 1, 5, 3, 6, 4]
    print(maxProfit(prices))  # Expected output: 5

    # Test case 2: prices decreasing (no profit possible)
    prices = [7, 6, 4, 3, 1]
    print(maxProfit(prices))  # Expected output: 0

    # Test case 3: empty list
    prices = []
    print(maxProfit(prices))  # Expected output: 0

    # Test case 4: single element
    prices = [5]
    print(maxProfit(prices))  # Expected output: 0
