"""
Reverse Bits

Reverse bits of a given 32-bit signed integer.

Example 1:
    Input: n = 43261596
    Output: 964176192
    Explanation:
        Integer    Binary
        43261596   00000010100101000001111010011100
        964176192  00111001011110000010100101000000

Example 2:
    Input: n = 2147483644
    Output: 1073741822
    Explanation:
        Integer    Binary
        2147483644 01111111111111111111111111111100
        1073741822 00111111111111111111111111111110

Constraints:
- 0 <= n <= 2^31 - 2
- n is even
"""

class Solution(object):
    def reverseBits(self, n):
        """
        Reverse the bits of a 32-bit integer.
        """
        result = 0
        for i in range(32):
            # Extract the last bit of n
            bit = n & 1
            # Shift result to the left and add the bit
            result = (result << 1) | bit
            # Shift n to the right
            n >>= 1
        return result

# Example Usage
if __name__ == "__main__":
    test_cases = [43261596, 2147483644, 1, 0, 4294967294]
    
    obj = Solution()
    
    for n in test_cases:
        print("Input:", n)
        print("Reversed Bits:", obj.reverseBits(n))
        print("-" * 30)
