"""
Radix Sort

Sorts a list of non-negative integers using Radix Sort algorithm.

Algorithm steps:
1. Find the maximum number to know the number of digits.
2. Sort numbers digit by digit using counting sort as a subroutine, starting from the least significant digit.
3. Repeat for each digit until the most significant digit is sorted.

Example:
    Input: [170, 45, 75, 90, 802, 24, 2, 66]
    Output: [2, 24, 45, 66, 75, 90, 170, 802]

Time Complexity: O(n * k) where k is the number of digits
Space Complexity: O(n + k)
"""

class RadixSort:
    @staticmethod
    def counting_sort(arr, exp):
        n = len(arr)
        output = [0] * n
        count = [0] * 10

        # Count occurrences of digits
        for i in range(n):
            index = (arr[i] // exp) % 10
            count[index] += 1

        # Update count to contain positions
        for i in range(1, 10):
            count[i] += count[i - 1]

        # Build output array
        i = n - 1
        while i >= 0:
            index = (arr[i] // exp) % 10
            output[count[index] - 1] = arr[i]
            count[index] -= 1
            i -= 1

        # Copy to original array
        for i in range(n):
            arr[i] = output[i]

    @staticmethod
    def sort(arr):
        if len(arr) == 0:
            return arr
        max_num = max(arr)
        exp = 1
        while max_num // exp > 0:
            RadixSort.counting_sort(arr, exp)
            exp *= 10
        return arr


# Example Usage
if __name__ == "__main__":
    test_cases = [
        [170, 45, 75, 90, 802, 24, 2, 66],
        [5, 123, 7, 56, 34]
    ]

    for case in test_cases:
        print("Input Array:", case)
        print("Sorted Array:", RadixSort.sort(case))
        print("-" * 30)
