"""
Selection Sort

Sorts a list of numbers in ascending order using the Selection Sort algorithm.

Algorithm steps:
1. Iterate through the array and assume the current element is the minimum.
2. Compare it with the rest of the array to find the actual minimum.
3. Swap the found minimum with the current element.
4. Repeat until the array is fully sorted.

Example:
    Input: [64, 25, 12, 22, 11]
    Output: [11, 12, 22, 25, 64]

Time Complexity:
    Best, Average, Worst: O(n^2)
Space Complexity: O(1) (in-place sorting)
"""

class SelectionSort:
    @staticmethod
    def sort(arr):
        """
        Performs selection sort on the input list.
        """
        n = len(arr)
        for i in range(n):
            min_index = i
            for j in range(i + 1, n):
                if arr[j] < arr[min_index]:
                    min_index = j
            arr[i], arr[min_index] = arr[min_index], arr[i]
        return arr


# Example Usage
if __name__ == "__main__":
    test_cases = [
        [64, 25, 12, 22, 11],
        [5, 3, 8, 6, 2]
    ]

    for case in test_cases:
        print("Input Array:", case)
        print("Sorted Array:", SelectionSort.sort(case))
        print("-" * 30)
