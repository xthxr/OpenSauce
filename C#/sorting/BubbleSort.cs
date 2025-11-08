using System;

/// <summary>
/// Bubble Sort implementation with early-exit optimization.
/// </summary>
public static class BubbleSort
{
    // Time: O(n^2) worst/avg, O(n) best; Space: O(1)
    public static void Sort(int[] arr)
    {
        bool swapped;
        for (int i = 0; i < arr.Length - 1; i++)
        {
            swapped = false;
            for (int j = 0; j < arr.Length - 1 - i; j++)
            {
                if (arr[j] > arr[j + 1])
                {
                    (arr[j], arr[j + 1]) = (arr[j + 1], arr[j]);
                    swapped = true;
                }
            }
            if (!swapped) break;
        }
    }

    public static void Main()
    {
        int[] arr = { 5, 1, 4, 2, 8 };
        Sort(arr);
        Console.WriteLine("[BubbleSort] Sorted: " + string.Join(", ", arr)); // 1, 2, 4, 5, 8
    }
}


