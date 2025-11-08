using System;

/// <summary>
/// Quick Sort (Lomuto partition). Average O(n log n), worst O(n^2).
/// </summary>
public static class QuickSort
{
    // Avg Time: O(n log n); Worst: O(n^2); Space: O(log n) recursion
    public static void Sort(int[] arr)
    {
        if (arr == null || arr.Length <= 1) return;
        SortRange(arr, 0, arr.Length - 1);
    }

    private static void SortRange(int[] arr, int left, int right)
    {
        if (left >= right) return;
        int p = Partition(arr, left, right);
        SortRange(arr, left, p - 1);
        SortRange(arr, p + 1, right);
    }

    private static int Partition(int[] arr, int left, int right)
    {
        int pivot = arr[right];
        int i = left;
        for (int j = left; j < right; j++)
        {
            if (arr[j] <= pivot)
            {
                (arr[i], arr[j]) = (arr[j], arr[i]);
                i++;
            }
        }
        (arr[i], arr[right]) = (arr[right], arr[i]);
        return i;
    }

    public static void Main()
    {
        int[] arr = { 10, 7, 8, 9, 1, 5 };
        Sort(arr);
        Console.WriteLine("[QuickSort] Sorted: " + string.Join(", ", arr));
    }
}


