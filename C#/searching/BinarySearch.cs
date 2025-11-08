using System;

/// <summary>
/// Binary Search on a sorted array. Returns the index of target or -1 if not found.
/// </summary>
public static class BinarySearch
{
    // Time: O(log n), Space: O(1)
    public static int Search(int[] sorted, int target)
    {
        int left = 0, right = sorted.Length - 1;
        while (left <= right)
        {
            int mid = left + ((right - left) / 2);
            if (sorted[mid] == target) return mid;
            if (sorted[mid] < target) left = mid + 1; else right = mid - 1;
        }
        return -1;
    }

    public static void Main()
    {
        int[] arr = { 1, 3, 5, 7, 9, 11 };
        Console.WriteLine("[BinarySearch] idx(7): " + Search(arr, 7));   // 3
        Console.WriteLine("[BinarySearch] idx(2): " + Search(arr, 2));   // -1
    }
}


