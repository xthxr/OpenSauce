using System;

/// <summary>
/// Linear Search on an array. Returns the index of target or -1 if not found.
/// </summary>
public static class LinearSearch
{
    // Time: O(n), Space: O(1)
    public static int Search(int[] arr, int target)
    {
        for (int i = 0; i < arr.Length; i++)
        {
            if (arr[i] == target) return i;
        }
        return -1;
    }

    public static void Main()
    {
        int[] arr = { 4, 2, 7, 1, 9 };
        Console.WriteLine("[LinearSearch] idx(7): " + Search(arr, 7)); // 2
        Console.WriteLine("[LinearSearch] idx(8): " + Search(arr, 8)); // -1
    }
}


