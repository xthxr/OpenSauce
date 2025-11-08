using System;

/// <summary>
/// Rotates an array to the right by k steps using reversal algorithm.
/// </summary>
public static class ArrayRotation
{
    // Time: O(n), Space: O(1)
    public static void RotateRight(int[] arr, int k)
    {
        if (arr == null || arr.Length == 0)
        {
            return;
        }

        k %= arr.Length;
        if (k == 0)
        {
            return;
        }

        Reverse(arr, 0, arr.Length - 1);
        Reverse(arr, 0, k - 1);
        Reverse(arr, k, arr.Length - 1);
    }

    private static void Reverse(int[] arr, int left, int right)
    {
        while (left < right)
        {
            (arr[left], arr[right]) = (arr[right], arr[left]);
            left++;
            right--;
        }
    }

    public static void Main()
    {
        int[] arr = { 1, 2, 3, 4, 5, 6, 7 };
        RotateRight(arr, 3);
        Console.WriteLine("[ArrayRotation] Rotated: " + string.Join(", ", arr)); // Expected: 5, 6, 7, 1, 2, 3, 4
    }
}


