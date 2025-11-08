using System;

/// <summary>
/// Merge Sort using divide and conquer with auxiliary array.
/// </summary>
public static class MergeSort
{
    // Time: O(n log n), Space: O(n)
    public static void Sort(int[] arr)
    {
        if (arr == null || arr.Length <= 1) return;
        int[] aux = new int[arr.Length];
        SortRange(arr, aux, 0, arr.Length - 1);
    }

    private static void SortRange(int[] arr, int[] aux, int left, int right)
    {
        if (left >= right) return;
        int mid = left + ((right - left) / 2);
        SortRange(arr, aux, left, mid);
        SortRange(arr, aux, mid + 1, right);
        Merge(arr, aux, left, mid, right);
    }

    private static void Merge(int[] arr, int[] aux, int left, int mid, int right)
    {
        int i = left, j = mid + 1, k = left;
        while (i <= mid && j <= right)
        {
            if (arr[i] <= arr[j]) aux[k++] = arr[i++]; else aux[k++] = arr[j++];
        }
        while (i <= mid) aux[k++] = arr[i++];
        while (j <= right) aux[k++] = arr[j++];
        for (int t = left; t <= right; t++) arr[t] = aux[t];
    }

    public static void Main()
    {
        int[] arr = { 38, 27, 43, 3, 9, 82, 10 };
        Sort(arr);
        Console.WriteLine("[MergeSort] Sorted: " + string.Join(", ", arr));
    }
}


