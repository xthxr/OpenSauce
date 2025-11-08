using System;
using System.Collections.Generic;

/// <summary>
/// Two Sum: Given an array of integers and a target, return indices of the two numbers such that they add up to the target.
/// Demonstrates use of a hash map for O(n) time.
/// </summary>
public static class TwoSum
{
    // Time: O(n), Space: O(n)
    public static int[] FindTwoSum(int[] nums, int target)
    {
        var valueToIndex = new Dictionary<int, int>();
        for (int i = 0; i < nums.Length; i++)
        {
            int complement = target - nums[i];
            if (valueToIndex.TryGetValue(complement, out int j))
            {
                return new[] { j, i };
            }
            if (!valueToIndex.ContainsKey(nums[i]))
            {
                valueToIndex[nums[i]] = i;
            }
        }
        return Array.Empty<int>();
    }

    public static void Main()
    {
        int[] nums = { 2, 7, 11, 15 };
        int target = 9;
        int[] result = FindTwoSum(nums, target);
        Console.WriteLine("[TwoSum] Indices: " + string.Join(", ", result)); // Expected: 0, 1
    }
}


