/// Two Sum problem implementation in Dart
/// Given an array of integers and a target sum, return indices of two numbers that add up to target
/// Time Complexity: O(n²) for brute force, O(n) for hash map approach
/// Space Complexity: O(1) for brute force, O(n) for hash map approach

import 'dart:collection';

/// Brute force approach - check all pairs
/// Time Complexity: O(n²), Space Complexity: O(1)
List<int>? twoSumBruteForce(List<int> nums, int target) {
  for (int i = 0; i < nums.length - 1; i++) {
    for (int j = i + 1; j < nums.length; j++) {
      if (nums[i] + nums[j] == target) {
        return [i, j];
      }
    }
  }
  return null;
}

/// Hash map approach using HashMap
/// Time Complexity: O(n), Space Complexity: O(n)
List<int>? twoSumHashMap(List<int> nums, int target) {
  HashMap<int, int> seen = HashMap<int, int>();
  
  for (int i = 0; i < nums.length; i++) {
    int complement = target - nums[i];
    if (seen.containsKey(complement)) {
      return [seen[complement]!, i];
    }
    seen[nums[i]] = i;
  }
  return null;
}

/// Find all pairs that sum to target
/// Time Complexity: O(n²), Space Complexity: O(n)
List<List<int>> twoSumAllPairs(List<int> nums, int target) {
  List<List<int>> result = [];
  
  for (int i = 0; i < nums.length - 1; i++) {
    for (int j = i + 1; j < nums.length; j++) {
      if (nums[i] + nums[j] == target) {
        result.add([i, j]);
      }
    }
  }
  return result;
}

/// Two sum with sorted array (two pointers approach)
/// Time Complexity: O(n), Space Complexity: O(1)
/// Assumes input array is sorted
List<int>? twoSumSorted(List<int> nums, int target) {
  int left = 0;
  int right = nums.length - 1;
  
  while (left < right) {
    int sum = nums[left] + nums[right];
    if (sum == target) {
      return [left, right];
    } else if (sum < target) {
      left++;
    } else {
      right--;
    }
  }
  return null;
}

void main() {
  List<int> nums1 = [2, 7, 11, 15];
  int target1 = 9;
  print('Two Sum Examples:');
  print('Array: $nums1, Target: $target1');
  print('Brute Force: ${twoSumBruteForce(nums1, target1)}');
  print('Hash Map: ${twoSumHashMap(nums1, target1)}');
  
  List<int> nums2 = [3, 2, 4];
  int target2 = 6;
  print('\nArray: $nums2, Target: $target2');
  print('Brute Force: ${twoSumBruteForce(nums2, target2)}');
  print('Hash Map: ${twoSumHashMap(nums2, target2)}');
  
  print('All pairs for target $target2: ${twoSumAllPairs(nums2, target2)}');
  
  List<int> sortedNums = [1, 2, 3, 4, 6];
  print('\nSorted array: $sortedNums, Target: 6');
  print('Two pointers: ${twoSumSorted(sortedNums, 6)}');
}