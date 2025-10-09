// Problem: Two Sum (LeetCode #1)
// Given an array of integers nums and an integer target,
// return indices of the two numbers such that they add up to target.
//
// Time Complexity: O(n)
// Space Complexity: O(n)

use std::collections::HashMap;

pub fn two_sum(nums: Vec<i32>, target: i32) -> Option<(usize, usize)> {
    let mut map = HashMap::new();
    for (i, &num) in nums.iter().enumerate() {
        if let Some(&j) = map.get(&(target - num)) {
            return Some((j, i));
        }
        map.insert(num, i);
    }
    None
}
