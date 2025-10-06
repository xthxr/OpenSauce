// Given an array nums of distinct integers, return all the possible permutations. You can return the answer in any order.

// Example 1:
// Input: nums = [1,2,3]
// Output: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

// Example 2:
// Input: nums = [0,1]
// Output: [[0,1],[1,0]]

// Example 3:
// Input: nums = [1]
// Output: [[1]]
 
// Constraints:
// 1 <= nums.length <= 6
// -10 <= nums[i] <= 10
// All the integers of nums are unique.

package main

import "fmt";

func permute(nums []int) [][]int {
	var result [][]int
	var backtrack func(start int)
	backtrack = func(start int) {
		if start == len(nums)-1 {
			perm := make([]int, len(nums))
			copy(perm, nums)
			result = append(result, perm)
			return
		}
		for i := start; i < len(nums); i++ {
			nums[start], nums[i] = nums[i], nums[start] // Swap
			backtrack(start + 1)
			nums[start], nums[i] = nums[i], nums[start] // Backtrack
		}
	}
	backtrack(0)

	return result
}

func main() {
	nums := []int{1, 2, 3}
	fmt.Println(permute(nums))
}