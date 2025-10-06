// Given an integer array nums of unique elements, return all possible subsets (the power set).

// The solution set must not contain duplicate subsets. Return the solution in any order.

// Example 1:
// Input: nums = [1,2,3]
// Output: [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

// Example 2:
// Input: nums = [0]
// Output: [[],[0]]

// Constraints:
// 1 <= nums.length <= 10
// -10 <= nums[i] <= 10
// All the numbers of nums are unique.

package main

import "fmt"

func subsets(nums []int) [][]int {
	res := [][]int{{}}
	for _, num := range nums {
		n := len(res)
		for i := 0; i < n; i++ {
			cur := make([]int, len(res[i]))
			copy(cur, res[i])
			cur = append(cur, num)
			res = append(res, cur)

		}
	}
	return res
}

func main() {
	nums := []int{1, 2, 3}
	result := subsets(nums)
	fmt.Println(result)
}