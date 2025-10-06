// Given a triangle array, return the minimum path sum from top to bottom.

// For each step, you may move to an adjacent number of the row below. More formally, if you are on index i on the current row, you may move to either index i or index i + 1 on the next row.

// Example 1:
// Input: triangle = [[2],[3,4],[6,5,7],[4,1,8,3]]
// Output: 11
// Explanation: The triangle looks like:
//    2
//   3 4
//  6 5 7
// 4 1 8 3
// The minimum path sum from top to bottom is 2 + 3 + 5 + 1 = 11 (underlined above).

// Example 2:
// Input: triangle = [[-10]]
// Output: -10

// Constraints:
// 1 <= triangle.length <= 200
// triangle[0].length == 1
// triangle[i].length == triangle[i - 1].length + 1
// -104 <= triangle[i][j] <= 104

package main

import "fmt";

func minimumTotal(triangle [][]int) int {
	if len(triangle) == 0 {
		return  0
	}
	dp := make([]int, len(triangle))
	dp[0] = triangle[0][0]
	for i := 1; i < len(triangle); i++ {
		dp[i] = dp[i-1] + triangle[i][i]
		for j := i - 1; j > 0; j-- {
			if dp[j] < dp[j-1] {
				dp[j] = dp[j] + triangle[i][j]
			} else {
				dp[j] = dp[j-1] + triangle[i][j]
			}
		}
		dp[0] = dp[0] + triangle[i][0]
	}
	min := dp[0]
	for i := 1; i < len(dp); i++ {
		if dp[i] < min {
			min = dp[i]
		}
	}
	return min
}

func main() {
	triangle := [][]int{
		{2},
		{3, 4},
		{6, 5, 7},
		{4, 1, 8, 3},
	}
	fmt.Println(minimumTotal(triangle))
}