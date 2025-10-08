// Given the root of a binary tree, check whether it is a mirror of itself (i.e., symmetric around its center).

// Example 1:
// Input: root = [1,2,2,3,4,4,3]
// Output: true

// Example 2:
// Input: root = [1,2,2,null,3,null,3]
// Output: false
 

// Constraints:
// The number of nodes in the tree is in the range [1, 1000].
// -100 <= Node.val <= 100

// https://leetcode.com/problems/symmetric-tree/description/?envType=problem-list-v2&envId=tree


/**
 * Definition for a binary tree node.
 * type TreeNode struct {
 *     Val int
 *     Left *TreeNode
 *     Right *TreeNode
 * }
 */
package main

func isSymmetric(root *TreeNode) bool {
	if root == nil {
		return true
	}
	
	q1 := []*TreeNode{root.Left}
	q2 := []*TreeNode{root.Right}

	for len(q1) > 0 && len(q2) > 0 {
		node1 := q1[0]
		node2 := q2[0]
		q1 = q1[1:]
		q2 = q2[1:]

		if node1 == nil && node2 == nil {
			continue
		}

		if node1 == nil || node2 == nil || node2.Val != node1.Val {
            return false
        }

		q1 = append(q1, node1.Left, node1.Right)
		q2 = append(q2, node2.Right, node2.Left)
	}

	return true
}