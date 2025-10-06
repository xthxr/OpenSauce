"""
Same Tree

Given the roots of two binary trees p and q, write a function to check if they are the same or not.
Two binary trees are considered the same if they are structurally identical, and the nodes have the same value.

Example 1:
    Input: p = [1,2,3], q = [1,2,3]
    Output: True

Example 2:
    Input: p = [1,2], q = [1,null,2]
    Output: False

Example 3:
    Input: p = [1,2,1], q = [1,1,2]
    Output: False

Constraints:
- The number of nodes in both trees is in the range [0, 100].
- -10^4 <= Node.val <= 10^4
"""

# Definition for a binary tree node.
class TreeNode(object):
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

class Solution(object):
    def isSameTree(self, p, q):
        """
        Recursively check if two binary trees are the same.
        """
        # Both nodes are None
        if not p and not q:
            return True
        # One of them is None
        if not p or not q:
            return False
        # Values differ
        if p.val != q.val:
            return False
        # Recursively check left and right subtrees
        return self.isSameTree(p.left, q.left) and self.isSameTree(p.right, q.right)

# Example Usage
if __name__ == "__main__":
    # Helper function to build tree from list
    def build_tree(lst):
        if not lst:
            return None
        nodes = [TreeNode(val) if val is not None else None for val in lst]
        kids = nodes[::-1]
        root = kids.pop()
        for node in nodes:
            if node:
                if kids: node.left = kids.pop()
                if kids: node.right = kids.pop()
        return root
    
    test_cases = [
        ([1,2,3], [1,2,3]),
        ([1,2], [1,None,2]),
        ([1,2,1], [1,1,2])
    ]
    
    obj = Solution()
    
    for p_list, q_list in test_cases:
        p_root = build_tree(p_list)
        q_root = build_tree(q_list)
        print("Tree p:", p_list)
        print("Tree q:", q_list)
        print("Are Same Tree:", obj.isSameTree(p_root, q_root))
        print("-" * 30)
