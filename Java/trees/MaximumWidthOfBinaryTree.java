
/**
 * Maximum Width of Binary Tree
 * Leetcode Problem no:#662
 * Time Complexity: O(N)
   Space Complexity: O(N)(worst case) O(w)(Average case)
 
     * Returns the maximum width of the binary tree.
     * Width is defined as the length between the end-nodes (the leftmost and rightmost non-null
     * nodes), where the null nodes between the end-nodes are also counted into the length.
     
**/
import java.util.LinkedList;
import java.util.Queue;

class TreeNode {
    int key;
    TreeNode left, right;

    public TreeNode(int item) {
        key = item;
        left = right = null;
    }
}

public class MaximumWidthOfBinaryTree {

    public int maximumWidth(TreeNode root) {
        // If root is null, width is zero
        if (root == null) {
            return 0;
        }

        // variable to store maximum width
        int ans = 0;

        // queue to store nodes for level order traversal
        Queue<TreeNode> q = new LinkedList<>();
        // queue to store index
        Queue<Integer> qIndex = new LinkedList<>();
        // push the root and index 0
        q.add(root);
        qIndex.add(0);
        while (!q.isEmpty()) {
            int size = q.size();
            // the minimum index of this level to be subtracted from each index to avoid
            // overflow
            int levelMin = qIndex.peek();
            int first = 0, last = 0;
            // process each node in the current level
            for (int i = 0; i < size; i++) {
                int curId = qIndex.remove() - levelMin;
                TreeNode node = q.remove();

                // if this is the first node in level update the 'first' variable
                if (i == 0) {
                    first = curId;
                }
                // If this is the last node in the level, update the 'last' variable
                if (i == size - 1) {
                    last = curId;
                }

                // if left node is not null add the left node in queue 'q' and index in qIndex
                if (node.left != null) {
                    q.add(node.left);
                    qIndex.add(curId * 2 + 1);
                }

                // if right node is not null add the right node in 'q' and index in qIndex
                if (node.right != null) {
                    q.add(node.right);
                    qIndex.add(curId * 2 + 2);
                }
            }

            // update ans as the width of this level
            ans = Math.max(ans, (int) (last - first + 1));
        }

        return ans;
    }

    public static void main(String args[]) {
        MaximumWidthOfBinaryTree mwbt = new MaximumWidthOfBinaryTree();
        // Test case 1 — Balanced tree
        TreeNode root1 = new TreeNode(3);
        root1.left = new TreeNode(5);
        root1.right = new TreeNode(1);
        root1.left.left = new TreeNode(6);
        root1.left.right = new TreeNode(2);
        root1.right.left = new TreeNode(0);
        root1.right.right = new TreeNode(8);
        root1.left.right.left = new TreeNode(7);
        root1.left.right.right = new TreeNode(4);
        System.out.println("Test 1 (Balanced tree) → " + mwbt.maximumWidth(root1)); // Expected: 4

        // Test case 2 — Complete binary tree
        TreeNode root2 = new TreeNode(1);
        root2.left = new TreeNode(2);
        root2.right = new TreeNode(3);
        root2.left.left = new TreeNode(4);
        root2.left.right = new TreeNode(5);
        root2.right.left = new TreeNode(6);
        root2.right.right = new TreeNode(7);
        System.out.println("Test 2 (Complete binary tree) → " + mwbt.maximumWidth(root2)); // Expected: 4

        // Test case 3 — Skewed tree (only left children)
        TreeNode root3 = new TreeNode(1);
        root3.left = new TreeNode(2);
        root3.left.left = new TreeNode(3);
        root3.left.left.left = new TreeNode(4);
        System.out.println("Test 3 (Skewed left tree) → " + mwbt.maximumWidth(root3)); // Expected: 1

        // Test case 4 — Sparse tree
        TreeNode root4 = new TreeNode(1);
        root4.left = new TreeNode(2);
        root4.right = new TreeNode(3);
        root4.left.right = new TreeNode(5);
        root4.right.right = new TreeNode(9);
        root4.right.right.left = new TreeNode(6);
        System.out.println("Test 4 (Sparse tree) → " + mwbt.maximumWidth(root4)); // Expected: 4
    }

}
